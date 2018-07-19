source("utils.R")

#Imports packages and installs if not found
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("data.table", "dplyr", "tidyr", "caret", "DMwR", 
              "naivebayes", "RColorBrewer", "corrplot","reldist", "MLmetrics", 
              "mlr", "xgboost", "onehot", "verification", "glmnet", "rpart")
ipak(packages)


#Data Processing===========================================================
train = as.data.frame(fread("data/train.csv", na.strings = c("-1", "-1.0")))
test = as.data.frame(fread("data/test.csv", na.strings = c("-1", "-1.0")))

train$id <- NULL
train$ps_car_03_cat <- NULL
train$ps_car_05_cat <- NULL
test$id <- NULL
test$ps_car_03_cat <- NULL
test$ps_car_05_cat <- NULL

na_cols_train = which(colSums(is.na(train)) >0)

#Imputing missing data in train
for(name in na_cols_train) {
  if(grepl("_cat", name)) {
    cat(name, "is a category \n")
    ids = which(is.na(train[, name]))
    # v = getmode(na.omit(train[, name]))
    # v = median(na.omit(train[, name]))
    train[ids, name] = -1
  } else {
    cat(name, "is a regular \n")
    ids = which(is.na(train[, name]))
    v = mean(train[, name], na.rm = TRUE)
    # v = median(train[, name], na.rm = TRUE)
    train[ids, name] = v
  }
}

#making sure that all categorical variables are factors
train <- train %>% mutate_at(vars(ends_with("_cat")), funs(factor))
train <- train %>% mutate_at(vars(ends_with("bin")), funs(factor))

#Data Partitioning===========================================================
set.seed(1)
splitIndex <- createDataPartition(train$target, p = .70, list = FALSE, times = 1)

cv_train = train[splitIndex, ]
cv_test = train[-splitIndex, ]

#SMOTE data balancing
cv_train$target <- as.factor(cv_train$target)
cv_train <- SMOTE(target ~ ., cv_train, perc.over = 100, perc.under=200)
table(cv_train$target)
prop.table(table(cv_train$target))

#Naive Bayes=======================================================
# cv_train$target = as.factor(cv_train$target)
naive_fit = naive_bayes(target ~ ., data = cv_train)
test_pred_prob = predict(naive_fit, cv_test[,-1], type = "prob")

smote_nb[3] = Gini(test_pred_prob[,2], cv_test$target)
# 
# id = which(test_pred_prob[,1] > test_pred_prob[,2])
# pred_target_train = rep(1, nrow(test_pred_prob))
# pred_target_train[id] = 0
# table(cv_test$target, pred_target_train)
# 
# ginis_nb = c(G_case1, G_case2, G_case3)

#GLM=======================================================
lin_comb <- findLinearCombos(cv_train)
cv_train = cv_train[,-lin_comb$remove]
cv_test = cv_test[,-lin_comb$remove]
logmod <- glm(target ~ ., data = cv_train, family = binomial(link = 'logit'))
preds_glm <- predict(logmod, newdata = cv_test[,-1], type = "response")
smote_glm[3] = Gini(preds_glm, cv_test$target)

#XGBoost=======================================================
cv_train$target = as.numeric(cv_train$target)
cv_train$target[cv_train$target == 1] = 0
cv_train$target[cv_train$target == 2] = 1

cv_train = as.data.frame(predict(onehot(cv_train, max_levels = 200), cv_train))
cv_test = as.data.frame(predict(onehot(cv_test, max_levels = 200), cv_test))

dgctrain_cv <- Matrix(as.matrix(cv_train[,setdiff(names(cv_train), "target")]), sparse = TRUE)
dgctest_cv <- Matrix(as.matrix(cv_test[,setdiff(names(cv_train), "target")]), sparse = TRUE)
dtrain_cv <- xgb.DMatrix(data = dgctrain_cv, label = cv_train$target)
dtest_cv <- xgb.DMatrix(data = dgctest_cv, label = cv_test$target)

param <- list("booster" = "gbtree", "objective" = "binary:logistic",
              "eta" = 0.04,
              "min_child_weight" = 4.9,
              "subsample" = .7,
              "colsample_bytree" = .7,
              "max_depth" = c(6))

num_rounds <- 500
watchlist <- list(val=dtest_cv, train=dtrain_cv)
bst5 <- xgb.train(params = param, data = dtrain_cv, feval = evalgini, 
                  nround=num_rounds, print_every_n = 10, watchlist=watchlist, 
                  maximize = TRUE, verbose = 1)

#predicting cv_test data
pred_xg <- predict(bst5, dtest_cv)
Gini(pred_xg, as.numeric(cv_test$target))

#Decision Tree=======================================================
model_DT <- rpart(target ~ ., data = cv_train
                  ,method = "class"
                  ,control = rpart.control(maxdepth = 30
                                           ,xval = 5
                                           ,cp = 10^(-10))
)
pred_DT <- predict(model_DT, cv_test,type = 'prob')#prob
smote_dt[4] = Gini(pred_DT[,2], cv_test$target)

#Plotting results from different models=======================================================
plot(ginis_nb, type = "b", col = "red", ylim = c(0.22, 0.32))
lines(ginis_glm, type = "b", col = "blue")
lines(ginis_xg, type = "b", col = "green")
legend("topright", legend = c("Naive Bayes","GLM", "XGBoost"), lty = c(1,1), lwd=c(2.5,2.5,2.5),col=c("red","blue","green"))

par(mfrow = c(2,2))
plot(ginis_nb, xaxt = "n", ylim = c(0.216, 0.235), type = "b", pch = 20, col = "red", ylab = "Gini Score", xlab = "Trials", main = "Naive Bayes")
lines(smote_nb, type = "b", pch = 20, col = "blue")
axis(1, at=1:4, labels=c("case_1","case_2","case_3","case_4"))
legend("topleft", legend = c("Imbalanced","SMOTE"), lty = c(1,1), lwd=c(2.5,2.5),col=c("red","blue"))

plot(ginis_glm, xaxt = "n", ylim = c(0.231, 0.259), type = "b", pch = 20, col = "red", ylab = "Gini Score", xlab = "Trials", main = "Logistic Regression")
lines(smote_glm, type = "b", pch = 20, col = "blue")
axis(1, at=1:4, labels=c("case_1","case_2","case_3","case_4"))
legend("topleft", legend = c("Imbalanced","SMOTE"), lty = c(1,1), lwd=c(2.5,2.5),col=c("red","blue"))

plot(ginis_xg, xaxt = "n", ylim = c(0.268, 0.28), type = "b", pch = 20, col = "red", ylab = "Gini Score", xlab = "Trials", main = "XGBoost")
lines(smote_xg, type = "b", pch = 20, col = "blue")
axis(1, at=1:4, labels=c("case_1","case_2","case_3","case_4"))
legend("topleft", legend = c("Imbalanced","SMOTE"), lty = c(1,1), lwd=c(2.5,2.5),col=c("red","blue"))


plot(ginis_glm, xaxt = "n", type = "b", pch = 20, col = "blue", ylab = "Gini Score", xlab = "Trials", main = "Logistic Regression")
axis(1, at=1:4, labels=c("case_1","case_2","case_3","case_4"))
plot(ginis_xg, xaxt = "n", type = "b", pch = 20, col = "chartreuse4", ylab = "Gini Score", xlab = "Trials", main = "XGBoost")
axis(1, at=1:4, labels=c("case_1","case_2","case_3","case_4"))

#=======================================================
optimalCutoff <- function(groundTruth, prob){
  require(ggplot2)
  df <- data.frame(cutoff = numeric()
                   ,Precision = numeric()
                   ,Recall = numeric()
                   ,F1 = numeric())
  for(cutoff in seq(0,1,0.05)){
    cm <- confusionMatrix(reference = groundTruth
                          ,data = ifelse(prob>cutoff,1,0)
                          ,positive = '1')
    df <- rbind(df,data.frame(cutoff = cutoff
                              ,Precision = cm$byClass[5]
                              ,Recall = cm$byClass[6] 
                              ,F1 = cm$byClass[7]))
    rownames(df) <- 1:nrow(df)
  }
  df <- na.omit(df)
  if(nrow(df) > 0){
    P <- ggplot(data = df) + 
      geom_line(aes(x = cutoff, y = Recall), col = 'red') + 
      geom_line(aes(x = cutoff, y = Precision), col = 'blue') + 
      geom_line(aes(x = cutoff, y = F1), col = 'orange') +
      ylab('Recall -- Precision -- F1')
    return(list(P,df))
  }
  return(list(df))
}

aa = optimalCutoff(cv_test$target, preds_glm)

bb = optimalCutoff(cv_test$target, pred_xg)
id = which(pred_xg > 0.45)
pred_target_train = rep(0, length(pred_xg))
pred_target_train[id] = 1
table(cv_test$target, pred_target_train)

require('e1071')
#### Naive Bayes ####
K <- 5 #5 fold CV
idx <- caret::createFolds(y = trn_cln[,"target"]
                               ,k = K
                               ,list = F)
gini_cv_xg = c()
# trn_cln$target = as.factor(trn_cln$target)
trn_cln = as.data.frame(predict(onehot(trn_cln, max_levels = 200), trn_cln))

dgctrain <- Matrix(as.matrix(trn_cln[,setdiff(names(trn_cln), "target")]), sparse = TRUE)
dtrain <- xgb.DMatrix(data = dgctrain, label = trn_cln$target)
for(k in 1:K){
  dtrain_cv <- dtrain[as.integer(which(idx!=k)),]
  dtest_cv <- dtrain[as.integer(which(idx==k)),]
  dtarget_cv <- trn_cln[idx==k, "target"]
  param <- list("booster" = "gbtree", "objective" = "binary:logistic",
                "eta" = 0.04,
                "min_child_weight" = 4.9,
                "subsample" = .7,
                "colsample_bytree" = .7,
                "max_depth" = c(6))
  
  num_rounds <- 300
  
  watchlist <- list(val=dtest_cv, train=dtrain_cv)
  
  bst5 <- xgb.train(params = param, data = dtrain_cv, feval = evalgini, 
                    nround=num_rounds, print_every_n = 10, watchlist=watchlist, 
                    maximize = TRUE, verbose = 1)
  
  #predicting cv_test data
  pred_xg <- predict(bst5, dtest_cv)
  gini_cv_xg[k] = Gini(pred_xg, dtarget_cv)
  
  # logmod <- glm(target ~ ., data = trn_cln[idx!=k,], family = binomial(link = 'logit'))
  # preds_glm <- predict(logmod, newdata = trn_cln[idx==k,setdiff(names(trn_cln), "target")], type = "response")
  # gini_cv_glm[k] = Gini(preds_glm, as.numeric(trn_cln[idx==k,"target"]))
  
  # naive_fit = naive_bayes(target ~ ., data = trn_cln[idx!=k,])
  # test_pred_prob = predict(naive_fit, trn_cln[idx==k,setdiff(names(trn_cln), "target")], type = "prob")
  # gini_cv_nb[k] = Gini(test_pred_prob[,2], as.numeric(trn_cln[idx==k,"target"]))
}

grp = rep(c("Naive Bayes", "Logistic", "XGBoost"), each = 5)
val = c(gini_cv_nb, gini_cv_glm, gini_cv_xg)
eval = data.frame("GiniScore" = val, "Model" = grp)
eval$Model = factor(eval$Model, levels = c("Naive Bayes", "Logistic", "XGBoost"))
bp <- ggplot(eval, aes(x=Model, y=GiniScore, fill = Model)) +
  geom_boxplot()
bp

eval = data.frame("gini_cv_nb"=gini_cv_nb,"gini_cv_glm"=gini_cv_glm,"gini_cv_xg"=gini_cv_xg)
ggplot(data = eval) + 
  geom_boxplot(width = .25, aes(y = gini_cv_nb, x = 1), fill = 'red') +
  geom_boxplot(width = .25, aes(y = gini_cv_glm, x = 2), fill = 'blue') +
  geom_boxplot(width = .25, aes(y = gini_cv_xg, x = 3), fill = 'chartreuse4') +
  xlab('Precision(red) -- Recall(blue) -- F1(brown)') + ylab('Gini Score') +
  ggtitle('Comparing Models')
