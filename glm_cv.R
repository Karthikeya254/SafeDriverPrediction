source("utils.R")

#Imports packages and installs if not found
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("data.table", "dplyr", "tidyr", "caret", "DMwR", 
              "naivebayes", "RColorBrewer", "corrplot","reldist", 
              "MLmetrics", "mlr", "xgboost", "onehot", "verification", "glmnet")
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
na_cols_test = which(colSums(is.na(test)) >0)

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

#Imputing missing data in test
for(name in na_cols_test) {
  if(grepl("_cat", name)) {
    cat(name, "is a category \n")
    ids = which(is.na(test[, name]))
    # v = getmode(na.omit(test[, name]))
    # v = median(na.omit(test[, name]))
    test[ids, name] = -1
  } else {
    cat(name, "is a regular \n")
    ids = which(is.na(test[, name]))
    v = mean(test[, name], na.rm = TRUE)
    # v = median(test[, name], na.rm = TRUE)
    test[ids, name] = v
  }
}

#making sure that all categorical variables are factors
train <- train %>% mutate_at(vars(ends_with("_cat")), funs(factor))
test <- test %>% mutate_at(vars(ends_with("_cat")), funs(factor))

#Converting factors to onehot
train = as.data.frame(predict(onehot(train, max_levels = 200), train))
test = as.data.frame(predict(onehot(test, max_levels = 200), test))


#Data Partitioning===========================================================
set.seed(1)
splitIndex <- createDataPartition(train$target, p = .70, list = FALSE, times = 1)

cv_train = train[splitIndex, ]
cv_test = train[-splitIndex, ]
prop.table(table(cv_train$target))
prop.table(table(cv_test$target))

#Finding linear dependencies and removing them
lin_comb <- findLinearCombos(cv_train)
d <- setdiff(seq(1:ncol(cv_train)), lin_comb$remove)
cv_train = cv_train[, d]
cv_train = cv_train[, setdiff(names(cv_train), "ps_ind_02_cat=4")]
cv_test = cv_test[, d]
cv_test = cv_test[, setdiff(names(cv_test), "ps_ind_02_cat=4")]


#GLM model===========================================================
logmod <- glm(target ~ ., data = cv_train, family = binomial(link = 'logit'))
preds_cat_na_allcol_lmna <- predict(logmod, newdata = cv_test[,-1], type = "response")
Gini(preds_cat_na_allcol_lmna, cv_test$target)

roc.plot(
  cv_test$target, 
  preds_cat_na_allcol_lmna, 
  threshold = seq(0, max(preds_cat_na_allcol_lmna), 0.01), 
  plot.thres = c(0.03, 0.05, 0.1))

#predicting probabilities over test data
preds_test <- predict(logmod, newdata = test, type = "response")


#GLM CV model===========================================================
fit.cv <- cv.glmnet(y = as.matrix(cv_train$target), 
                    x = as.matrix(cv_train[,setdiff(names(cv_train), "target")]), family="binomial", type.measure = "auc")
cv_test_temp = cv_test
cv_test$target <- NULL
ts = data.matrix(cv_test)
p1 = predict(fit.cv, newx = ts, type = "response")
p2 = predict(fit.cv, newx = ts, s="lambda.min", type = "response")
Gini(p1, cv_test_temp$target)
Gini(p2, cv_test_temp$target)

#predicting probabilities over test data
dim_names = setdiff(names(train[,d]), "target")
test = test[, dim_names]
test = test[, setdiff(names(test), "ps_ind_02_cat=4")]

preds_test_cv = predict(fit.cv, newx = data.matrix(test), s="lambda.min", type = "response")
f = cbind(test[,1], preds_test_cv)
write.csv(f, file = "sub.csv", row.names = FALSE)
