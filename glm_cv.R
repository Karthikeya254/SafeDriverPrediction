library(data.table)
library(dplyr)
library(tidyr)
library(caret)
library(DMwR)
library(naivebayes)
library(RColorBrewer)
library(corrplot)
library(reldist)
library(MLmetrics)
library(mlr)
library(xgboost)
library(onehot)
library(verification)
library(glmnet)

train = as.data.frame(fread("data/train.csv", na.strings = c("-1", "-1.0")))
test = as.data.frame(fread("data/test.csv", na.strings = c("-1", "-1.0")))

################################################################
getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

trn_cln = train
test_cln = test

trn_cln$ps_car_03_cat <- NULL
trn_cln$ps_car_05_cat <- NULL
test_cln$ps_car_03_cat <- NULL
test_cln$ps_car_05_cat <- NULL

na_cols_train = which(colSums(is.na(trn_cln)) >0)
na_cols_test = which(colSums(is.na(test_cln)) >0)

for(i in 1:length(na_cols_train)) {
  name = names(na_cols_train)[i]
  value = na_cols_train[i]
  cat(name, "\n")
  if(grepl("_cat", name)) {
    cat(na_cols_train[i], "is a category \n")
    ids = which(is.na(trn_cln[, value]))
    # v = getmode(na.omit(trn_cln[, value]))
    # v = median(na.omit(trn_cln[, value]))
    trn_cln[ids, value] = -1
  } else {
    cat(na_cols_train[i], "is a regular \n")
    ids = which(is.na(trn_cln[, value]))
    v = mean(trn_cln[, value], na.rm = TRUE)
    # v = median(trn_cln[, value], na.rm = TRUE)
    trn_cln[ids, value] = v
  }
}

for(i in 1:length(na_cols_test)) {
  name = names(na_cols_test)[i]
  value = na_cols_test[i]
  cat(name, "\n")
  if(grepl("_cat", name)) {
    cat(na_cols_test[i], "is a category \n")
    ids = which(is.na(test_cln[, value]))
    # v = getmode(na.omit(test_cln[, value]))
    # v = median(na.omit(test_cln[, value]))
    test_cln[ids, value] = -1
  } else {
    cat(na_cols_test[i], "is a regular \n")
    ids = which(is.na(test_cln[, value]))
    v = mean(test_cln[, value], na.rm = TRUE)
    # v = median(test_cln[, value], na.rm = TRUE)
    test_cln[ids, value] = v
  }
}

trn_cln$id <- NULL
test_cln$id <- NULL

tmp_trn = trn_cln
tmp_test = test_cln

trn_cln <- trn_cln %>%
  mutate_at(vars(ends_with("_cat")), funs(factor))

test_cln <- test_cln %>%
  mutate_at(vars(ends_with("_cat")), funs(factor))

train_oh = predict(onehot(trn_cln, max_levels = 200), trn_cln)
test_oh = predict(onehot(test_cln, max_levels = 200), test_cln)

trn_cln = as.data.frame(train_oh)
test_cln = as.data.frame(test_oh)

############################################################

set.seed(1)
splitIndex <- createDataPartition(trn_cln$target, p = .70, list = FALSE, times = 1)

cv_train = trn_cln[splitIndex, ]
cv_test = trn_cln[-splitIndex, ]
prop.table(table(cv_train$target))
prop.table(table(cv_test$target))

lin_comb <- findLinearCombos(cv_train)
d <- setdiff(seq(1:ncol(cv_train)), lin_comb$remove)
cv_train = cv_train[, d]
cv_train = cv_train[, setdiff(names(cv_train), "ps_ind_02_cat=4")]
cv_test = cv_test[, d]
cv_test = cv_test[, setdiff(names(cv_test), "ps_ind_02_cat=4")]

#==========================================
#GLM model
logmod <- glm(target ~ ., data = cv_train, family = binomial(link = 'logit'))
preds_cat_na_allcol_lmna <- predict(logmod, newdata = cv_test[,-1], type = "response")
Gini(preds_cat_na_allcol_lmna, cv_test$target)

roc.plot(
  cv_test$target, 
  preds_cat_na_allcol_lmna, 
  threshold = seq(0, max(preds_cat_na_allcol_lmna), 0.01), 
  plot.thres = c(0.03, 0.05, 0.1))

#predicting probabilities over test data
preds_test <- predict(logmod, newdata = test_cln, type = "response")

#==========================================
#GLM CV model
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
dim_names = setdiff(names(trn_cln[,d]), "target")
test_cln = test_cln[, dim_names]
test_cln = test_cln[, setdiff(names(test_cln), "ps_ind_02_cat=4")]

preds_test_cv = predict(fit.cv, newx = data.matrix(test_cln), s="lambda.min", type = "response")
f = cbind(test[,1], preds_test_cv)
write.csv(f, file = "sub.csv", row.names = FALSE)
