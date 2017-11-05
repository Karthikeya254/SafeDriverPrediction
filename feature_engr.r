library(data.table)
library(dplyr)
library(tidyr)
library(caret)
library(DMwR)
library(naivebayes)
library(RColorBrewer)
library(corrplot)

train = as.data.frame(fread("data/train.csv", na.strings = c("-1", "-1.0")))
test = as.data.frame(fread("data/test.csv", na.strings = c("-1", "-1.0")))

na_summary <- function(x) {
  count_na = length(which(is.na(x)))
  perc = round(100*(length(which(is.na(x))))/length(x), digits = 2)
  return(list(count_na, perc))
}

predictNAfromLM <- function(x, ind_var, dep_var) {
  data_ind_dep = subset(x, select = c(ind_var, dep_var))
  na_ids_ind = which(is.na(data_ind_dep[, ind_var]))
  na_ids_dep = which(is.na(data_ind_dep[, dep_var]))
  total_na_ids = union(na_ids_ind, na_ids_dep)
  predictable_ids = setdiff(na_ids_dep, na_ids_ind)
  
  if(length(na_ids_ind) >= length(na_ids_dep)) {
    cat("not enough predictor observations...\n")
  }
  
  lm.fit = lm(formula(paste(dep_var,"~", ind_var)), data = data_ind_dep[-total_na_ids,])
  pred_dep_values = predict(lm.fit, data_ind_dep[predictable_ids,], type = "response")
  return(list(ids = predictable_ids, values = pred_dep_values))
}

apply(train, 2, function(x)
  na_summary(x)) %>% unlist() %>%
  matrix(byrow = T, ncol = 2) %>%
  as.data.frame(row.names = names(train)) %>%
  `names<-`(c('count_na', 'Percent_na')) 

apply(train %>% select(contains("_ind")), 2, function(x)
  na_summary(x)) %>% unlist() %>%
  matrix(byrow = T, ncol = 2) %>%
  as.data.frame(row.names = names(train %>% select(contains("_ind")))) %>%
  `names<-`(c('count_na', 'Percent_na')) 

corrplot(cor(na.omit(train %>% select(contains("_ind"), target))), 
         type="lower", method="color",
         col=brewer.pal(n=8, name="RdBu"),diag=FALSE)


pred_na_reg_03 = predictNAfromLM(train, "ps_reg_02", "ps_reg_03")
train$ps_reg_03[pred_na_reg_03$ids] = pred_na_reg_03$values
plot(train$ps_reg_02, train$ps_reg_03)
lines(train$ps_reg_02[pred_na_reg_03$ids], pred_na_reg_03$values, col = "red")

pred_na_car_14 = predictNAfromLM(train, "ps_car_12", "ps_car_14")
train$ps_car_14[pred_na_car_14$ids] = pred_na_car_14$values
plot(train$ps_car_12, train$ps_car_14)
lines(train$ps_car_12[pred_na_car_14$ids], pred_na_car_14$values, col = "red")

################################################################
# Test
apply(test, 2, function(x)
  na_summary(x)) %>% unlist() %>%
  matrix(byrow = T, ncol = 2) %>%
  as.data.frame(row.names = names(test)) %>%
  `names<-`(c('count_na', 'Percent_na')) 

apply(test %>% select(contains("_car")), 2, function(x)
  na_summary(x)) %>% unlist() %>%
  matrix(byrow = T, ncol = 2) %>%
  as.data.frame(row.names = names(test %>% select(contains("_car")))) %>%
  `names<-`(c('count_na', 'Percent_na')) 

corrplot(cor(na.omit(test %>% select(contains("_car")))), 
         type="lower", method="color",
         col=brewer.pal(n=8, name="RdBu"),diag=FALSE)

pred_na_reg_03 = predictNAfromLM(test, "ps_reg_02", "ps_reg_03")
test$ps_reg_03[pred_na_reg_03$ids] = pred_na_reg_03$values

pred_na_car_14 = predictNAfromLM(test, "ps_car_12", "ps_car_14")
test$ps_car_14[pred_na_car_14$ids] = pred_na_car_14$values

################################################################
################################################################
getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

train <- train %>% 
  mutate_at(vars(ends_with("_cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical)) %>%
  mutate(target = as.factor(target))

test <- test %>%
  mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical))

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
    v = getmode(na.omit(trn_cln[, value]))
    trn_cln[ids, value] = v
  } else {
    cat(na_cols_train[i], "is a regular \n")
    ids = which(is.na(trn_cln[, value]))
    v = mean(trn_cln[, value], na.rm = TRUE)
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
    v = getmode(na.omit(test_cln[, value]))
    test_cln[ids, value] = v
  } else {
    cat(na_cols_test[i], "is a regular \n")
    ids = which(is.na(test_cln[, value]))
    v = mean(test_cln[, value], na.rm = TRUE)
    test_cln[ids, value] = v
  }
}

trn_cln$id <- NULL
test_cln$id <- NULL

set.seed(1)
splitIndex <- createDataPartition(trn_cln$target, p = .70, list = FALSE, times = 1)

cv_train = trn_cln[splitIndex, ]
cv_test = trn_cln[-splitIndex, ]
prop.table(table(cv_train$target))
prop.table(table(cv_test$target))

#not using currently
cv_train$target <- as.factor(cv_train$target)
cv_train <- SMOTE(target ~ ., cv_train, perc.over = 100, perc.under=200)
cv_train$target <- as.numeric(cv_train$target)

#===============

cv_train$target = as.factor(cv_train$target)
naive_fit = naive_bayes(target ~ ., data = cv_train)
cv_test$target = as.factor(cv_test$target)

test_pred_prob = predict(naive_fit, cv_test[,-1], type = "prob")

id = which(test_pred_prob[,1] > test_pred_prob[,2])
pred_target_train = rep(1, nrow(test_pred_prob))
pred_target_train[id] = 0
table(cv_test$target, pred_target_train)

pred_test = predict(naive_fit, test_cln, type = "prob")

f = cbind(test[,1], round(pred_test[,2], 4))
write.csv(f, file = "sub.csv")