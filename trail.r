library(data.table)
library(dplyr)
library(tidyr)
library(caret)
library(DMwR)
library(naivebayes)

train = as.data.frame(fread("data/train.csv", na.strings = c("-1", "-1.0")))
test = as.data.frame(fread("data/test.csv", na.strings = c("-1", "-1.0")))

train <- train %>% 
  mutate_at(vars(ends_with("_cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical)) %>%
  mutate(target = as.factor(target))

test <- test %>%
  mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical))

trn_cln = train %>% select(everything(), -which(colSums(is.na(.))>10000))
test_cln = test %>% select(everything(), -which(colSums(is.na(.))>10000))

na_cols = which(colSums(is.na(trn_cln)) >0)
na_cols_test = which(colSums(is.na(test_cln)) >0)

for(i in 1:length(na_cols)) {
  name = names(na_cols)[i]
  value = na_cols[i]
  cat(name, "\n")
  if(grepl("_cat", name)) {
    cat(na_cols[i], "is a category \n")
    ids = which(is.na(trn_cln[, value]))
    v = getmode(na.omit(trn_cln[, value]))
    trn_cln[ids, value] = v
  } else {
    cat(na_cols[i], "is a regular \n")
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

cv_train$target <- as.factor(cv_train$target)
cv_train <- SMOTE(target ~ ., cv_train, perc.over = 100, perc.under=200)
cv_train$target <- as.numeric(cv_train$target)

X = as.matrix(cbind(1, cv_train[,-1]))
test_X = as.matrix(cbind(1, cv_test[,-1]))
Y = as.matrix(cv_train[,1])
test_Y = as.matrix(cv_test[,1])
alpha = 0.01
num.iters = 1000
theta = matrix(0,ncol(X),1)
log_reg = gradientDescentLogReg(X, Y, theta, alpha, num.iters)
theta = log_reg$theta
J_list = log_reg$J_list
plot(J_list)


h = sigmoid(X %*% theta)
h_test = sigmoid(test_X %*% theta)

pred_target = c()
for(i in 1:length(h_test)) {
  if(h_test[i] > 0.5) {
    pred_target[i] = 1
  } else {
    pred_target[i] = 0
  }
}

f = cbind(test[,1], h_test)
write.csv(f, file = "sub.csv")

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

##################################################################



trn_cln_names = colnames(train)[colSums(is.na(train)) > 0]

which(names(trn_cln) == trn_cln_names)

which(contains(names(trn_cln), "_cat"))
s = "ps_cat"






train %>% select(everything(), -matches('_cat|_bin')) %>% summary()

train %>% summarise_each(funs(sum(!is.na(.))))

tna_names_4 = colnames(train)[colSums(is.na(train)) > 10000]
id = which(names(train) %in% tna_names_4)

t = train[, -id]
t <- train %>% select(everything(), -tna_names)

train_na = subset(train, select = - quote(tna_names))

count_na = function(x) sum(is.na(x))
s <- train %>% select(-one_of(tna_names_4))
trn_cln = train %>% select(everything(), -which(colSums(is.na(.))>10000))
test %>% select(which(colSums(is.na(.))>10000))



test %>% select(which(colSums(is.na(.))>0)) %>% summary()

train %>% select(tna_names) %>% summary()

apply(train, 2, function(x) length(which(is.na(x))))
apply(test, 2, function(x) length(which(is.na(x))))

temp = train %>% select(contains(c("_cat", "-bin")))
apply(temp, 2, function(x) length(which(x == -1)))

train[train == -1] = NA
test[test == -1] = NA

#ps_reg_03, ps_car_03_cat, ps_car_05_cat, ps_car_14
#removing these variables as they have large number of missing values
train$ps_reg_03 <- NULL
train$ps_car_03_cat <- NULL
train$ps_car_05_cat <- NULL
train$ps_car_14 <- NULL

#creating NAs in glm
train$ps_ind_09_bin <- NULL
train$ps_ind_14 <- NULL

test$ps_reg_03 <- NULL
test$ps_car_03_cat <- NULL
test$ps_car_05_cat <- NULL
test$ps_car_14 <- NULL
test$ps_ind_09_bin <- NULL
test$ps_ind_14 <- NULL

train = na.omit(train)
t = subset(train, select = -c(ps_reg_03, ps_car_03_cat,
                                 ps_car_05_cat, ps_car_14))
d = train[,-1]
e = test[,-1]

glm.fit = glm(target ~ ., data = d)

tg = predict(glm.fit, e, type = "response")


X = as.matrix(cbind(1, d[,-1]))
eX = as.matrix(cbind(1, e))
Y = as.matrix(d[,1])++
alpha = 0.01
num.iters = 100
theta = matrix(0,ncol(X),1)
log_reg = gradientDescentLogReg(X, Y, theta, alpha, num.iters)
theta = log_reg$theta
J_list = log_reg$J_list
plot(J_list)


h = sigmoid(X %*% theta)
h_test = sigmoid(eX %*% theta)


f = cbind(test[,1], h_test)
write.csv(f, file = "sub.csv")
pred_train = predict(glm.fit, dd
                     , type = "response")

#=====================================================
sigmoid <- function(z) {
  g = 1/(1+exp(-z))
  return(g)
}

costFunctionLogReg <- function(x, y, theta) {
  z = x%*%theta
  h = sigmoid(z)
  z1 = as.numeric(-(t(y) %*% log(h)))
  z2 = as.numeric(-(t(1-y) %*% log(1-h)))
  J = (1/length(y))*(z1 + z2)
  return(J)
}

gradientDescentLogReg <- function(x, y, theta, alpha, num.iters) {
  J_list = c()
  theta_list = c()
  for(i in 1:num.iters) {
    temp = matrix(0,ncol(x),1)
    z = x%*%theta
    h = sigmoid(z)
    for(j in 1:length(theta)) {
      temp[j] = theta[j] - (alpha/nrow(x))*t(h-y)%*%as.matrix(x[,j])
    }
    
    J_list[i] = costFunctionLogReg(x, y, temp)
    theta = temp
  }
  return(list(J_list = J_list, theta = theta, theta_list = theta_list))
}

getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
