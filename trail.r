library(data.table)
library(dplyr)
train = fread("data/train.csv")
test = fread("data/test.csv")

apply(train, 2, function(x) length(which(x == -1)))
apply(test, 2, function(x) length(which(x == -1)))

temp = train %>% select(contains("_cat"))
apply(temp, 2, function(x) length(which(x == -1)))

train[train == -1] = NA
test[test == -1] = NA

#ps_reg_03, ps_car_03_cat, ps_car_05_cat, ps_car_14
#removing these variables as they have large number of missing values
train$ps_reg_03 <- NULL
train$ps_car_03_cat <- NULL
train$ps_car_05_cat <- NULL
train$ps_car_14 <- NULL
train$ps_ind_09_bin <- NULL
train$ps_ind_14 <- NULL

test$ps_reg_03 <- NULL
test$ps_car_03_cat <- NULL
test$ps_car_05_cat <- NULL
test$ps_car_14 <- NULL
test$ps_ind_09_bin <- NULL
test$ps_ind_14 <- NULL

train = na.omit(train)
# t = subset(train, select = -eval(parse(text = c("ps_reg_03", "ps_car_03_cat",
#                                  "ps_car_05_cat", "ps_car_14"))))
DF[ , !(names(DF) %in% drops)]
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