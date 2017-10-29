train = read.csv("data/train.csv")
test = read.csv("data/test.csv")
d = train[,-1]
e = test[,-1]
glm.fit = glm(target ~ ., data = d)
tg = predict(glm.fit, e, type = "response")

f = cbind(test[,1], tg)
