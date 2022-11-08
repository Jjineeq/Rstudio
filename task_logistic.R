library(plyr)

data = read.csv("C:/Users/user/Desktop/Social_Network_Ads.csv")
data
names(data)
data = data[2:5]
data

summary(data) #na 값은 없음 따로 drop 안해도 됨

logistic = glm(Purchased ~ Gender + Age + EstimatedSalary , family = binomial, data = data)

logistic

#glm.fit

summary(logistic)

#########

logistic.model = step(logistic,direction = "backward")
summary(logistic.model)
plot(logistic)

#confint(logistic)

Odd = function(x, digits = 2){
  suppressMessages(a == confint(x))
  logistic = data.frame(exp(coef(x)),exp(a))
  logistic = round(logistic, digits)
  logistic = cblind(logistic, round(summary(x)$coefficient[,4],3))
  colnames(result) = c("OR","2.5%","97.5%","p")
  result
}
summary(logistic)

Odd(logistic)
