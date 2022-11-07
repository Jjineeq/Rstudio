#logistic regression using GA
library(caret) #min max scaling 
library(ggplot2) #통계툴

df = read.csv("C:/Users/User/Desktop/Social_Network_Ads.csv")# csv파일 read

#데이터 확인
head(df)
summary(df)

#Gender값 1,0 변경
df
df[df$Gender=='Male',"Gender"] = 1
df[df$Gender=='Female',"Gender"] = 0
df = df[2:5]
df
df$Gender = as.numeric(df$Gender)
typeof(df$Gender)

#변수간의 관계 plot
plot(df)
plot(df$EstimatedSalary,df$Purchased)
plot(df$Gender, df$Purchased)
plot(df$Age,df$Purchased)


# #min-max scaling 
# process = preProcess(as.data.frame(df[3]), method = c("range"))
# min_max = predict(process, as.data.frame(df[3]))
# min_max
# df$Age = min_max[1]
# 
# df
# 
# process = preProcess(as.data.frame(df[4]), method = c("range"))
# min_max = predict(process, as.data.frame(df[4]))
# min_max
# df$EstimatedSalary = min_max
# 
# df # normalization complete

#sigmoid function
sigmoid = function(z){
  return(1/(1+exp(-z)))
}  

#loss_function
loss_fun = function(y,a){
  return(J = -(y*log(a)+(1-y)*log(1-a)))
}
# 
# #tarin set, test set
# train = 0.7*nrow(df)
# train
# train_ = df[1:tarin,]
# test_ = df[tarin+1:120,]
# train_
# test_
# colnames(df)
# 
# model = glm(Purchased ~ ., data = train_,family = 'binomial')
# model
# summary(model)
# 
# pred = predict(model, newdata = test_, type = "response")
# head(pred)
# head(test_$Purchased)
# 
# result_pred = ifelse(pred>=0.5,1,0)
# Table = table(result_pred,test_$Purchased)
# Table # confusion matrix
# 
# total = matrix(0,nrow(test_),1)
# 
# # library(ROCR)
# # pr = prediction(pred,test_$Purchased)
# # prf = performance(pr,measure = "tpr", x.measure = "fpr")
# # plot(prf,main = "ROC Curve")
# 
# for (i in nrow(test_)) {
#   k = pred[i]
#   total[i] = sigmoid(i)
#   return (total[i])
# }
# 
# sigmoid
# pred
# plot(pred)

print("-------------------------------------------")

colnames(df)

ggplot(df,aes(x=Age,y=Purchased)) + geom_point()
ggplot(df,aes(x=Age,y=Purchased)) + geom_point() + stat_smooth()


ggplot(df,aes(x=EstimatedSalary,y=Purchased)) + geom_point()
ggplot(df,aes(x=EstimatedSalary,y=Purchased)) + geom_point() + stat_smooth()

ggplot(df,aes(x=EstimatedSalary,y=Purchased)) + geom_point()
ggplot(df,aes(x=Gender,y=Purchased)) + geom_point() + stat_smooth()

z = lm(formula = df$Purchased ~ df$Age + df$Gender + df$EstimatedSalary)
z$coefficients

beta0 = c(z$coefficients[1],z$coefficients[2],z$coefficients[3],z$coefficients[4])

beta0 = as.matrix(beta0)
beta0[1]
beta0[2]
beta0[3]
beta0[4]
typeof(beta0)

typeof(df[1,1])

nrow(test_)


q = matrix(0,nrow(test_),1)
w = matrix(0,nrow(test_),ncol(test_))
w
q
beta0[4]*test_[1,4]
q[1] = beta0[1] * test_[1,1]
beta0[1]
# test_ = as.data.frame(test_)
# test_$Gender=as.numeric(test_$Gender)
# test_$Age=as.numeric(test_$Age)
# test_$EstimatedSalary=as.numeric(test_$EstimatedSalary)
# test_$Purchased=as.numeric(test_$Purchased)


typeof(test_$Gender)
typeof(beta0[1])

i = 1
k = 1
for (i in 1:nrow(df)+1) {
  for (k in 1:ncol(df)+1) {
    w[i,k] = beta0[k] * df[i,k]
    return(w)
  }
  q[i] = w[i,1] + w[i,2] + w[i,3] + beta0[1]
  return(q)
}

w[1,2]=beta0[1]*df[1,2]

df[1,2]

w
q
