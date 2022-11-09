library(ggplot2)

df = read.csv("C:/Users/User/Desktop/Social_Network_Ads.csv")# csv파일 read

df
typeof(df)

df[df$Gender=='Male',"Gender"] = 1
df[df$Gender=='Female',"Gender"] = 0
df = df[2:5]
df
df$Gender = as.numeric(df$Gender)
df$EstimatedSalary = as.data.frame(df$EstimatedSalary)

df[2:3]
# min-max scaling
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}
df[2]=normalize(df[2])
df[3]=normalize(df[3])
df

beta_0 = 0
beta_1 = 0
beta_2 = 0
beta_3 = 0

# 다변수 로지스틱 함수 구현
logistic = function(data){
  
  z = beta_0 + beta_1*data[1] + beta_2*data[2] + beta_3*data[3]
  
  return (1/(1 + exp(-z)))
}

logistic(df)[3,]
nrow(df)

# threshold 0.5 이상이면 1 아니면 0
pred = function(x){
  if (x>0.5){
    return(1)
  }else{
    return(0)
  }
}

cross_entropy = function(beta_0,beta_1,beta_2,beta_3,x_1,x_2,x_3,t){
  pred = logistic(beta_0,beta_1,beta_2,beta_3,x_1,x_2,x_3)
  if(t==1){
    return (-log(pred))
    
  }else{
    return (-log(1-pred))
  }
}

## 엔트로피 계산
cross_entropy(1,0.5,1.4,0.2,0.5,0.3,0.2,0.3)

## 총 비용함수
cost = function(beta_0,beta_1,beta_2,beta_3,data){
  loss = matrix(0,4,1) 
  for (i in 1:nrow(data)){
    x_1 = data[i,1] 
    x_2 = data[i,2]
    x_3 = data[i,3]
    t = data[i,4]
    loss[i] = loss[i] + cross_entropy(beta_0,beta_1,beta_2,beta_3,x_1,x_2,x_3,t)
  }
  return(loss)
}

## 기울기 계산
cal_grad = function(x_1,x_2,loss_1,loss_2){
  return((loss_2-loss_1)/(x_2-x_1))
}

## 경사하강법 구현 beta_1 추정임  ###### 자코비안 구현 해야됨

ncol(df)
x = matrix(c(0,0,0,0),ncol=4)
init1 = 0
init2 = 0
init3 = 0
init4 = 0

# gd_beta = function(data, rate){
#   oldx = matrix(c(init1,init2,init3,init4))
#   x = matrix(c(0,0,0,0))
#   for (i in 1:4) {
#     x[i] = oldx[i]+1
#     while(abs(x[i]-oldx[i])>0.001){
#       loss_1 = cost(0,oldx[i],data[i])
#       loss_2 = cost(0,x[i],data[i])
#       grad = cal_grad(oldx[i], x[i], loss_1, loss_2)
#       new_x = x - rate*grad
#       ## 업데이트
#       oldx[i] = x[i]
#       x[i] = new_x
#     }
#     return(x)
#   }
# }

cost(0,oldx[1],data[1])

f = function(x){
  for (i in 1:4) {
    k[i] = df[i]
  }
  return(c(k))
}
data = f(df)
data[2]

f(df)[1]

names(df)
f(df)[1]
f(df)[2]
f(df)[3]
f(df)[4]

## 경사하강법을 통해 계수 추청
gd_beta(init = 10,data = f(df), rate = 0.1) ## 베타 추정

## glm 써서 coef 비교
model = glm(df[1:100,2] ~ df[1:100,1])
summary(model)
