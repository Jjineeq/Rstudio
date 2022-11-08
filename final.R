library(ggplot2)
library(caret) #min max scaling 

df = read.csv("C:/Users/User/Desktop/Social_Network_Ads.csv")# csv파일 read

df
typeof(df)

df[df$Gender=='Male',"Gender"] = 1
df[df$Gender=='Female',"Gender"] = 0
df = df[2:5]
df
df$Gender = as.numeric(df$Gender)

df[2:3]
#min-max scaling
process = preProcess(as.data.frame(df[2]), method = c("range"))
min_max = predict(process, as.data.frame(df[2]))
min_max
df$EstimatedSalary = min_max[1]
df$Age = min_max[1]

process = preProcess(as.data.frame(df[3]), method = c("range"))
min_max = predict(process, as.data.frame(df[3]))
df$EstimatedSalary = min_max[1]

df

## 1변수 로지스틱 함수 구현
logistic = function(beta_0,beta_1,beta_2,beta_3,x_1,x_2,x_3){
  
  z = beta_0 + beta_1*x_1 + beta_2*x_2 + beta_3*x_3
  
  return (1/(1 + exp(-z)))
}

logistic(1,0.5,1.4,0.2,0.5,0.3,0.2)

## threshold 0.5 이상이면 1 아니면 0
pred = function(x){
  if (x>0.5){
    return(1)
  }else{
    return(0)
  }
}

pred(logistic(1,0.5,1.4,0.2,0.5,0.3,0.2))

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
  loss = 0 
  for (i in 1:nrow(data)){
    x_1 = data[i,1] 
    x_2 = data[i,2]
    x_3 = data[i,3]
    t = data[i,4]
    loss = loss + cross_entropy(beta_0,beta_1,beta_2,beta_3,x_1,x_2,x_3,t)
  }
  
  return(loss)
  
}

## 기울기 계산
cal_grad = function(x_1,x_2,loss_1,loss_2){
  return((loss_2-loss_1)/(x_2-x_1))
}

## 경사하강법 구현 beta_1 추정임  ###### 자코비안 구현 해야됨

gd_beta_1 = function(init, data, rate){
  oldx = init
  x = init+1
  while(abs(x-oldx)>0.001){
    loss_1 = cost(0,oldx,data)
    loss_2 = cost(0,x,data)
    
    grad = cal_grad(oldx, x, loss_1, loss_2)
    
    new_x = x - rate*grad
    
    ## 업데이트
    oldx = x
    x = new_x
    
  }
  return(x)
}

gd_beta_0 = function(init, data,rate){
  oldx = init
  x = init+1
  while(abs(x-oldx)>0.001){
    loss_1 = cost(oldx,0,data)
    loss_2 = cost(x,0,data)
    
    grad = cal_grad(oldx, x, loss_1, loss_2)
    
    new_x = x - rate*grad
    
    ## 업데이트
    oldx = x
    x = new_x
    
  }
  return(x)
}

## 경사하강법을 통해 계수 추청
gd_beta_1(init = 10,data = df, rate = 0.1) ## 베타1 추정
gd_beta_0(init = 10,data = df, rate = 0.1) ## 베타2 추정

## glm 써서 coef 비교
model = glm(df[1:100,2] ~ df[1:100,1])
summary(model)
