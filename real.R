detach(package:ROCR)
detach(package:neuralnet)
library(dplyr)

df = read.csv("C:/Users/User/Desktop/Social_Network_Ads.csv")# csv파일 read

df

summary(df)
plot(df)
plot(df$EstimatedSalary,df$Purchased)
plot(df$Age,df$Purchased)

table(df$Purchased)

df[df$Gender=='Male',"Gender"] = 1
df[df$Gender=='Female',"Gender"] = 0
df = df[2:5]
df
df$Gender = as.numeric(df$Gender)

z = as.data.frame(df[1:4])
z

beta0 = sample(0:1,1)
beta1 = sample(0:1,1)
beta2 = sample(0:1,1)
beta3 = sample(0:1,1)
w = c(beta1,beta2,beta3)
w
z = z %>% mutate(cal = (beta0 + beta1*Gender + beta2*Age + beta3*EstimatedSalary))
z = as.data.frame(z)
z

sigmoid = function(z){
  return(1/(1+exp(-z)))
}

q = matrix(0, nrow(df), 1)
q # return y값
nrow(z)

cost = function(z){
  for (i in 1:nrow(z)) {
    t = z[i,5]
    if (z[i,4] == 1)  {
      q[i] == -log(sigmoid(t))
      # return(q[i])
    }else{
      q[i] == -(1-log(sigmoid(t)))
      # return(q[i])
    }
  }
  print(q)
}

cost(z)


g_desc = function(fp,x,h=1e-2, tol = 1e-4, m=1e3){
  iter = 0
  oldx = x
  x = x -h*fp(x) # initial gradient만큼 이동
  while (abs(x-oldx)>tol){
    iter = iter +1
    if (iter > m) stop("max iteration")
    oldx = x
    x = x -h*fp(x) # 부호가 +라면 gradient ascent
  }
  return (x)
}

z = as.matrix(z)
z= z[1:3]
w

loss_fun = function(x, t){
  delta = 1e-7
  k = z%*%w + beta0
  y = sigmoid(z)
  return(sum(t*log(y+delta)+(1-t)*log(1-y)+delta))
}

loss_fun(x,1)
