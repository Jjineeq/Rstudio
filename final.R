df = read.csv("C:/Users/User/Desktop/Social_Network_Ads.csv")# csv파일 read

df

#label encoding
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

test_x = df[1:3]
test_x = cbind(1,test_x)
test_x # 내적할 행렬

#beta 초기값
beta_0 = 0.3
beta_1 = 0.5
beta_2 = 0.4
beta_3 = 0.3
beta0 = c(beta_0,beta_1,beta_2,beta_3)

beta0

threshold = 1e-6 # 판정 임계값
rate = 0.01 #학습률 

# 다변수 로지스틱 함수 구현
logistic = function(data){
  
  z = beta_0 + beta_1*data[1] + beta_2*data[2] + beta_3*data[3]
  
  return (1/(1 + exp(-z)))
}

sigmoid = function(x){
  return(1/(1 + exp(-x)))
}

logistic(df)

# threshold 0.5 이상이면 1 아니면 0

pred = function(data){
  pred_ <- c()
  x = logistic(data)
  for (i in 1:nrow(x)) {
    if (x[i,1]>0.5){
      pred_=c(pred_,1)
    }else{
      pred_=c(pred_,0)
    }
  }
  return(pred_)
}

t = as.matrix(pred(df))
t

cross_entropy = function(data){
  pred = logistic(data)
  for (i in nrow(data)) {
    if(data[i,4]==1){
      return (-log(pred))
    }else{
      return (-log(1-pred))
    }
  }
}

cross_entropy(df)

cost = function(data){
  cost_ <- c()
  for (i in 1:nrow(data)) {
    cost_0 = -data[ncol(data)][i,1] * log(logistic(data)[i,1]) - (1-data[ncol(data)][i,1]) * log(logistic(data)[i,1]) # %*%으로 하면 한줄로 가능 나중에 수정 
    cost_ = c(cost_, cost_0)
  }
  return(cost_)
}

cost(df)

total_cost = function(data){
  return(sum(cost(data))/400)
}

gradient = t(as.matrix(test_x))%*%as.matrix(df[4])/400 # update를 진행할 beta0, beta1, beta2, beta3 값
gradient

gd = function(data){
  gradient = t(as.matrix(test_x))%*%as.matrix(data[ncol(data)])
  w <- c()
  for (k in 1:nrow(data)) { # 1:400
    for (i in 1:1e+5) {
      w[k] = (rate * gradient)
      gradient[k] = gradient[k] - w[k]
    }
  }
  return(w)
}

gd(df)

w = gd(df)[1:4]
w

# predict = function(data){
#   h = cbind(1,data[1:ncol(data)-1])
#   as.matrix(t(h))%*%as.matrix(w[1:4])
# }
# h = cbind(1,df[1:ncol(df)-1])
# h
# sigmoid(as.matrix(h)%*%as.matrix(w[1:4]) + w[1])
# 
# w[1]
# ncol(t(h))
# nrow(t(h))
# 
# ncol(as.matrix(w[1:4]))
# nrow(as.matrix(w[1:4]))
# 
# names(df)

model = glm(formula = Purchased ~ Gender + Age + EstimatedSalary, data = df)
summary(model)

sigmoid(as.matrix(test_x)%*%as.matrix(beta0))


"
현재 한계점으로 다변량 data을 미분하려면 자코비안으로 미분 값이 나오게되는데
여기서 파라미터 추정하는 것으로 반영을 못해 파라미터 학습이 가능하지 않습니다.
학습 반영 불가로 인해 glm라이브러리를 사용하여 파라미터를 비교한 결과와 큰 차이가 발생했습니다.
"


