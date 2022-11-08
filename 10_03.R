## 데이터셋 불러오기 
## 독립변수: Petal.Length
## 반응변수: Species
df = datasets::iris
df = df[1:100,]
typeof(df)

Species = rbind(matrix(0, nrow = 50, ncol = 1),matrix(1, nrow = 50, ncol = 1))

df = cbind(df$Petal.Length,Species)



plot(df[1:100,1],df[1:100,2])

## 1변수 로지스틱 함수 구현
logistic = function(beta_0,beta_1,x_1){
  
  z = beta_0 + beta_1*x_1
  
  return (1/(1 + exp(-z)))
}

df
## 값 확인 beta_0 = 1, beta_1 = 0.5, Petal.Length = 1.4
logistic(1,0.5,1.4)

## threshold 0.5 이상이면 1 아니면 0
pred = function(x){
  if (x>0.5){
    return(1)
  }else{
    return(0)
  }
}

pred(logistic(1,0.5,1.4))

cross_entropy = function(beta_0,beta_1,x_1,t){
  pred = logistic(beta_0,beta_1,x_1)
  if(t==1){
    return (-log(pred))
    
  }else{
    return (-log(1-pred))
  }
}

## 엔트로피 계산
cross_entropy(-0.46,0.33,1.4,0)

## 총 비용함수
cost = function(beta_0,beta_1,data){
  loss = 0 
  for (i in 1:nrow(data)){
    x_1 = data[i,1] 
    t = data[i,2]
    
    loss = loss + cross_entropy(beta_0,beta_1,x_1,t)
  }
  
  return(loss)
  
}

## 기울기 계산
cal_grad = function(x_1,x_2,loss_1,loss_2){
  return((loss_2-loss_1)/(x_2-x_1))
}


## 기울기계산산
cal_grad(1,2,3,6)

## 경사하강법 구현 beta_1 추정임 

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

gd_beta_0 = function(init, data, rate){
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
gd_beta_1(init = 10,data = df, rate = 0.02) ## 베타1 추정
gd_beta_0(init = 10,data = df, rate = 0.015) ## 베타2 추정

## glm 써서 coef 비교
model = glm(df[1:100,2] ~ df[1:100,1])
summary(model)
