# 최적화 1

newton_raphson=function(f, fp, init, tol = 1e-9,max =100){ # tol은 0에 가까운 값이나 0은 아님  
  iter = 0 # iteration 시작,
  oldx = init  # x0를 의미
  x = oldx + 10*tol  #밑에 x-oldx가 tol 보다 커야되는 조건
  fp = D(expression(f),"x")
  
  #converge
  while(abs(x-oldx)>tol){## 수렴할때까지 run
    iter = iter +1
    
    if(iter>max){ 
      stop("there is no solution")  ## 100번 돌 때까지 수렴하지 않으면 출력
    }
    
    oldx = x
    x = x -f(x)/fp(x)   # 뉴튼랩슨 구현
  }
  
  return(paste("solution is", x))      
}

fx = function(x) exp(-x)+x
#library(Deriv)

#fp = D(expression(exp(-x)+x),"x")

newton_raphson(fx,10, init = 5)

secant = function(f,init,tol = 1e-9,max = 100){
  i = 0
  oldx = init
  oldfx = f(init)              #초기값에 따른 y값
  x = oldx +10*tol             # 밑에 조건 참고
  
  # convergence
  while(abs(x-oldx)>tol){
    i = i+1
    
    if(i>max) stop("there is no solution")
    fx = f(x)                        # 업데이트된 y
    newx = x-f(x)*((x-oldx)/(fx-oldfx))  # secant, 할선의 방정식 (즉, y=0을 지나는 x 찾기)
    oldx = x                                # 기존 oldX 는 x로 바꿔주고
    oldfx = fx                               # oldfx도 fx로 바꿔주고
    x = newx                                 # x는 newx로 업데이트
    cat("iteration",i,"value of x is:", x,"\n")
  }
  return(x)
}

fx = function(x) exp(-x)+x

secant(fx,10)

# 최적화2

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


f <-readline(prompt="Function? ") # 수식입력
f = parse(text = f) # 수식을 expression으로
fd = D(f,'x') #text형태로 된 수식 미분0

fd = function(x) 0.25 * (4 * x^3) + 3 * x^2 - 1
# 미분식을 활용 function으로 다시 만듬

Res = g_desc(fd,-1)
curve(fd,-5,2)
abline(v=c(-1), lty =2, col ="blue")
abline(v=c(Res), lty =2, col ="red")

res = g_desc(fd,2)
abline(v=c(2), lty =2, col ="blue")
abline(v=c(res), lty =2, col ="red")


print("--------------------")


# f = function(x) 0.25*x^4+x^3-x-1
# plot(f,main="f(x) = 0.25*x^4+x^3-x-1",xlim =c(-5,2),ylim = c(-5,5))
# abline(v=c(-1), lty =2, col ="blue")
# abline(v=c(Res), lty =2, col ="red")
# 
# res = g_desc(fd,2)
# abline(v=c(2), lty =2, col ="blue")
# abline(v=c(res), lty =2, col ="red")

print("-----------------------")

x <- seq(-5, 5, length= 100)
y <- x
f <- function(x, y) { (x-1)^2 +(2*y-1)^2 }
z <- outer(x, y, f)
op <- par(bg = "white")
persp(x, y, z, theta=45, phi=30, col = "lightblue")

print("------------------------")

g_bi = function(fp,x,h=1e-2, tol = 1e-4, m=1e3){
  iter = 0
  oldx = x
  x = x -h*fp(x) # initial gradient만큼 이동
  b = x-oldx 
  vecnorm = sqrt(sum(b^2)) # 벡터놈 정의
  
  
  while ((vecnorm)>tol){ # 다변수에서는 이동량을 벡터놈으로 정의
    iter = iter +1
    if (iter > m) return(x) # 수렴횟수보다 넘어서면 결과값 산출
    oldx = x
    x = x -h*fp(x) # 부호가 +라면 gradient ascent
  }
  return (x)
}

fp = function(x){  ##  다변수미분을 위한 방정식 셋팅 (전장의 식을 미분한 다변수 도함수)
  x1 = 2*x[1]-2    ## x[1] : x벡터중 첫번째 element
  x2 = 8*x[2]-4    ## x[2] : x벡터중 두번째 element
  return(c(x1,x2))
}

g_bi(fp, c(0,0),m=1000) # 수렴횟수를 1000번으로 제한 , 결과는 ?

print("---------------------")

library(numDeriv)

func <- function(x) {
  a <- x[1]
  b <- x[2]
  rez <- (a^2)*(b^3)
  rez
}

grad(func, c(1,2))

hessian(func, c(1,2))

print("----------------------")

#install.packages("numDeriv")

library(numDeriv)

f <- function(x) {c(x[1]^2 + x[2]^2 - 1, sin(pi*x[1]/2) + x[2]^3)}

g <- function(x) {(x[1])^3+(x[2])^2}

jacobian(f, c(1,1)) 

hessian(g, c(1,1))

print("----------------------------")

#install.packages("imager")
library(imager)
im = load.image('C:/Users/User/Desktop/Transgrid-electrical-network-map-for-NSW-Australia.png')
plot(im)
grad = imgradient(im)
plot(grad)
hess = imhessian(im)
plot(hess)

print("--------------------------------")
#install.packages("plot3D")
library(plot3D)

rastrigin = function(x1,x2){20+x1^2 + x2^2 -10*(cos(2*pi*x1)+cos(2*pi*x2))}
x1 = seq(-5,5,by = 0.1)
x2 = seq(-5,5,by = 0.1)

f = outer(x1,x2, rastrigin)

persp3D(x1,x2,f, theta = 50, phi=20)

print("------------------------------------")

#install.packages("GA")
library(GA) # GA 라이브러리 가져오기
Rastrigin = function(x1,x2){20+x1^2 + x2^2 -10*(cos(2*pi*x1)+cos(2*pi*x2))} # rastrigin 함수
x1 = seq(-5,5,by = 0.1)
x2 = seq(-5,5,by = 0.1) ## x,y의 영역

f = outer(x1,x2, rastrigin) # 함수의 외적

persp3D(x1,x2,f, theta = 50, phi=20) # x,y에 따른 외적값

ga = ga(type = "real-valued", fitness  = function(x) - rastrigin(x[1],x[2])
        ,lower = c(-5,-5), upper = c(5,5), popSize = 1000,maxiter = 100, pcrossover = 0.8, 
        pmutation = 0.1) #벡터

summary(ga)

plot(ga) # GA 함수 수렴 시각화
ga@solution # 최소값을 만족시키는 x1, x2

rastrigin(ga@solution[1],ga@solution[2]) # 최소목적함수값

print("---------------------")
csv = read.csv("C:/Users/User/Desktop/measurmens.csv")
csv2 = read.csv("C:/Users/User/Desktop/kalmanv.csv")


csv
summary(csv)
summary(csv2)

z = csv[,1]

z
k = csv2[,2]
k
k =t(k)
k
q = z%*%k
q
u = q[0:8,0:8]
u

det(u)



z = matrix(c(1,2,3,4), ncol=2)
z
det(z)