# 4주차 빅데분 강의 - 1
fd = function(f,x,h= x*sqrt(.Machine$double.eps)){
  return((f(x+h)-f(x))/h)
}
f = function(x) -x^2 + 6*x-6
curve(f,0,10)
fd(f,2,1)
fd(f,2,0.01)
fd(f,2,0.00000001)

print("------------------------------------")

fxyz = expression(x^5-1/x+cos(x)^z)

exp = deriv(fxyz, c("x", "y", "z"))
exp

f2 = function(x,y,z) eval(c(exp))
f2(1,1,1)

print("----------------------------")

fx = function(x) 1/((x+1)*sqrt(x))
integrate(fx,lower = 0, upper = Inf)

print("----------------------------")

fx = function(x) x^4 - 10*x^3 + 15*x^2 - 6*x +10
curve(fx)

inter_by_you = function(a,b,n){
  sum = 0
  h = (b-a)/n
  for (i in 1:n) sum = sum + h*fx(a+i*h)
  print(sum)
}

inter_by_you(0,1,10)

integrate(fx,0,1)

print("-------------------------------")

fx = function(x) x^2
integrate(fx,-2,2)

set.seed(123)
a=-2
b= 2
c=0
d=4
n=10^5

x = runif(n,a,b)
y = runif(n,c,d)
temp = sum(y<fx(x))/n
16*temp

print("---------------------------------")