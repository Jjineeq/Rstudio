f = function(x) -x^2 + 6*x -6

curve(f,1,5)

fd = function(f,x, h=x*sqrt(.Machine$double.eps)){
  return((f(x+h)-f(x))/h)
}

fd(f,2,h=1)
fd(f,2,h=0.5)


fx = expression(-x^2 + 6*x -6,"x")
D(fx,'x')

fx1 = expression(x^5-1/x +cos(x)^x,"x")
D(fx1,'x')

exp = deriv(fx1,c('x','y','z'))
exp

f2 = function(x,y,z) eval(c(exp))
f2(1,1,1)

"--------------"
#integrate

fx3 = function(x) 1/((x+1)*sqrt(x))
integrate(fx3, lower=0, upper = Inf)

fx4 = function(x) x^4 - 10*x^3 + 15*x^2 - 6*x +10
curve(fx4,1,10)

inter_by_you = function(a,b,n){
  sum = 0
  h = (b-a)/n
  for (i in 1:n) {
    sum = sum + h * fx4(a+i*h)
  }
}

inter_by_you(0,1,10)
integrate(fx4,0,1)


"--------------"


fx5 = function(x) x^2
integrate(fx5,-2,2)

set.seed(100)
a = -2
b = 2
c = 0
d = 4
n = 10^5
x = runif(n,a,b)
y = runif(n,c,d)
temp = sum(y<f(x))/n
16*temp


