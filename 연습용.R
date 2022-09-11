for (i in 1:10) {
  if (i%%2 == 0){
    print("Â¦¼ö")
  }
  else {
    print("È¦¼ö")
  }
}

f = function(x)-x^2+6*x-6
curve(f,1,5)

fd = function(f,x,h=x*sqrt(.Machine$double.eps)){
  return((f(x+h)-f(x))/h)
}

fd(f,2,1)
fd(f,2,0.1)
fd(f,2,0.00000000001)

fx = expression(-x^2+6*x-6,"x")
D(fx,'x')

f <- function(x) eval(c(fx)[[1]])
f(2)

fxyz = expression(x^5-1/x+cos(x)^x,'x')
exp = deriv(fxyz,c("x","y","z"))
exp

f2 = function(x,y,z) eval(c(exp))
f2(1,1,1)

fx1 = function(x) 1/((x+1)*sqrt(x))
integrate(fx1,lower = 0, upper = Inf)

fx3 = function(x) x^4-10*x^3+15*x^2 -6*x+10
curve(fx3)

inter_by_you = function(a,b,n){
  sum = 0
  h = (b-a)/n
  for (i in 1:n) sum = sum + h *fx3(a+i*h)
  print(sum)
}
inter_by_you(0,1,10)

integrate(fx,0,1)

fx2 = function(x) x^2
set.seed(132)
a = -2
b = 2
c = 0
d = 4
n =10^5

x = runif(n,a,b)
y = runif(n,c,d)

16*temp 

a = c(1,4)
b = c(3,6)
a%*%b/(sqrt(a%*%a)%*%sqrt(b%*%b))

res = matrix(0,980,1)
for (i in 1:980) {
  res[i,1] = cos(i*pi/180)
}
plot(res,type = "l")
which(res<=0.7)

mat = matrix(c(5,25,35,25,155,175,35,175,325),ncol = 3)
mat

enalysis = eigen(mat,symmetric = T)
det(enalysis$vectors%*%enalysis$vectors)

prod(enalysis$values) 
det(mat)

install.packages("pracma")
library(pracma)

v = c(1,2,1)
Norm(v,p=2)

P = matrix(c(2,4,3,2),ncol=2)
Norm(P,p=1)

install.packages("matlib")
library(matlib)
xlim <-c(0,10)
ylim <-c(0,10)
plot(xlim,ylim, type = "n", xlab ="x1",ylab = "y1",asp=1)
grid()

a=c(4,5)
b=c(1,3)

vectors(b,labels = "b",pos.lab = 4, frac.lab = 5,col=colors()[3])
vectors(a,labels = 4,frac.lab = 5,col=colors()[50])

ha = slove(t(x)%*%x)%*%(t(x)%*%y)
plot(pred)
co = cor(y,predict)
co^2
vectors(a+b, labels = "a+b", pos.lab = 4, frac.lab = 5,col=colors()[55])

