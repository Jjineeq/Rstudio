# 4주차 선형대수

#install.packages("matlib")
library(matlib)

xlim <- c(0,6)
ylim = c(0,6)
plot(xlim,ylim, type ='n', xlab = 'X1', ylab = 'x2', asp = 1)
grid()

#define some vectors
a = c(4,2)
b = c(1,3)

vectors(b,labels = "b", pos.lab = 4, frac.lab = .5, col = "green")
vectors(a,labels = "a", pos.lab = 4, frac.lab = .5, col = "black")
vectors(a+b,labels = "a+b", pos.lab = 4, frac.lab = .5, col = "red")

print("-------------------------------------")

x = c(3,5)
y = c(1,2)
sqrt((3-1)^2 + (5-2)^2)

x%*%y/(sqrt(a%*%a) %*% sqrt(b%*%b)) # x와 y의 cos(seta) 값

print("-----------------------------------------")

res = matrix(0,980,1)

for (i in 1:980){
  res[i,1] = cos(i*pi/180)#라디안으로 변경
}
plot(res,type = "l") # line으로 plot

which(res <= 0.7)

print("---------------------------------------")

#install.packages("pracma")
library("pracma")
v = c(1,2,1)
Norm(v,p=2)

P = matrix(c(2,4,3,2), ncol = 2)
Norm(P,p=1)

print("--------------------------------------------")

mat = matrix(c(5,25,35,25,166,175,35,175,325), ncol = 3)
mat
eanalysis = eigen(mat,symmetric =T)
eanalysis
prod(eanalysis$value) # = det(mat)
det(mat)
