library(pracma)

a = c(50,70)
b = c(70,80)
c = c(50,90)
d = c(80,60)
e = c(90,95)
f = c(50,55)

univs = rbind(a,b,c,d,e,f)

plot(univs, xlab = '입학성적', ylab = '취업률', xlim=c(40,100), ylim =c(40,100))
main = "scatter plot of scores"
text(univs[,1],univs[,2], labels = abbreviate(rownames(univs)), cex = 0.8, pos = 1, col= 'blue')

eucl = dist(univs, method = 'euclidean')
print(eucl)

a-b
sum(abs(a-b))
eucl0 = t(a-b)%*%(a-b) #manhattan 
eucl0

eucl2 = dist(univs[c(1,3),], method = "euclidean")
print(eucl2)

sqrt(t(a-b)%*%(a-b))

xbar = as.vector(colMeans(univs[c(1,2,5,6),]))
cova = cov(univs[c(1,2,5,6),])
xbar
cova

sqrt(t(c-xbar)%*%solve(cova)%*%(c-xbar))
sqrt(t(d-xbar)%*%solve(cova)%*%(d-xbar))

print("-----------------------------")
# 마할라노비스 거리 구현

t_square <- function(trdat, tedat, alpha) {
  
  obs = nrow(trdat)
  dim = ncol(trdat)
  
  mu = colMeans(trdat)
  
  CL = qf(1-alpha,dim,obs - dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)))
  library(pracma)
  sinv = ginv(cov(trdat))  
  
  mu_mat = repmat(mu, nrow(tedat),1)
  dte = tedat-mu_mat
  
  
  
  Tsq_mat = matrix(numeric(0), nrow(tedat),1) 
  
  for( i in 1:nrow(tedat)) {
    Tsq_mat[i,1] = as.double(dte[i,]) %*% sinv %*% t(t(as.double(dte[i,])))
  }
  
  ret <- list(
    Tsq_mat =Tsq_mat,
    CL = CL
  )
  return (ret)                                    
}

df = iris[,1:4]
setosa = iris[iris$Species == "setosa",1:4]
t2 = t_square(setosa, df[,1:4],alpha = 0.1) # training을 setosa로 전체데이터를 testing set으로 설정하여 진행한다는 의미, setosa 데이터의 90 percentile 값을 threshold로 활용 (alpha = 0.1)

## control chart (차트에 시각화)



plot(t2$Tsq_mat, type = "o", lwd = 2, col = "blue")
abline(h = c(t2$CL), col = "red", lwd = 2)

print("===================================================")

