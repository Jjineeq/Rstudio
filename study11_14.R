library(pracma)

t_squre = function(trdat, tedat,alpha){
  obs = nrow(trdat)
  dim = ncol(trdat)
  mu = colMeans(trdat)
  
  CL = qf(1-alpha, dim, obs -dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)))
  sinv = ginv(cov(trdat))
  mu_mat = repmat(mu, nrow(tedat),1)
  dte = as.matrix(tedat-mu_mat)
  
  Tsq_mat = matrix(numeric(0), nrow(tedat),1)
  
  for (i in 1:nrow(tedat)) {
    Tsq_mat[i,1] = as.double(dte[i,]) %*% sinv %*% t(t(as.double(dte[i,])))
  }
  
  ret = list(
    Tsq_mat = Tsq_mat,
    CL = CL
  )
  return(ret)
}


tr = iris[1:100, 1:4]
ts = iris[1:150, 1:4]
setosa = iris[iris$Species == "setosa", 1:4]

a = t_squre(setosa,ts,0.8)

a$Tsq_mat
a$CL

plot(a$Tsq_mat, type = 'o')
abline(h = c(a$CL), col ='red', lwd=2)

beta = a$Tsq_mat>a$CL
sum(beta)

a$Tsq_mat[1:50]
a$Tsq_mat

k  = sum(a$Tsq_mat[1:100] > a$CL)/100

k

# code로 베타 곡선 그리기 
# alpha 값에 따른 beta 값 -> 반비례 관계 그래프 그리기
# 적은 알파로 좋은 알파 값 찾기

for (i in 1;) {
  
}