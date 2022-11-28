df = iris[,1:4]

df = scale(df)

##sigma(xi %*% t(x(i)))

tmp = df[1,]%*%t(df[1,])

result = matrix(0,4,4) 

tmp = function(x) {
  for (i in 1:nrow(x)) {
    tmp = df[i,]%*%t(df[i,])
    result = result + tmp
  }
  return(result/nrow(x))
}

tmp(df)

cor(df)
mat = cov(df)

eig = eigen(mat)

c = eig$values # 첫번째 축 설명 정도 -> 2.91 / 두번째 축 설명 정도 -> 0.914 ... // 설명정도 = 분산

det(mat - eig$values*diag(4))
# diag(4)-> 4*4 단위 행렬 // cov - lambda*diag() 


pc=princomp(df)
pc$scores # 줄인 차원에서는 data가 어떻게 생겼는가? 주성분으로 나타냄

plot(pc$scores[,1], pc$scores[,2]) # 주성분 2가지에 대해 4차원에서 2차원으로 줄여서 보여줌
biplot(pc) # 전체적인 주성분 확인

pc$loadings # 나온 값들의 곱이 w // w1 = v(0.521,-0.269,0.580,0.565) -> 아이겐 벡터랑 같음

pc$sdev 

summary(pc) # standard deviation -> 아이겐 벨류
screeplot(pc) # 값들이 확 줄어서 나옴 -> 첫번째 값이 중요함 / 1,2번만 쓰자는 결론을 얻을 수 있음

  