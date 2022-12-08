###################
##  TFT PCA분석  ##
###################
# install.packages("devtools")
# library(devtools)
# install.packages("DAAG")
# install_github("vqv/ggbiplot")

library(rgl)
library(ggbiplot)
library(ggplot2)
library(corrplot)

source("C://Users/user/github/Function/R/msetRegression.R")
df = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph1.csv", fileEncoding = 'CP949') # train
df2 = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph2.csv", fileEncoding = 'CP949') # normal test
df3 = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph2_out.csv", fileEncoding = 'CP949') # abnormal test

df_tr = df[,8:49] # 필요 정보만 select
df_te = df2[,8:49] # 필요 정보만 select
df_te_2 = df3[,8:49] # 필요 정보만 select

te = rbind(df_te, df_te_2)

df_test = df[,8:49] # PCA set
df_test2 = df[,8:49] # Normalization PCA set

cor(df_test) # 상관관계
corrplot(cor(df_test))
corrplot.mixed(cor(df_test), upper = 'shade')

mat = cov(df_test) # 공분산

eig = eigen(mat)

c = eig$values # 설명정도 = 분산

det(mat - eig$values*diag(42))
# diag(4)-> 4*4 단위 행렬 // cov - lambda*diag() 


pc=princomp(df_test)

pc$scores # 줄인 차원에서는 data가 어떻게 생겼는가? 주성분으로 나타냄

plot(pc$scores[,1], pc$scores[,2]) # 주성분 2가지에 대해 4차원에서 2차원으로 줄여서 보여줌
biplot(pc) # 전체적인 주성분 확인 // scale해야 될 수도 있음 MAX 값들과 표준편차들의 차이가 커 제대로 확인이 어려움

pc$loadings # 나온 값들의 곱이 w // 아이겐 벡터랑 같음

pc$sdev 

summary(pc) # standard deviation -> 아이겐 벨류
screeplot(pc, type = 'l') # 값들이 확 줄어서 나옴 -> 첫번째 값이 중요함 / 1,2번만 써도 될수도?

# 다른 방법의 pca
pc2=prcomp(df_test, center = T,  scale = T)

pc2

plot(pc2, type = 'l')

summary(pc2) # cumulative Proportion이 누적 설명력 // pc13정도부터 80%이상 설명가능


g = ggbiplot(pc2, choices = c(1,2), obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE )
g = g + scale_color_discrete(name="")
g = g + theme(legend.direction = 'horizontal', legend.position = 'top')
g


##
fit = mset_regress(df_tr, df_tr)
fit$residual_tr
fit$residual_ts[,42]
plot(fit$residual_ts)

# par(mfrow = c(42,1))
# for (i in 1:ncol(df_tr)) {
#   plot(fit$residual_ts[,i], col = 'blue', lwd = 2, type = 'o')
#   ucl = bootlimit(fit$residual_tr[,i],0.1,100)
#   lcl = bootlimit(fit$residual_tr[,i],0.9,100)
#   
#   abline(h = c(ucl, lcl), col = 'red', lwd = 2)
# }
# plot(fit$residual_ts[1,2]) # 코드 상 오류는 없는데 너무 많아서 plot에 못들어감 pca통해서 variable 줄이고 plot 찍으면 볼 수 있음

# 아래코드는 변수 3개만 추출해서 돌린건데 잘 돌아감

# par(mfrow = c(1,1))
par(mfrow = c(3,1))
for (i in 1:3) {
  plot(fit$residual_ts[,i], col = 'blue', type = 'o')
  ucl = bootlimit(fit$residual_tr[,i],0.1,100)
  lcl = bootlimit(fit$residual_tr[,i],0.9,100)
  
  abline(h = c(ucl, lcl), col = 'red')
}

# pca 한거(pc1, pc2) 양측 검증 / 하긴했는데 무슨 의미인지 모르겠음..
fit2 = mset_regress(as.matrix(pc3$x[,1]), as.matrix(pc3$x[,1]))
fit2$residual_tr
fit2$residual_ts
plot(fit2$residual_ts, col = 'blue', type = 'o')
ucl2 = bootlimit(fit2$residual_tr, 0.1, 100)
lcl2 = bootlimit(fit2$residual_tr, 0.9, 100)
abline(h = c(ucl2, lcl2), col = 'red')

x <- pc2$x[,1]
y <- pc2$x[,2]
z <- pc2$x[,3]

plot3d(x,y,z, col = rainbow(1000))
plot(x,y)

ir
ir_mset = mset_regress(ir[1:50,],ir)

ir_mset$residual_tr

ir_mat = matrix(0,nrow(ir),1)
for(i in 1:nrow(ir_mset$residual_tr)){
  ir_mat[i,] = as.matrix(ir_mset$residual_ts[i,])%*%solve(cov(ir))%*%t(as.matrix(ir_mset$residual_ts[i,]))
}
as.matrix(ir_mset$residual_ts[1,])%*%solve(cov(ir))%*%t(as.matrix(ir_mset$residual_ts[1,]))
plot(ir_mat)

iris[,5]
