###################
##  TFT PCA분석  ##
###################

options(warn = -1) # 경고끄기
# install.packages("devtools")
# library(devtools)
# install.packages("DAAG")
# install_github("vqv/ggbiplot")
#install.packages('psych')

library(psych)
library(rgl)
library(ggbiplot)
library(ggplot2)
library(corrplot)
library(plotly)

df = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph1_bytime.csv", fileEncoding = 'CP949') # train
df2 = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph2_in.csv", fileEncoding = 'CP949') # normal test
df3 = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph2_out.csv", fileEncoding = 'CP949') # abnormal test

df_tr = df[,8:49] # 필요 정보만 select
df_te = df2[,8:49] # 필요 정보만 select
df_te_2 = df3[,8:49] # 필요 정보만 select

te = rbind(df_te, df_te_2)
tk = rbind(df_tr, df_te)

df_test = df[,8:49] # PCA set
df_test2 = df[,8:49] # Normalization PCA set

df_test5 = cbind(df[,1], df_test)


cor(df_test) # 상관관계
#corrplot.mixed(cor(df_test), cl.pos = 'n', tl.pos = 'n') # 에러는 안뜨는데 확인이 어려움
corrplot.mixed(cor(df_test), upper = 'shade')

## 유의미한지 확인 -> 나중에 feature selection 하면 필요할 수도 있음

cor_test_mat = corr.test(df_test)$p

corrplot(cor(df_test), p.mat = cor_test_mat, cl.pos = 'n', tl.pos = 'n' )
corrplot(cor(df_test),method = 'number', cl.pos = 'n', tl.pos = 'n' )

##
mat = cov(df_test) # 공분산

eig = eigen(mat)

c = eig$values # 설명정도 = 분산

det(mat - eig$values*diag(42))
# diag(4)-> 4*4 단위 행렬 // cov - lambda*diag() 

pc=princomp(df_test)

pc$scores # 줄인 차원에서는 data가 어떻게 생겼는가? 주성분으로 나타냄

plot(pc$scores[,1], pc$scores[,2]) # 주성분 2가지에 대해 4차원에서 2차원으로 줄여서 보여줌
biplot(pc) # 전체적인 주성분 확인 

pc$loadings # 나온 값들의 곱이 w // 아이겐 벡터랑 같음

pc$sdev 

summary(pc) # standard deviation -> 아이겐 벨류
screeplot(pc, type = 'l') # 값들이 확 줄어서 나옴 -> 첫번째 값이 중요함 / 1,2번만 사용해도 무관

## PCA 2차원 시각화
k = ggbiplot(pc, choices = c(1,2), obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE )
k = k + scale_color_discrete(name="")
k = k + theme(legend.direction = 'horizontal', legend.position = 'top')
k

# 다른 방법의 pca
pc2=prcomp(df_test, center = T,  scale = T)

pc2

plot(pc2, type = 'l')

summary(pc2) # cumulative Proportion이 누적 설명력 // pc13정도부터 80%이상 설명가능

g = ggbiplot(pc2, choices = c(1,2), obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE )
g = g + scale_color_discrete(name="")
g = g + theme(legend.direction = 'horizontal', legend.position = 'top')
g

## Tool별 3차원 시각화
par(mfrow = c(1,1))

x <- pc2$x[,1]
y <- pc2$x[,2]
z <- pc2$x[,3]
o = df[,1]
q = list(o,x,y,z)
q_mat <- matrix(unlist(q), ncol =4)
q_mat = as.data.frame(q_mat)

cols = c('blue', 'red','skyblue','green')

plot3d(q_mat[,2],q_mat[,3],q_mat[,4], 
       col = cols[as.factor(q_mat$V1)],
       xlab = 'pc1',
       ylab = 'pc2',
       zlab = 'pc3',
       
       )

# pca해서 3차원 표현 
# 위랑 동일함 대신 좌표 볼 수 있음

g1 = plot_ly(x = q_mat[,2], y = q_mat[,3], z = q_mat[,4], 
             type = "scatter3d", 
             mode = 'markers' ,
             color = cols[as.factor(q_mat$V1)] )
g1

plot(x,y) # pca해서 2차원 표현
