###################
##  TFT PCA분석  ##
###################


df = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph1.csv",header = T, fileEncoding = 'CP949')

head(df)


# sensor 값 numeric으로 변경
# 안써도 코드 잘 돌아감
# for(i in 8:size[2]-1){
#   
#   imsi = as.numeric(df[,i]);   
#   
#   imsi[is.na(imsi)] <- 0     #해당 열에서 NA 값이 있을 경우 0으로 변환
#   
#   df[,i] <- imsi;
#   
# }

df_test = df[,8:49] # PCA set
df_test2 = df[,8:49] # Normalization PCA set

cor(df_test) # 상관관계

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
screeplot(pc) # 값들이 확 줄어서 나옴 -> 첫번째 값이 중요함 / 1,2번만 써도 될수도?

################### 아래부터는 scale만 추가됨 딱히 다른건 없음 ###################
##
## scale 방법은 min max
## min max 컬럼마다 독립 시행
## Normalization은 원래 아무때나 사용가능한 것은 아님
## 컬럼마다 상관관계가 큰 경우가 많은데 normalization을 하는 경우 
## 기존의 상관관계가 무너질 수 있음
##
##################################################################################

# min max 기본 정의
nor_minmax = function(x){
  result = (x - min(x)) / (max(x) - min(x))
  return(result)
} # 이거 안됨 전체 기준으로 min max가됨

min_max = function(x){
  k = matrix(0,nrow = nrow(x), ncol = ncol(x))
  for (i in 1:ncol(x)) { 
    result = (x[i]-min(x[i])) / (max(x[i])-min(x[i]))
    k[,i] = result
    
  }
  return (k)
}

df3 = scale(df_test2)

min_max(df_test2)

df2 = nor_minmax(df_test2)

cor(df3)

mat2 = cov(df3)

eig2 = eigen(mat3)

c = eig2$values 

det(mat3 - eig2$values*diag(42)) 

pc2=princomp(df_test2)

pc2$scores 

biplot(pc2) 

pc$loadings 

pc$sdev 

summary(pc) 
screeplot(pc) 
