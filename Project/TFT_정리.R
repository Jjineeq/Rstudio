#########################
###   TFT 총 정리ver  ###
#########################

# solve와 ginv 둘중 어떤 함수를 사용하는가에 따라 main함수의 성능 차이가 발생
# solve가 진짜 역행렬을 구하는 것으로 성능이 더 좋습니다.
# ginv의 경우 유사 역행렬으로, solve를 할 수 없는 matrix일때 사용합니다.
# 보통 한 열 또는 행이 0에 수렴하는 경우에 역행렬을 구할 수 없어 발생하는데
# 이럴때는 차라리 0에 수렴하는 것을 drop하고 solve로 풀이하는 것이 더 좋을때가 많습니다.
# TFT 데이터의 경우에는 solve로 진행해도 역행렬을 바로 구할 수 있어 해당사항은 없지만
# solve, ginv 차이를 보여주기 위해서 두가지로 코드 구성했습니다.


# boostrap, t_square, cbm 불러오기
source("C:/Users/User/github/Function/R/t_square.R")
source("C:/Users/User/github/Function/R/CBM.R")
source("C:/Users/User/github/Function/R/bootstrap1.R")

# library 불러오기
library(corrplot)
library(MASS)
library(pracma)
library(autoencoder)

# 데이터 불러오기
df = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph1.csv", fileEncoding = 'CP949') # train
df2 = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph2.csv", fileEncoding = 'CP949') # normal test
df3 = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph2_out.csv", fileEncoding = 'CP949') # abnormal test

# 필요 없는 데이터 제거
df_tr = df[,8:49] 
df_te = df2[,8:49] 
df_te_2 = df3[,8:49]

# normal, abnormal test bind -> total test
te = rbind(df_te, df_te_2)
tk = rbind(df_tr, te)

###################
#### ginv 풀이 ####
###################

# Hotelling T square 진행 -> 다변량 t 검정
# 집단간 차이가 없다고 가정 -> 다음에 확인해볼게요 / 무조건 귀무가설 채택되긴 할겁니다.
df_t2 = t_square(df_tr, te, 0.05)
df_t2_2 = t_square(df_tr, tk, 0.05)


# 데이터 확인
plot(df_t2$Tsq_mat, type = 'o', ylim = c(0,600)) 
plot(df_t2_2$Tsq_mat, type = 'l', ylim = c(0,700), xlab = 'time', ylab = 'T-square') 

# t square으로 나온 limit plot
abline(h = c(df_t2$CL), col = 'red')

###############################################
# 유의수준 변경에 따른 alpha, beta error 확인 #
###############################################

# output 저장할 빈 matrix 정의
mat = matrix(0,1000,3)

i = 1 # 아래 t_square 돌리기 위해서 대충 정의 

s1 = t_square(df_tr, df_tr, i/1000) # 정상 data에 대한 t-square
t2 = t_square(df_tr, te, i/1000) # 모든 data에 대한 t-square

# 유의수준에 따른 error 확인
for (i in 1:1000) {
  s2 = bootlimit1(s1$Tsq_mat,i/1000,100) # boostrap은 정상에 대해 진행해야 됨
  mat[i,1] = i/1000
  mat[i,2] = length(which(t2$Tsq_mat[1:1000] > s2))/1000 # alpha error = 정상인데 비정상으로 나오는 것
  mat[i,3] = length(which(t2$Tsq_mat[1001:2000] < s2))/1000 # beta error = 비정상인데 정상으로 나오는 것
}

# 시각화
plot(mat[,2:3], xlab = 'alpha error', ylab = 'beta error', )

################################################

# cbm 진행
# cbm input = train, test, 유의수준, knn 진행시 k 개수

# output 저장할 matrix 생성
mat2 = matrix(0,1000,3) 

# cbm 한번만 구하면 됩니다.
# cbm도 k에따른 error 구할 수 있긴한데 오래걸려서 일단 제외했습니다.
# 차주에 이야기해보죠..
s3 = cbm(df_tr, df_tr, 0.05, 5)
tft_cbm = cbm(df_tr, te, 0.05, 3)

# 유의 수준에 따른 error 확인
for(i in 1:1000){
  tft_boot = bootlimit1(s3$cbm_res, i/1000, 100)
  mat2[i,1] = i/1000
  mat2[i,2] = length(which(tft_cbm$cbm_res[1:1000]>tft_boot))/1000
  mat2[i,3] = length(which(tft_cbm$cbm_res[1001:2000]<tft_boot))/1000
}

# 시각화하면 error 값 줄어든 것이 확인 가능합니다.
points(mat2[,2:3],col='red',type='o')



####################
#### solve 풀이 ####
####################

### 위와 동일하지만 ginv가 solve으로 변경되어 있습니다.
### 과정 동일해서 주석은 생략할게요!

# Hostelling t-square solve
mat3 = matrix(0,1000,3)

t2_solve = t_square_solve(df_tr, te, i/1000) 
s1_solve = t_square_solve(df_tr, df_tr, i/1000)

for (i in 1:1000) {
  s2_solve = bootlimit1(s1_solve$Tsq_mat,i/1000,100)
  mat3[i,1] = i/1000
  mat3[i,2] = length(which(t2_solve$Tsq_mat[1:1000] > s2_solve))/1000 
  mat3[i,3] = length(which(t2_solve$Tsq_mat[1001:2000] < s2_solve))/1000 
}

points(mat3[,2:3],col='blue',type='o')

### cbm solve

mat4 = matrix(0,1000,3)

tft_cbm_solve = cbm_solve(df_tr, te, 0.05, 3)

s4 = cbm(df_tr, df_tr, 0.05, 5)

for(i in 1:1000){
  tft_boot_solve = bootlimit1(s4$cbm_res, i/1000, 100)
  mat4[i,1] = i/1000
  mat4[i,2] = length(which(tft_cbm_solve$cbm_res[1:1000]>tft_boot_solve))/1000
  mat4[i,3] = length(which(tft_cbm_solve$cbm_res[1001:2000]<tft_boot_solve))/1000
}

points(mat4[,2:3],col='green',type='o')


#####################
### 최종적인 결과 ###
#####################

plot(mat[,2:3], xlab = 'alpha error', ylab = 'beta error', main = '유의 수준에 따른 error') # hostelling t-square ginv 진행
points(mat2[,2:3],col='green',type='o') # cbm ginv 진행
points(mat3[,2:3],col='blue',type='o') # hostelling t-square solve 진행
points(mat4[,2:3],col='red',type='o') # cbm solve 진행
legend('topright', legend = c('t-square ginv','cbm ginv','t-square solve','cbm solve'), fill =  c('black', 'green','blue','red'))
legend('topright', legend = c('t-square ginv','t-square solve'), fill =  c('black', 'blue'))
