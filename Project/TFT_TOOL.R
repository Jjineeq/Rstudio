## tool별로 t2 진행
library(MASS)
library(pracma)

df = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph1_bytime.csv", fileEncoding = 'CP949') # train
df2 = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph2_in.csv", fileEncoding = 'CP949') # normal test
df3 = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph2_out.csv", fileEncoding = 'CP949') # abnormal test

df4 = rbind(df2,df3)

train1 = as.matrix(df[df$TOOL_NAME == 'Tool1', 9:49])
train2 = as.matrix(df[df$TOOL_NAME == 'Tool2', 9:49])
train3 = as.matrix(df[df$TOOL_NAME == 'Tool3', 9:49])
train4 = as.matrix(df[df$TOOL_NAME == 'Tool4', 9:49])

test1 = as.matrix(df4[df4$TOOL_NAME == 'P8TCVD060306', 9:49])
test2 = as.matrix(df4[df4$TOOL_NAME == 'P8TCVD060307', 9:49])
test3 = as.matrix(df4[df4$TOOL_NAME == 'P8TCVD060308', 9:49])
test4 = as.matrix(df4[df4$TOOL_NAME == 'P8TCVD060309', 9:49])


tr_1 = t_square_solve(train1, train1, 0.05)
tr_2 = t_square_solve(train2, train2, 0.05)
tr_3 = t_square_solve(train3, train3, 0.05)
tr_4 = t_square_solve(train4, train4, 0.05)

tr_1_ = t_square_solve(train1, test1, 0.05) #1103 624
tr_2_ = t_square_solve(train2, test2, 0.05) #
tr_3_ = t_square_solve(train3, test3, 0.05)
tr_4_ = t_square_solve(train4, test4, 0.05)

plot(tr_1_$Tsq_mat, type = 'o', ylim = c(0,600))
abline(h = c(tr_1$CL), col = 'red')
abline(v = c(334), col = 'blue')


plot(tr_2_$Tsq_mat, type = 'o', ylim = c(0,600)) 
abline(h = c(tr_2$CL), col = 'red')
abline(v = c(210), col = 'blue')

plot(tr_3_$Tsq_mat, type = 'o', ylim = c(0,600)) 
abline(h = c(tr_3$CL), col = 'red')
abline(v = c(287), col = 'blue')

plot(tr_4_$Tsq_mat, type = 'o', ylim = c(0,600)) 
abline(h = c(tr_4$CL), col = 'red')
abline(v = c(197), col = 'blue')

i=1
s1 = t_square_solve(train1, train1, 0.05)
t2 = t_square_solve(train1, test1, 0.05) # 624 479

mat_1 = matrix(0,1000,3)
for (i in 1:1000) {
  s2 = bootlimit1(s1$Tsq_mat,i/1000,100) # boostrap은 정상에 대해 진행해야 됨
  mat_1[i,1] = i/1000
  mat_1[i,2] = length(which(t2$Tsq_mat[1:334] > s2))/334 # alpha error = 정상인데 비정상으로 나오는 것
  mat_1[i,3] = length(which(t2$Tsq_mat[334:479] < s2))/145 # beta error = 비정상인데 정상으로 나오는 것
}

plot(mat_1[,2:3], xlab = 'alpha error', ylab = 'beta error', )

s1_2 = t_square_solve(train2, train2, 0.05)
t2_2 = t_square_solve(train2, test2, 0.05) # 370 244 

mat_2 = matrix(0,1000,3)
for (i in 1:1000) {
  s2 = bootlimit1(s1_2$Tsq_mat,i/1000,100) # boostrap은 정상에 대해 진행해야 됨
  mat_2[i,1] = i/1000
  mat_2[i,2] = length(which(t2_2$Tsq_mat[1:210] > s2))/210 # alpha error = 정상인데 비정상으로 나오는 것
  mat_2[i,3] = length(which(t2_2$Tsq_mat[210:244] < s2))/34 # beta error = 비정상인데 정상으로 나오는 것
}
mat_2
plot(mat_2[,2:3], xlab = 'alpha error', ylab = 'beta error',type = 'o' )

s1_3 = t_square_solve(train3, train3, 0.05)
t2_3 = t_square_solve(train3, test3, 0.05) # 612 794

mat_3 = matrix(0,1000,3)
for (i in 1:1000) {
  s2 = bootlimit1(s1_3$Tsq_mat,i/1000,100) # boostrap은 정상에 대해 진행해야 됨
  mat_3[i,1] = i/1000
  mat_3[i,2] = length(which(t2_3$Tsq_mat[1:287] > s2))/287 # alpha error = 정상인데 비정상으로 나오는 것
  mat_3[i,3] = length(which(t2_3$Tsq_mat[287:794] < s2))/507 # beta error = 비정상인데 정상으로 나오는 것
}

plot(mat_3[,2:3], xlab = 'alpha error', ylab = 'beta error', )


s1_4 = t_square_solve(train4, train4, 0.05)
t2_4 = t_square_solve(train4, test4, 0.05) # 394 483

mat_4 = matrix(0,1000,3)
for (i in 1:1000) {
  s2 = bootlimit1(s1_4$Tsq_mat,i/1000,100) # boostrap은 정상에 대해 진행해야 됨
  mat_4[i,1] = i/1000
  mat_4[i,2] = length(which(t2_4$Tsq_mat[1:197] > s2))/197 # alpha error = 정상인데 비정상으로 나오는 것
  mat_4[i,3] = length(which(t2_4$Tsq_mat[198:483] < s2))/286 # beta error = 비정상인데 정상으로 나오는 것
}

plot(mat_4[,2:3], xlab = 'alpha error', ylab = 'beta error', )



plot(mat_1[,2:3],type = 'o', xlab = 'alpha error', ylab = 'beta error', main = '유의 수준에 따른 error') 
points(mat_2[,2:3],col='green',type='o') 
points(mat_3[,2:3],col='orange',type='o') 
points(mat_4[,2:3],col='purple',type='o')
points(mat2[,2:3],col='red',type='o')
points(mat4[,2:3],col='blue',type='o')
points(x=c(0,1), y=c(0,1), type ='l')
legend('topright', 
       legend = c('TOOL1','TOOL2','TOOL3','TOOL4','T-square','CBM'), 
       fill =  c('black', 'green','orange','purple','red','blue'))
