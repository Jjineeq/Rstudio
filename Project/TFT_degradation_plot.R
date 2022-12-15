## DegradationModel 적용후
## 열화정도 plot
library(pracma)
library(MASS)
library(reshape)

df = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph1_bytime.csv", fileEncoding = 'CP949') # train
df2 = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph2_in.csv", fileEncoding = 'CP949') # normal test
df3 = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph2_out.csv", fileEncoding = 'CP949') # abnormal test

df_tr = df[,8:49] # 필요 정보만 select
df_te = df2[,8:49] # 필요 정보만 select
df_te_2 = df3[,8:49] # 필요 정보만 select

df2[df2$TOOL_NAME == 'P8TCVD060306','TOOL_NAME'] = 'Tool1'
df2[df2$TOOL_NAME == 'P8TCVD060307','TOOL_NAME'] = 'Tool2'
df2[df2$TOOL_NAME == 'P8TCVD060308','TOOL_NAME'] = 'Tool3'
df2[df2$TOOL_NAME == 'P8TCVD060309','TOOL_NAME'] = 'Tool4'

te = rbind(df_te, df_te_2)
all = rbind(df_tr, te)
all = cbind(df2[,1],all)
all = cbind(df2[,6],all)

head(all)
colnames(all)
all = rename(all,c('df2[, 1]' = 'TOOL_NAME'))
all = rename(all,c('df2[, 6]' = 'TIME_STAMP'))

write.csv(all,"C:\\Users\\User\\github\\data\\TFTLCD\\TFT_EDA_ALL.csv")


which(all$`df2[, 1]` == 'Tool1')
which(all$`df2[, 1]` == 'Tool2')
which(all$`df2[, 1]` == 'Tool3')
which(all$`df2[, 1]` == 'Tool4')



setwd('C:\\Users\\User\\github\\Rstudio\\Project')

for (i in 2:43) {
  train = as.matrix(df_tr[,i])
  test = as.matrix(all[,i])
  msetLR = mset_regress(train, test)
  trDegradation = degradation_model(msetLR$residual_tr)
  tsDegradation = degradation_model(msetLR$residual_ts)
  png(paste0("sensor ",colnames(df_tr)[i]," all.png"))
  plot(tsDegradation)
  dev.off()
}
