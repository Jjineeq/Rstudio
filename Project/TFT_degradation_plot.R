## DegradationModel 적용후
## 열화정도 plot
df = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph1.csv", fileEncoding = 'CP949') # train
df2 = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph2.csv", fileEncoding = 'CP949') # normal test
df3 = read.csv("C:/Users/user/github/Anomaly-Detection/data/ph2_out.csv", fileEncoding = 'CP949') # abnormal test

df_tr = df[,8:49] # 필요 정보만 select
df_te = df2[,8:49] # 필요 정보만 select
df_te_2 = df3[,8:49] # 필요 정보만 select
ncol(df_tr)

te = rbind(df_te, df_te_2)
all = rbind(df_tr, te)

library(pracma)
library(MASS)

setwd('C:\\Users\\User\\github\\Rstudio\\Project')
for (i in 1:42) {
  train = as.matrix(df_tr[,i])
  test = as.matrix(all[,i])
  msetLR = mset_regress(train, test)
  trDegradation = degradation_model(msetLR$residual_tr)
  tsDegradation = degradation_model(msetLR$residual_ts)
  png(paste0("sensor ",colnames(df_tr)[i]," all.png"))
  plot(tsDegradation)
  dev.off()
}
