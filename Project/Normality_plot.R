# 데이터 불러오기
df = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph1_bytime.csv", fileEncoding = 'CP949') # train
df2 = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph2_in.csv", fileEncoding = 'CP949') # normal test
df3 = read.csv("C:\\Users\\User\\github\\data\\TFTLCD\\ph2_out.csv", fileEncoding = 'CP949') # abnormal test

# 필요 없는 데이터 제거
df_tr = df[,8:49] 
df_te = df2[,8:49] 
df_te_2 = df3[,8:49]

# normal, abnormal test bind -> total test
te = rbind(df_te, df_te_2)
tk = rbind(df_tr, te)

shapiro.test(df_tr$MP_ALLSTEP.INNER_TEMP.Mean)

qqnorm(df_tr$MP_ALLSTEP.INNER_TEMP.Mean)
qqline(df_tr$MP_ALLSTEP.INNER_TEMP.Mean)
title(paste0("sensor ",colnames(data)[i],"normality test.png"))


setwd('C:\\Users\\User\\github\\Rstudio\\Project\\Normality_test')

nrow(df_tr)

normality = function(data){
  for (i in 1:ncol(data)) {
    png(paste0("sensor ",colnames(data)[i],"normality test.png"))
    shapiro.test(df_tr[,i])
    qqnorm(df_tr[,i], main =paste0("sensor ",colnames(data)[i]," normality test"))
    qqline(df_tr[,i])
    dev.off()
  }
}
# normality(df_tr)
