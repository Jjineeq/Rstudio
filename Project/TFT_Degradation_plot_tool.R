## TFT_Degragation by Tool
## Tool별로 열화 정도를 나타내는 Plot찍기 

library(pracma)
library(MASS)

df1 = read.csv("C:\\Users\\User\\github\\Rstudio\\Project\\Tool1_all.csv",, fileEncoding = 'CP949') 
df2 = read.csv("C:\\Users\\User\\github\\Rstudio\\Project\\Tool2_all.csv", fileEncoding = 'CP949') 
df3 = read.csv("C:\\Users\\User\\github\\Rstudio\\Project\\Tool3_all.csv", fileEncoding = 'CP949') 
df4 = read.csv("C:\\Users\\User\\github\\Rstudio\\Project\\Tool4_all.csv", fileEncoding = 'CP949') 

df1_train = df1[1:636,2:43] # tool1 정상 구간
df2_train = df2[1:398,2:43] # tool2 정상 구간
df3_train = df3[1:574,2:43] # tool3 정상 구간
df4_train = df4[1:391,2:43] # tool4 정상 구간

df1_test = df1[,2:43] # tool1 전체 구간
df2_test = df2[,2:43] # tool2 전체 구간
df3_test = df3[,2:43] # tool3 전체 구간
df4_test = df4[,2:43] # tool4 전체 구간

# setwd('C:\\Users\\User\\github\\Rstudio\\Project\\TFT_Degradation_plot_tool')
# 
# de = function(train, test){
#   for (i in 1:42) {
#     train_ = as.matrix(train[,i])
#     test_ = as.matrix(test[,i])
#     msetLR = mset_regress(train_, test_)
#     trDegradation = degradation_model(msetLR$residual_tr)
#     tsDegradation = degradation_model(msetLR$residual_ts)
#     png(paste0("tool 4 sensor ",colnames(df_tr)[i]," all.png"))
#     plot(tsDegradation)
#     dev.off()
#   }
# }