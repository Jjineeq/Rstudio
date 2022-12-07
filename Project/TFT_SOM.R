
df = read.csv("C:/Users/jangs/OneDrive/Documents/GitHub/Anomaly-Detection/data/ph1.csv", fileEncoding = 'CP949') # train
df2 = read.csv("C:/Users/jangs/OneDrive/Documents/GitHub/Anomaly-Detection/data/ph2.csv", fileEncoding = 'CP949') # normal test
df3 = read.csv("C:/Users/jangs/OneDrive/Documents/GitHub/Anomaly-Detection/data/ph2_out.csv", fileEncoding = 'CP949') # abnormal test

head(df)

df_tr = df[,8:49] # 필요 정보만 select
df_te = df2[,8:49] # 필요 정보만 select
df_te_2 = df3[,8:49] # 필요 정보만 select

# df_tr = cbind(df[1], df_tr)
# df_te = cbind(df[1], df_te)
# df_te_2 = cbind(df[1], df_te_2)

te = rbind(df_te, df_te_2)


#install.packages("kohonen")

library(kohonen)
#grid에서 topology 방법은 rectangular or hexagonal

#hexagonal
som_tft = som(scale(df_tr), grid = somgrid(3,3, 'hexagonal'), rlen = 100, alpha = c(0.05,0.01), keep.data = TRUE)
# alpha = 학습률, 벡터 = 변화의 양 표현, rlen 업데이트 0.05에서 0.01으로 감소

summary(som_tft)
# 맵 간의 거리는 16.149

plot(som_tft, type = 'counts')
plot(som_tft, type = 'quality')
plot(som_tft, type = 'mapping')

#rectangular
som_tft = som(scale(df_tr), grid = somgrid(6,6, 'rectangular'), rlen = 100, alpha = c(0.05,0.01), keep.data = TRUE)
plot(som_tft)
