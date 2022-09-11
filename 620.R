install.packages("installr")
library("installr")

check.for.updates.R()

install.R()
version


read.csv("C:/Users/jang/Documents/data.csv",sep=",", header=FALSE, fill=TRUE)


frame = read.csv("C:/Users/jang/Documents/data.csv")

frame

head(frame)
summary(frame)

install.packages("e1071")
library("e1071")

train = svm(1:,300)
(sv = svm(frame$Temp, data = frame, subset = ))