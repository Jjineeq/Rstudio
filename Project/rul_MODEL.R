source("C:/Users/User/github/R_function/curve_model.R")
source("C:/Users/User/github/R_function/RUL.R")
source("C:/Users/User/github/R_function/RUL_information_text.R")
source("C:/Users/User/github/R_function/msetRegression.R")
source("C:/Users/User/github/R_function/.R")


df = read.csv("C:/Users/User/github/data/rul_hrs/rul_hrs.csv")
nrow(df)
ncol(df)

head(df)

plot(df['rul'])

nrow(df)*0.7

# train, test set나눔
x_train = df[1:nrow(df)*0.7 ,1:51]
y_train = df[1:nrow(df)*0.7 ,52]
x_test = df[nrow(df)*0.7+1:nrow(df) ,1:51]
y_test = df[nrow(df)*0.7:nrow(df) ,52]

# MSET linear regression
msetLR = mset_regress(x_train, y_train)

RUL(x_train,y_train, x_test, y_test)
