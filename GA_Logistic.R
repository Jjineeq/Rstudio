library(GA)

df = read.csv("C:/Users/User/Desktop/Social_Network_Ads.csv")# csv파일 read

df

#label encoding
df[df$Gender=='Male',"Gender"] = 1
df[df$Gender=='Female',"Gender"] = 0
df = df[2:5]
df
df$Gender = as.numeric(df$Gender)
df$EstimatedSalary = as.data.frame(df$EstimatedSalary)
df[2:3]

names(df)

OLS <- function(data, b0, b1, b2, b3){
  
  attach(data, warn.conflicts=F)
  
  # Y_hat <- b0  + b1*Gender + b2*Age + b3*EstimatedSalary
  Y_hat <- 1/(1+exp(-(b0  + b1*Gender + b2*Age + b3*EstimatedSalary)))
  
  SSE = t(Purchased-Y_hat) %*% (Purchased-Y_hat) 
  
  detach(data)
  
  return(SSE)
  
}

ga.OLS <- ga(type='real-valued', min=c(-100,-100, -100,-100), 
             max=c(100, 100, 100,100), popSize=500, maxiter=500, names=c('beta','Gender', 'Age', 'EstimatedSalary'),
             keepBest=T, fitness = function(b) -OLS(df, b[1],b[2], b[3], b[4]))

ga.model <- summary(ga.OLS)
ga.model

