library(ggplot2)

df = read.csv("C:/Users/user/Desktop/Social_Network_Ads.csv")# csv파일 read

df

df[df$Gender=='Male',"Gender"] = 1
df[df$Gender=='Female',"Gender"] = 0
df = df[2:5]
df
df$Gender = as.numeric(df$Gender)

a = 0
b = 0

rate = 0.1

epochs = 50000

sigmoid = function(x){
  return(1/(1+exp(-x)))
}

x_data = df[3]
y_data = df[4]

for (i in 1:4000) {
  a_difference1 = x_data*(sigmoid(a*x_data+b)-y_data)
  b_difference2 = sigmoid(a*x_data+b)-y_data
  a = a-rate*a_difference1
  b = b-rate*b_difference2
  if (i%%2000 == 0){
    print(a)
    print(b)
  }
}

plot(a)
plot(b)
a = as.data.frame(a)
b = as.data.frame(b)
k = data.frame(a,b)
names(k) = c('x','y')
k
plot(k)
ggplot(k,aes(x=x,y=y)) + geom_point()
ggplot(k,aes(x=x,y=y)) + geom_point() + stat_smooth()

a
b
sigmoid(k)

plot(k)

t = sigmoid(a)
z = data.frame(a,t)
names(z) = c('x','y')
plot(z)
ggplot(z,aes(x=x,y=y)) + geom_point() + stat_function(fun = sigmoid)
