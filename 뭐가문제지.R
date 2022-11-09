library(ggplot2)

df = read.csv("C:/Users/User/Desktop/Social_Network_Ads.csv")# csv파일 read

df

df[df$Gender=='Male',"Gender"] = 1
df[df$Gender=='Female',"Gender"] = 0
df = df[2:5]
df
df$Gender = as.numeric(df$Gender)

# min-max scaling
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}
df[2]=normalize(df[2])
df[3]=normalize(df[3])
df

sigmoid = function(x){
  return(1/(1+exp(-x)))
}

x_data = df[3]
y_data = df[4]
x_data
a = 0
b = 0

rate = 0.1

epochs = 50000

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

x_data # 400*1
a

v = as.matrix(x_data)%*%t(as.matrix(a))
v[,2]
b

plot(k)

t = sigmoid(a)
z = data.frame(a,t)
names(z) = c('x','y')

finish = function(z){
  for (i in 1:400) {
    if (z[i,2]>0.5) {
      result = (z[i,2]=1)
    }else result = (z[i,2]=0)
  }
}

finish(z)
z[400,2]
pred = function(x){
  if (x>0.5){
    return(1)
  }else{
    return(0)
  }
}
for (i in 1:400) {
  z[i,2] = pred(z[i,2])
}
z[2]
plot(z)
ggplot(z,aes(x=x,y=y)) + geom_point() + stat_function(fun = sigmoid)
q
