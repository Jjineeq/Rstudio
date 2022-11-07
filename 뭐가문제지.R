df = read.csv("C:/Users/jangs/Social_Network_Ads.csv")# csv파일 read

df

df[df$Gender=='Male',"Gender"] = 1
df[df$Gender=='Female',"Gender"] = 0
df = df[2:5]
df
df$Gender = as.numeric(df$Gender)

a = 0
b = 0

rate = 2

epochs = 4000

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

x_data
a
b
a = as.data.frame(a)
b = as.data.frame(b)

plot(a)
plot(b)

typeof(x_data)
x_data = as.data.frame(x_data)

x = transform(x_data[1]) 
plot(1:400,for(i in 1:400) return(print(sigmoid(a*i+b))))

sigmoid(a*400+b)

k = for(i in 1:400){
  return(sigmoid(a*i+b))
} 

plot(1:400,for(i in 1:400)sigmoid(a*400+b))

plot

nrow(sigmoid(a*2+b))
ncol(sigmoid(a*2+b))
plot(1:400,sigmoid(a*2+b))

for(i in 1:400)print(sigmoid(a*400+b))

sigmoid(a*1+b)
a = as.data.frame(a)
b = as.data.frame(b)
typeof(a)
typeof(b)

plot(a)
plot(b)
