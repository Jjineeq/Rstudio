data = read.csv("C:/Users/User/Desktop/The_salinity_of_the_Nakdonggang_River.csv")
data = data[0:3]
data
ncol(data)

log <- function(x){
  i = 0
  k = 0
  for (i in ncol(data)+1) {
    fx = 1/(1+exp(k+i*x.format(i)))
    print(fx)
  }
}


