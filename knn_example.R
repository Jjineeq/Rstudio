data = iris

km = kmeans(iris[,1:4],3)

nrow(iris)
ncol(iris)

km$centers

dist1 = sqrt(sum((data[1,1:4] - km$centers[1,])^2))
dist2 = sqrt(sum((data[1,1:4] - km$centers[2,])^2))
dist3 = sqrt(sum((data[1,1:4] - km$centers[3,])^2))

dist1

min(dist1,dist2,dist3)

iris[1:4,]
iris
tr = x[1:nrow(iris)*0.7 , ]
ts = x[nrow(iris)*0.7+1:nrow(iris) , ]

knn = function(tr,ts,k){
  km = kmeans(tr, k)
  t = matrix(0,ncol(ts),1)
  
  for (j in 1:nrow(x)) {
    for (i in 1:ncol(x)-1) {
      t[j,1] = sqrt(sum((ts[j,] - km$centers[i,])^2))
    }
  }
  return(min(t))
}

knn(tr,ts,3)


