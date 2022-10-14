z = matrix(c(1,2,3,4), ncol=2)
z
det(z)

t = matrix(c(4,5,6,7), ncol=2)
t
det(t)
s = t(t)
s

q = t%*%s
det(q)

k = matrix(c(1,2,3,4))
k
j = t(k)
j
n = k%*%j
n
det(n)
