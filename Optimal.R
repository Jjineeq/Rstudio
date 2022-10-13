# 최적화

g_desc = function(fp,x,h=1e-2, tol = 1e-4, m=1e3){
  iter = 0
  oldx = x
  x = x -h*fp(x) # initial gradient만큼 이동
  while (abs(x-oldx)>tol){
    iter = iter +1
    if (iter > m) stop("max iteration")
    oldx = x
    x = x -h*fp(x) # 부호가 +라면 gradient ascent
  }
  return (x)
}


f <-readline(prompt="Function? ") # 수식입력
f = parse(text = f) # 수식을 expression으로
fd = D(f,'x') #text형태로 된 수식 미분0

fd = function(x) 0.25 * (4 * x^3) + 3 * x^2 - 1
# 미분식을 활용 function으로 다시 만듬

Res = g_desc(fd,-1)
plot(fd,-5,2)
abline(v=c(-1), lty =2, col ="blue")
abline(v=c(Res), lty =2, col ="red")

res = g_desc(fd,2)
abline(v=c(2), lty =2, col ="blue")
abline(v=c(res), lty =2, col ="red")


print("--------------------")


# f = function(x) 0.25*x^4+x^3-x-1
# plot(f,main="f(x) = 0.25*x^4+x^3-x-1",xlim =c(-5,2),ylim = c(-5,5))
# abline(v=c(-1), lty =2, col ="blue")
# abline(v=c(Res), lty =2, col ="red")
# 
# res = g_desc(fd,2)
# abline(v=c(2), lty =2, col ="blue")
# abline(v=c(res), lty =2, col ="red")
