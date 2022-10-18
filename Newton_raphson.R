newton <- function(f, tol = 1e-7, x0 = 1, N = 300){
  h <- 1e-7
  i <- 1
  x1 <- x0
  p <- numeric(N)
  while(i <= N){
    df.dx <- (f(x0 + h) - f(x0)) / h
    x1 <- (x0 - (f(x0) / df.dx))
    p[i] <- x1
    i = i+1
    if(abs(x1 - x0) < tol) break
    x0 = x1
  }
  return(p[1:(i - 1)])
}

f = function(x) x^5+7*x^3+2

newton(f,x0=2)

p <- ggplot(data.frame(x = c(-2, 2)), aes(x = x)) + stat_function(fun = f, color = "blue") + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)

d <- data.frame(label = 1:length(newton(f, x0 = 2)), 
                x = newton(f, x0 = 2), 
                y = f(newton(f, x0 = 2)))

p2 <- p + geom_point(data = d, aes(x = x, y = y, frame = label)) + 
  geom_text(data = d, aes(x = x, y = y + 20, label = label, frame = label), size = 3)

p2

#install.packages("tidyverse")
#library(tidyverse)
