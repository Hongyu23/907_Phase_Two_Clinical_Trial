# Data From Simon's Method
Alpha <- 0.1
Beta <- 0.9
a1 <- 6
a <- 18
n1 <- 20
n2 <- 27
p0 <- 0.3
p1 <- 0.5
b1 <- 10

p <- c()
Bias_MLE <- c()
k <- 0
i <- 0.2

while (i <= 0.6){
  i <- i + 0.01
  k = k+1
  p[k] <- i
  Bias_MLE[k] <- 0
  for (j in (a1+1):b1-1){
    Bias_MLE[k] = Bias_MLE[k] + ((j-n1*p[k])*(p[k]^j)*(1-p[k])^(n1-j)*choose(n1,j))  
  }
  Bias_MLE[k] = (-n2/(n1*(n1+n2)))*Bias_MLE[k]
}

plot(p, Bias_MLE, type="o", ylim = c(-0.03, 0.03))
