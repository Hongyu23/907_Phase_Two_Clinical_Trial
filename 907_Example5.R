#Data At First
a1 <- 6
a <- 18
n1 <- 20
n <- 47
b1 <- 10
MSE_MLE <- c()
MSE_UMVUE <- c()
p <- 0.2
i <- 1
a_sum <- a+a1
p_t <- c()

while (p <= 0.6) {
  p = p + 0.01
  p_t[i] <- p
  MSE_MLE[i] <- 0
  MSE_UMVUE[i] <- 0
  for (s in 0:a_sum){
    if (0 <= s & s <= a1){
      m = 1
      MSE_MLE[i] = MSE_MLE[i] + (((s/n1)-p)^2)*choose(n1,s)*(p^s)*((1-p)^(n1-s))
      MSE_UMVUE[i] = MSE_UMVUE[i] + (((s/n1)-p)^2)*choose(n1,s)*(p^s)*((1-p)^(n1-s))
    }
    if (a1 +1 <= s & s <= a1+a){
      m = 2
      if (s <= b1-1) {
        Up = s
      }
      if (s >= b1-1) {
        Up = n1
      }
      if (a1+1 <= s-(n-n1)) {
        Down = s-(n-n1)
      }
      if (a1+1 >= s-(n-n1)) {
        Down = a1+1
      }
      fmsp <- 0
      fmsp2 <- 0
      for (z in Down:Up){
        fmsp = fmsp + choose(n1,z)*choose(n-n1,s-z)
      }
      for (z in Down:Up){
        fmsp2 = fmsp2 + choose(n1-1,z-1)*choose(n-n1,s-z)
      }
      MSE_MLE[i] = MSE_MLE[i] + (((s/n)-p)^2)*fmsp*(p^s)*((1-p)^(n-s))
      MSE_UMVUE[i] = MSE_UMVUE[i] + (((fmsp2/fmsp)-p)^2)*fmsp*(p^s)*((1-p)^(n-s))
    }
  }
  i = i + 1
}

Relative <- MSE_MLE/MSE_UMVUE

plot(p_t, Relative, type="o", ylim = c(0.3, 1.3))