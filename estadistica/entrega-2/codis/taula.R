N <- 10000
n <- 100
taula <- ''
alfes <- numeric(n)
k <- 1
for(alfa in c(0.1, 0.2)) {
  for(nu in c(0.5, 1, 2, 4, 8)) {
    alfes[k] <- mean(alfa.moments(N, mida = n, alfa = alfa, nu = nu))
    k <- k + 1
    #taula <- paste(taula, mean(alfa.moments(N, mida = n, alfa = alfa, nu = nu)), '&')
  }
}

alfesl <- numeric(n)
for(alfa in c(0.1, 0.2)) {
  for(nu in c(0.5, 1, 2, 4, 8)) {
    alfesl[k] <- mean(alfa.moments(N, mida = n, alfa = alfa, nu = nu))
    #taula <- paste(taula, mean(alfa.moments(N, mida = n, alfa = alfa, nu = nu)), '&')
  }
}

alfes1 <- alfa.moments(1000, mida = 50, alfa = 0.1, nu = 1)

alfes2 <- alfa.moments(1000, mida = 100, alfa = 0.1, nu = 1)

alfes3 <- alfa.moments(1000, mida = 1000, alfa = 0.1, nu = 1)

biaix <- numeric(10)
mse <- numeric(10)
mides <- numeric(10)
for(k in 1:10) {
  biaix[k] = mean(alfa.moments(1000, mida = 100*k, alfa = 0.1, nu = 1)) - 0.1
  mse[k] = mean((alfa.moments(1000, mida = 100*k, alfa = 0.1, nu = 1) - 0.1)^2)
  mides[k] <- 100*k
}
plot(mides, biaix, xlab = 'mostra', ylab = 'biaix', col = 'red', pch = 16, main ='Evolució del biaix')
plot(mides, mse, xlab = 'mostra', ylab = 'biaix', col = 'red', pch = 16, main ='Evolució del MSE')

biaix1 <- numeric(10)
mse1 <- numeric(10)
for(k in 1:10) {
  biaix1[k] = mean(alfa.mle(1000, mida = 100*k, alfa = 0.1, nu = 1)) - 0.1
  mse1[k] = mean((alfa.moments(1000, mida = 100*k, alfa = 0.1, nu = 1) - 0.1)^2)
}

points(mides, biaix1, col = 'blue', pch = 16)
points(mides, mse1, col = 'blue', pch = 16)
