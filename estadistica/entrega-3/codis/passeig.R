passeig <- function(n, inicial, mu, sigma) {
  cumsum(c(inicial, rnorm(n - 1, mu, sigma)))
}
preu.inicial <- 100
dies <- (1:500)
N <- 10
passejos <- matrix(nrow = N, ncol = length(dies))
for (k in 1:N) { passejos[k,] <- passeig(length(dies),
                                         log(preu.inicial),
                                         mean(rend.CAT), 
                                         sd(rend.CAT))
}

passeig.final <- function(n, inicial, mu, sigma) {
  sum(c(inicial), rnorm(n-1, mu, sigma))
}
fav <- 0
for(k in 1:10000) { if(passeig.final(62, log(100), mean(rend.CAT), sd(rend.CAT)) < log(100))
  fav <- fav+1
}
fav/10000
