# Generem N mostres de mida `mida` i n'estimem els paràmetres pel mètode dels moments. Per tant estem generant nombres que segueixen la distribució de cada estimador
alfa.moments <- function(N, mida, alfa, nu) {
  alfes <- numeric(N) 
  for(k in 1:N) {
    mostra <- rgamma(mida, shape = nu, rate = alfa);
    alfes[k] <- mean(mostra)/var(mostra);
  }
  return(alfes)
}

nu.moments <- function(N, mida, alfa, nu) {
  nus <- numeric(N) 
  for(k in 1:N) {
    mostra <- rgamma(mida, shape = nu, rate = alfa);
    nus[k] <- (mean(mostra))^2/var(mostra);
  }
  return(nus)
}

