# Programem la funció de log-versemblança de perfil pel paràmetre shape donada una mostra `mostra`
log.pvers <- function(x, mostra) {
  length(mostra)*(x*log(x) - x*log(mean(mostra)) - log(gamma(x)) + (x-1)*mean(log(mostra)) - x)
}

# Generem N mostres de mida `mida` i n'estimem els paràmetres pel mètode dels moments. Per tant estem generant nombres que segueixen la distribució de cada estimador
alfa.mle <- function(N, mida, alfa, nu) {
  alfes <- numeric(N) 
  for(k in 1:N) {
    mostra <- rgamma(mida, shape = nu, rate = alfa);
    lp <- partial(log.pvers, mostra = mostra);
    nu <- optimize(lp, interval = c(0.1, 10), maximum = TRUE)$maximum
    alfes[k] <- nu/mean(mostra)
  }
  return(alfes)
}

# Generem N mostres de mida `mida` i n'estimem els paràmetres pel mètode dels moments. Per tant estem generant nombres que segueixen la distribució de cada estimador
nu.mle <- function(N, mida, alfa, nu) {
  nus <- numeric(N) 
  for(k in 1:N) {
    mostra <- rgamma(mida, shape = nu, rate = alfa);
    lp <- partial(log.pvers, mostra = mostra);
    nus[k] <- optimize(lp, interval = c(0.1, 10), maximum = TRUE)$maximum
  }
  return(nus)
}
