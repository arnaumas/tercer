# Definim els estimadors del mètode dels moments
moments.gamma <- function(mostra) {
  c(alfa = mean(mostra)/var(mostra), nu = mean(mostra)/var(mostra)^2)
}

# Generem N.rep mostres de mida N i n'estimem els paràmetres pel mètode dels moments. Per tant estem generant nombres que segueixen la distribució de cada estimador
alfa.moments <- function(N.reps, N, alfa, nu) {
  alfes <- numeric(N.reps) 
  for(k in 1:N.reps) {
    mostra <- rgamma(N, shape = nu, rate = alfa);
    alfes[k] <- as.numeric(moments.gamma(mostra)['alfa'])
  }
  return(alfes)
}

nu.moments <- function(N.reps, N, alfa, nu) {
  nus <- numeric(N.reps) 
  for(k in 1:N.reps) {
    mostra <- rgamma(N, shape = nu, rate = alfa);
    nus[k] <- as.numeric(moments.gamma(mostra)['nu'])
  }
  return(nus)
}

