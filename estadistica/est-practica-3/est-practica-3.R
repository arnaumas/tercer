# Paràmetres de la distribució gamma que simulem
al <- 0.2
nu <- 1
N <- 100

mostra <- rgamma(N, shape = nu, scale = 1/al)

#Estimem pel mètode dels moments
al.moments <- mean(mostra)/var(mostra)
nu.moments <- mean(mostra)^2/var(mostra)

#Repetim això diverses vegades
N.rep <- 1000
al.moments <- nu.moments <- numeric(N.rep)

for(k in 1:N.rep) {
  mostra <- rgamma(N, shape = nu, scale = 1/al);
  al.moments[k] <- mean(mostra)/var(mostra);
  nu.moments[k] <- mean(mostra)^2/var(mostra);
}

hist(nu.moments)
abline(v = mean(nu.moments), col = 'red')
abline(v = nu, col = 'blue')

hist(al.moments)
abline(v = mean(al.moments), col = 'red')
abline(v = al, col = 'blue')

#Repetim amb una mostra més gran
N <- 10000
for(k in 1:N.rep) {
  mostra <- rgamma(N, shape = nu, scale = 1/al);
  al.moments[k] <- mean(mostra)/var(mostra);
  nu.moments[k] <- mean(mostra)^2/var(mostra);
}

hist(nu.moments)
abline(v = mean(nu.moments), col = 'red')
abline(v = nu, col = 'blue')

hist(al.moments)
abline(v = mean(al.moments), col = 'red')
abline(v = al, col = 'blue')

#Automatizem l'estimació pel mètode dels moments
moments.gamma <- function(mostra) {
  c(alfa = mean(mostra)/var(mostra), nu = mean(mostra)^2/var(mostra))
}


#Busquem els intervals de confiança de Wald
#Informació de fisher
inf <- function(alfa, nu, N) {
  N * cbind(c(nu/alfa^2, - 1/alfa), c(- 1/alfa, trigamma(nu)))
}

# Intervals de confiança pels paràmetres
conf <- function(alfa, nu, N) {
  c(conf.alfa = sqrt(solve(inf(alfa, nu, N))[1, 1]), conf.nu = sqrt(solve(inf(alfa, nu, N))[2, 2]))
}

# El paquet MASS fa màxima versemblança
library('MASS')
fitdistr(mostra, "gamma")

#Repetim amb màxima versemblança
N <- 100
al.mle <- nu.mle <- numeric(N.rep)
for(k in 1:N.rep) {
  mostra <- rgamma(N, shape = nu, scale = 1/al);
  estimacio = fitdistr(mostra, "gamma")$estimate;
  al.mle[k] <- estimacio['rate'];
  nu.mle[k] <- estimacio['shape']
}

hist(nu.mle)
abline(v = mean(nu.mle), col = 'red')
abline(v = nu, col = 'blue')

hist(al.mle)
abline(v = mean(al.mle), col = 'red')
abline(v = al, col = 'blue')
