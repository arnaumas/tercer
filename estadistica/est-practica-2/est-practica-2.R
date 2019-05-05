# Instruccions ----
# Definició de funcions:
# <nom> <- function(<args>) { <cos> }

# I/O
# getwd() pel directori actual, setwd(<path>) per canviar de directori
# per lllegir data frames: read.table("<path>"), header decideix si es llegeixen els noms de les columnes de la primera fila
# save(list = , file = ) i load(file = ) per llegir dades en binari

# Resolem equacions de versemblança
# Fent servir el mètode de Newton i notació estadística
# a[n] = a[n - 1] + J^-1 * S
# on J és la matriu d'informació de Fisher i S l'score


# Problema 1 ----
# Calcular la log-versemblança d'una binomial amb n = 50, i paràmetre p 
# Calculem la log-versemblança, l'score i la funció de Fisher
lv <- function(p, n, k) {k*log(p) + (n - k)*log(1 - p) + lchoose(n,k)}
S <- function(p, n, k) {k/p - (n - k)/(1 - p)}
J <- function(p, n, k) {k/p^2 + (n - k)/(1 - p)^2}

# L'iterat de Newton és 
NR <- function(x) {x + S(x, 50, 4) / J(x, 50, 4)}

# Fem un dibuix de la log-versemblança
plot(Curry(lv, n = 50, k = 4), from = 0, to = 0.3, main = "Log-versemblança d'una binomial", xlab = "p", ylab = "l(p)")

# Iterem unes quantes vegades
lb <- Curry(lv, n = 50, k = 4)
m <- 0.15
points(m, lb(m), col = "red")
m <- NR(m)

abline(v = m, col = "red")

# La manera numèrica és la funció optimize
optimize(lb, c(0, 0.3), maximum = TRUE)$maximum




# Problema 2 ----
# Distribucions Gamma
# Primer dibuixem algunes densitats
g1 = Curry(dgamma, shape = 1, rate = 1/5)
g2 = Curry(dgamma, shape = 2, rate = 1/5)
g3 = Curry(dgamma, shape = 3, rate = 1/5)

plot(g1, from = 0, to = 25, main = "Diverses distribucions Gamma", xlab = "x", ylab = "f(x)", col = "blue")
plot(g2, from = 0, to = 25, col = "red", add = T)
plot(g3, from = 0, to = 25, col = "green", add = T)

legend('topright', legend = c('shape = 1, rate = 0.2', 'shape = 2, rate = 0.2', 'shape = 3, rate = 0.2'), col = c('blue', 'red', 'green'), lty = 1)

# Descarreguem unes dades del paquet 'evir'
data("nidd.thresh", package = "evir")
hist(nidd.thresh)
boxplot(nidd.thresh, col = "blue", horizontal = "T")

# Desplacem les dades per a què comencin a 0
xdat <- nidd.thresh - 65
hist(xdat, probability = T, col = "blue")

# Volem ajustar aquestes dades a una distribució Gamma, és a dir, volem estimar els paràmetres alfa i nu
# Amb el mètode dels moments trobem llavors per a fer la iteració de Newton
a = mean(xdat)/var(xdat)
n = mean(xdat)^2/var(xdat)

# La log-versemblança de la distribució gamma és
l <- function(x,y, dat) {length(dat) * (-x*mean(dat) + (y - 1)*mean(log(dat)) - log(gamma(y)) + y*log(x))}
vers = Curry(l, dat = xdat)

# Com que l'estimador d'alfa és senzill, trobem la profile likelihood per la nu
lp <- function(x, dat) {length(dat) * (-x + (x - 1)*mean(log(dat)) - log(gamma(x)) + x*(log(x) - log(mean(dat))))}
provers = Curry(lp, dat = xdat)


# Busquem el màxim d'aquesta funció
nu = optimize(provers, interval = c(0.1, 2), maximum = TRUE)$maximum
alfa = nu / mean(xdat)

plot(provers, from = 0.1, to = 3, main = "Log-versemblança per el paràmetre nu", xlab = "n", ylab = "l(n)")
abline(v = nu, col = "red")

# Comparem l'histograma amb la distribució obtinguda
plot(Curry(dgamma, shape = nu, rate = alfa), from = 0, to = 250, col = 'red', add = T)
plot(Curry(dgamma, shape = n, rate = a), from = 0, to = 250, col = 'green', add = T)
legend('topright', col = c('red', 'green'), legend = c('Distribució gamma amb les estimacions de màxima versemblança', 'Distribució gamma amb les estimacions del mètode dels moments'), lty = 1)
