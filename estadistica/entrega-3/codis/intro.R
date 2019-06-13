library(quantmod)
library(fBasics)
library(tikzDevice)
getSymbols("^DJI",from="2015-01-01",to="2018-12-31")
getSymbols("CAT",from="2015-01-01",to="2018-12-31")

# Preus i rendibilitats ----
preus.CAT <- CAT$CAT.Close
preus.DJI <- DJI$DJI.Close
rendibilitat <- function(preus) {
  diff(log(preus))[-1]
}
rend.CAT <- rendibilitat(preus.CAT)
rend.DJI <- rendibilitat(preus.DJI)

# Sèrie de preus ----
tikz(file = "../figs/serie-preus.tex", width = 6, height = 3)
serie <- plot(preus.CAT, main = "Evolució del preu de tancament", col = 'blue')
serie <- addPanel(rendibilitat, type = 'h', method = 'discrete', col = 'red')
plot(serie)
dev.off()

# Obertures ----
getSymbols("CAT",from="2000-01-01",to="2019-01-07")
ober.CAT <- CAT$CAT.Close
anys <- endpoints(ober.CAT, "years")[-length(anys) - 1] + 1
ober.CAT <- tanc.CAT[anys]
tikz(file = "../figs/inici.tex", width = 6, height = 1.7)
plot(ober.CAT, main = "Preus d'obertura entre 2000 i 2019", col = 'blue')
dev.off()

# Evolució d'una inversió de $100 ----
tikz(file = "../figs/inversio.tex", width = 6, height = 2)
inv <- plot(100*preus.CAT/as.numeric(preus.CAT[1]), main = "Evolució d'una inversió de \\$100", col = 'blue')
inv <- lines(100*preus.DJI/as.numeric(preus.DJI[1]), col = 'red', lwd = 2)
inv <- addLegend("topleft", legend.names = c("CAT", "DJI"), 
          lty=c(1, 1), lwd=c(2, 2),
          col=c("blue", "red"))
plot(inv)
dev.off()

# Distribució de les rendibilitats ----
volat.CAT <- sqrt(250)*sd(rend.CAT)
mitj.CAT <- 250*mean(rend.CAT)
n.dades <- length(rend.CAT)

tikz(file = "../figs/dist-rend.tex", width = 3.5, height = 3.5)
nf <- layout(mat = matrix(c(2,1), 2, 1, byrow = TRUE), height = c(3,1))
par(mar = c(3,4,0,0))
boxplot(as.numeric(rend.CAT), horizontal = TRUE, frame = FALSE,
        col = 'lightblue', whiskcol = 'black', staplecol = 'black', 
        border = 'white', medcol = 'black', medlwd = 1)
par(mar = c(0,4,3,0))
hist(rend.CAT, ylim = c(0,30), breaks = 12, xlab = "", ylab = "Densitat", 
     xaxt = 'n', probability = TRUE, col = 'lightblue', border = FALSE,
     main = "Distribució de les rendibilitats diàries")
curve(dnorm(x, mean = mean(rend.CAT), sd = sd(rend.CAT)), 
      add = TRUE, col = 'orchid', lty = 3, lw = 3)
curve(dnig(x, mu = mean(rend.CAT), delta = sqrt(phi*w), alpha = sqrt(w/phi), beta = 0),
      add = TRUE, col = 'limegreen', lty = 3, lw = 3)
legend("topleft", inset = 0.03, box.lty = 0, 
       col = c("orchid", "limegreen"), legend = c("Normal", "NIG"), 
       lty = 3, lw = 4, border = FALSE)
dev.off() 

# Test per a normalitat de les rendibilitats en un període t ----
rend.periodica <- function(rends, periode) {
  M <- matrix(as.numeric(rends), nrow = length(rends)/periode, ncol = periode, byrow = TRUE)
  return(apply(M, 1, sum))
}
# Rendibilitats setmanals
rend.set.CAT <- rend.periodica(rend.CAT[-(1:4)], 5)
kurtosi.set <- kurtosis(rend.set.CAT)
# Rendibilitats mensuals
rend.mes.CAT <- rend.periodica(rend.CAT[-(1:4)], 20)
kurtosi.mes <- kurtosis(rend.mes.CAT)
kurtosi.dia <- kurtosis(rend.CAT)

jarqueberaTest(rend.CAT)

# Comparació amb les distribucions normal i NIG ----
library(GeneralizedHyperbolic)

phi <- as.numeric(var(rend.CAT))
w <- as.numeric(3/(kurtosi.dia))
ord <- sort(as.numeric(rend.CAT))
prob <- (1:length(rend.CAT))/length(rend.CAT)
norm <- pnorm(ord, mean(rend.CAT), sd(rend.CAT))
nig <- pnig(ord, mu = mean(rend.CAT), delta = sqrt(phi*w), alpha = sqrt(w/phi), beta = 0)

# Comparació amb una NIG
tikz(file = "../figs/nig.tex", width = 5, height = 2.5)
nf <- layout(mat = matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(2.5,2.5))
par(mar = c(4,4,2,0.5))
qqnig(rend.CAT, main = "QQ-plot amb una NIG", xlab = "Quantils NIG", ylab = "Quantils mostrals", mu = mean(rend.CAT), delta = sqrt(phi*w), alpha = sqrt(w/phi), beta = 0, pch = 4, col = 'gray40', line = FALSE)
abline(0,1, lty = 'dashed', col = 'red', lw = 3)
plot(prob, nig, main = "PP-plot amb una NIG", 
     xlab = "$F_n(x)$", ylab = "$F(x)$",
     pch = 4, col = 'gray40')
abline(0,1, lty = 'dashed', col = 'red', lw = 3)
dev.off()

# Comparació amb una normal
tikz(file = "../figs/normal.tex", width = 5, height = 2.5)
nf <- layout(mat = matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(2.5,2.5))
par(mar = c(4,4,2,0.5))
qqplot(qnorm((1:length(rend.CAT)/length(rend.CAT)), mean(rend.CAT), sd(rend.CAT)), ord,
             main = "QQ-plot amb una normal", 
             xlab = "Quantils normals", ylab = "Quantils mostrals",
             pch = 4, col = 'gray40')
abline(0,1, lty = 'dashed', col = 'red', lw = 3)
plot(prob, norm, main = "PP-plot amb una normal", 
     xlab = "$F_n(x)$", ylab = "$F(x)$",
     pch = 4, col = 'gray40')
abline(0,1, lty = 'dashed', col = 'red', lw = 3)
dev.off()


# Simulació ----
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

tikz(file = "../figs/simulacio.tex", width = 6, height = 3)
nf <- layout(mat = matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(2.5,2.5))
par(mar = c(4,4,2,0.5))

confiança <- function(t, inicial, mu, sd, signe) {
  log(inicial) + mu*t + signe*qnorm(1 - 0.05/2)*sqrt(t)*sd
}
library(colorRamps)
colors <- blue2green(10)

ultims <- order(passejos[,length(dies)])
curve(confiança(x, preu.inicial, mean(rend.CAT), sd(rend.CAT), 1),
      main= "Evolució dels log-preus", 
      xlab = "Dies", ylab = "$p_t$", 
      ylim = c(4,5.5), xlim = c(0,500), 
      lty = 'dashed', col = 'magenta')
curve(confiança(x, preu.inicial, mean(rend.CAT), sd(rend.CAT), -1),
      add = TRUE,
      lty = 'dashed', col = 'magenta')
for(k in 1:N) { lines(dies, passejos[ultims[k],], type = "l", col = colors[k])}
curve(confiança(x, preu.inicial, mean(rend.DJI), sd(rend.DJI), -1),
      add = TRUE,
      lty = 'dashed', col = 'red')
curve(confiança(x, preu.inicial, mean(rend.DJI), sd(rend.DJI), 1),
      add = TRUE,
      lty = 'dashed', col = 'red')
legend("topleft", legend = c("CAT", "DJI"), 
       lty="dashed", lwd=c(2, 2),
       col=c("magenta", "red"), bty = 'n')

curve(exp(confiança(x, preu.inicial, mean(rend.CAT), sd(rend.CAT), 1)),
      main= "Evolució dels preus", 
      xlab = "Dies", ylab = "$P_t$", 
      ylim = c(50,250), xlim = c(0,500), 
      lty = 'dashed', col = 'magenta')
curve(exp(confiança(x, preu.inicial, mean(rend.CAT), sd(rend.CAT), -1)),
      add = TRUE,
      lty = 'dashed', col = 'magenta')
for(k in 1:N) {lines(dies, exp(passejos[ultims[k],]), type = "l", col = colors[k])}
curve(exp(confiança(x, preu.inicial, mean(rend.DJI), sd(rend.DJI), -1)),
      add = TRUE,
      lty = 'dashed', col = 'red')
curve(exp(confiança(x, preu.inicial, mean(rend.DJI), sd(rend.DJI), 1)),
      add = TRUE,
      lty = 'dashed', col = 'red')
legend("topleft", legend = c("CAT", "DJI"), 
       lty="dashed", lwd=c(2, 2),
       col=c("magenta", "red"), bty = 'n')
dev.off()
