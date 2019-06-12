library(quantmod)
library(fBasics)
getSymbols("^DJI",from="2015-01-01",to="2018-12-31")
getSymbols("CAT",from="2015-01-01",to="2018-12-31")

# Preus i rendibilitats ----
preus.CAT <- CAT$CAT.Close
preus.DJI <- DJI$DJI.Close
rendibilitat <- function(preus) {
  100*diff(log(preus))[-1]
}
rend.CAT <- rendibilitat(preus.CAT)

# Sèrie de preus ----
tikz(file = "../figs/serie-preus.tex", width = 6, height = 3)
serie <- plot(preus.CAT, main = "Evolució del preu de tancament", col = 'blue')
serie <- addPanel(rendibilitat, type = 'h', method = 'discrete', col = 'red')
plot(serie)
dev.off()

# Tancaments
getSymbols("CAT",from="2000-01-01",to="2019-01-01")
tanc.CAT <- CAT$CAT.Close
anys <- endpoints(tanc.CAT, "years")
tanc.CAT <- tanc.CAT[anys]
tikz(file = "../figs/tancament.tex", width = 6, height = 1.7)
plot(tanc.CAT, main = "Tancaments anuals entre 2000 i 2018", col = 'blue')
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

tikz(file = "../figs/dist-rend.tex", width = 5, height = 4)
nf <- layout(mat = matrix(c(2,1), 2, 1, byrow = TRUE), height = c(3,1))
par(mar = c(3,4,0,0))
boxplot(as.numeric(rend.CAT), horizontal = TRUE, frame = FALSE, col = 'lightblue', whiskcol = 'black', staplecol = 'black', border = 'white', medcol = 'black', medlwd = 1)
par(mar = c(0,4,3,0))
hist(rend.CAT, ylim = c(0,0.3), breaks = 12, xlab = "", ylab = "Densitat", xaxt = 'n', freq = FALSE, col = 'lightblue', border = FALSE, main = "Distribució de les rendibilitats diàries")
curve(dnorm(x, mean = mean(rend.CAT), sd = sd(rend.CAT)), add = TRUE, col = 'orchid', lty = 3, lw = 4)
curve(dnig(x, mu = mean(rend.CAT), delta = sqrt(phi*w), alpha = sqrt(w/phi), beta = 0), add = TRUE, col = 'limegreen', lty = 3, lw = 4)
legend("topleft", inset = 0.03, box.lty = 0, col = c("orchid", "limegreen"), legend = c("Normal", "NIG"), lty = 3, lw = 4, border = FALSE)
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

# Comparació amb una NIG
tikz(file = "../figs/nig.tex", width = 5, height = 2.5)
nf <- layout(mat = matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(2.5,2.5))
par(mar = c(4,4,2,0.5))
qqnig(rend.CAT, main = "QQ-plot amb una NIG", xlab = "Quantils NIG", ylab = "Quantils mostrals", mu = mean(rend.CAT), delta = sqrt(phi*w), alpha = sqrt(w/phi), beta = 0, pch = 4, col = 'gray40', line = FALSE)
abline(0,1, lty = 'dashed', col = 'red', lw = 3)
ppnig(rend.CAT, main = "PP-plot amb una NIG", xlab = "$F(x)$", ylab = "$F_n(x)$", mu = mean(rend.CAT), delta = sqrt(phi*w), alpha = sqrt(w/phi), beta = 0, pch = 4, col = 'gray40', line = FALSE)
abline(0,1, lty = 'dashed', col = 'red', lw = 3)
dev.off()

# Comparació amb una normal
tikz(file = "../figs/normal.tex", width = 5, height = 2.5)
nf <- layout(mat = matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(2.5,2.5))
par(mar = c(4,4,2,0.5))
qqPlot(as.numeric(rend.CAT), main = "QQ-plot amb una normal", xlab = "Quantils NIG", ylab = "Quantils mostrals", distribution = "norm", param.list = list(mean = mean(rend.CAT), sd = sd(rend.CAT)), pch = 4, points.col = 'gray40')
abline(0,1, lty = 'dashed', col = 'red', lw = 3)
ppnig(rend.CAT, main = "PP-plot amb una NIG", xlab = "$F(x)$", ylab = "$F_n(x)$", mu = mean(rend.CAT), delta = sqrt(phi*w), alpha = sqrt(w/phi), beta = 0, pch = 4, col = 'gray40', line = FALSE)
abline(0,1, lty = 'dashed', col = 'red', lw = 3)
dev.off()
