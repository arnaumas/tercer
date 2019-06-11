library(quantmod)
getSymbols("^DJI",from="2015-01-01",to="2018-12-31")
getSymbols("CAT",from="2015-01-01",to="2018-12-31")

# Preus i rendibilitats
preus.CAT <- CAT$CAT.Close
preus.DJI <- DJI$DJI.Close
rendibilitat <- function(preus) {
  100*diff(log(preus))[-1]
}
rend.CAT <- rendibilitat(preus.CAT)

# Sèrie de preus
tikz(file = "../figs/serie-preus.tex", width = 5, height = 4)
serie <- plot(preus.CAT, main = "Evolució del preu de tancament")
serie <- addPanel(rendibilitat, type = 'h', method = 'discrete', col = 'red')
plot(serie)
dev.off()

# Evolució d'una inversió de $100
plot(100*preus.CAT/as.numeric(preus.CAT[1]), main = "Evolució d'una inversió de $100")

# Distribució de les rendibilitats
volat.CAT <- sqrt(250)*sd(rend.CAT)
mitj.CAT <- 250*mean(rend.CAT)
n.dades <- length(rend.CAT)

tikz(file = "../figs/dist-rend.tex", width = 7, height = 4)
nf <- layout(mat = matrix(c(2,1), 2, 1, byrow = TRUE), height = c(8,1))
par(mar = c(3,4,0,0))
boxplot(as.numeric(rend.CAT), horizontal = TRUE, frame = FALSE, col = 'lightblue', whiskcol = 'black', staplecol = 'black', border = 'white', medcol = 'black', medlwd = 1)
par(mar = c(0,4,2,0))
hist(rend.CAT, breaks = 12, xlab = "", ylab = "Densitat", xaxt = 'n', freq = FALSE, col = 'lightblue', border = FALSE, main = "Distribució de les rendibilitats diàries")
curve(dnorm(x, mean = mean(rend.CAT), sd = sd(rend.CAT)), add = TRUE, col = 'red', lty = 3, lw = 2)
dev.off()
