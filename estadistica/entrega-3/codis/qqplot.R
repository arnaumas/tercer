ord <- sort(as.numeric(rend.CAT))
prob <- (1:length(rend.CAT))/length(rend.CAT)
norm.pp <- pnorm(ord, mean(rend.CAT), sd(rend.CAT))
norm.qq <- qnorm(prob, mean(rend.CAT), sd(rend.CAT))
phi <- as.numeric(var(rend.CAT))
w <- as.numeric(3/(kurtosis(rend.CAT)))
nig.pp <- pnig(ord, mu = mean(rend.CAT), delta = sqrt(phi*w),
               alpha = sqrt(w/phi), beta = 0)
nig.qq <- qnig(prob, mu = mean(rend.CAT), delta = sqrt(phi*w),
               alpha = sqrt(w/phi), beta = 0)

tikz(file = "../figs/nig.tex", width = 5, height = 2.5)
nf <- layout(mat = matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(2.5,2.5))
par(mar = c(4,4,2,0.5))
plot(prob, nig.pp, main = "PP-plot amb una NIG", 
     xlab = "$F_n(x)$", ylab = "$F(x)$",
     pch = 4, cex = 0.5, col = 'gray40')
abline(0,1, lty = 'dashed', col = 'red', lw = 2)
plot(nig.qq, ord, main = "QQ-plot amb una NIG", 
     xlab = "Quantils NIG", ylab = "Quantils empírics",
     pch = 4, cex = 0.5, col = 'gray40')
abline(0,1, lty = 'dashed', col = 'red', lw = 2)
dev.off()

tikz(file = "../figs/normal.tex", width = 5, height = 2.5)
nf <- layout(mat = matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(2.5,2.5))
par(mar = c(4,4,2,0.5))
plot(prob, norm.pp, main = "PP-plot amb una NIG", 
     xlab = "$F_n(x)$", ylab = "$F(x)$",
     pch = 4, cex = 0.5, col = 'gray40')
abline(0,1, lty = 'dashed', col = 'red', lw = 2)
plot(norm.qq, ord, main = "QQ-plot amb una normal", 
     xlab = "Quantils normals", ylab = "Quantils empírics",
     pch = 4, cex = 0.5, col = 'gray40')
abline(0,1, lty = 'dashed', col = 'red', lw = 2)
dev.off()
