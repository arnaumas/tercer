ord <- sort(as.numeric(rend.CAT))
intervals <- seq(from = -0.08, to = 0.08, by = 0.02)
totals.exp <- hist(ord, intervals, plot = FALSE)$counts
totals.norm <- length(ord)*diff(pnorm(intervals, 
                                        mean = mean(ord), sd = sd(ord)))
totals.nig <- length(ord)*diff(pnig(intervals, 
                                      mu = mean(rend.CAT), delta=sqrt(phi*w), 
                                      alpha = sqrt(w/phi), beta = 0))