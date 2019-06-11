library(quantmod)
getSymbols("^DJI",from="2015-01-01",to="2018-12-31")
getSymbols("CAT",from="2015-01-01",to="2018-12-31")

# Preus i rendibilitats
preus.CAT <- CAT$CAT.Close
rend.CAT <- 100*diff(log(preus.CAT),1)
rend.CAT <- rend.CAT[-1]