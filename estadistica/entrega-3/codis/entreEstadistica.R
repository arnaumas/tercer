library(quantmod)
getSymbols("^DJI",from="2015-01-01",to="2018-12-31")
getSymbols("KO",from="2015-01-01",to="2018-12-31")

names(DJI)
names(KO)

prices<-DJI$DJI.Close
pricesko<-KO$KO.Close


plot(prices)
plot(pricesko,col="red")

View(prices)
View(pricesok)

MX<-merge(prices,pricesko)
MX[1]
plot(MX,multi.panel=2)

P1<-as.numeric(MX[1]);P1
plot(100*MX[,1]/P1[1],col="red")
lines(100*MX[,2]/P1[2],col="blue")

plot(100*MX[,1]/P1[1],col="red",ylim = c(70,160))
lines(100*MX[,2]/P1[2],col="blue")

M<-apply.monthly(prices,mean)
Mko<-apply.monthly(pricesko,mean)

prices<-as.numeric(prices)
prices<-prices[!is.na(prices)]

#rendibilitats

rend<-100*diff(log(prices))
rendko<-100*diff(log(pricesko))

ts.plot(rend,main="Returns",col="red")

#rati de sharpe=mu-rf/sigma

#rendibilitat i volatilitat anualitzades

rendn<-as.numeric(rend)
rendnko<-as.numeric(rendko)

rendn<-rendn[!is.na(rendn)]
rendnko<-rendnko[!is.na(rendnko)]

c(250*mean(rendn),sqrt(250)*sd(rendn))
c(250*mean(rendnko),sqrt(250)*sd(rendnko))

ts.plot(prices,main="closing prices",col="maroon")
ts.plot(pricesko,main="closing prices",col="maroon")

library(xts)
plot(M)
length(M) #numero de mesos
plot(Mko)
length(Mko)

#i entonces hacer los tests de kolmogorov i el otro

library(fBasics)
jarqueberaTest(rendn)
kurtosis(rendn)
skewness(rendn)

jarqueberaTest(rendnko)
kurtosis(rendnko)
skewness(rendnko)


#falla més les cues pesades que la simetria

length(rendn)
R<-rendn[-(1:4)]

nn<-length(R);nn/5
dim(R)<-c(200,5)
W<-apply(R,1,sum)

dim(R)<-c(50,20)
M1<-apply(R,1,sum)

jarqueberaTest(W)
ku<-kurtosis(W);ku
sk<-skewness(W);sk

jarqueberaTest(M1)
ku<-kurtosis(M1);ku
sk<-skewness(M1);sk

#hipotesi nulla simple

m0=0;sg0=1;nn<-500
x<-rnorm(nn,m0,sg0)
ks.test(x,"pnorm",m0,sg0)
mu<-mean(x);sg<-sd(x)
ks.test(x,"pnorm",mu,sg)

#NO el pots aplicar si no saps sg0 i m0

#hipotesi nula composta
library(nortest)
lillie.test(x)


#manualment

x<-sort(x)
Ft<-pnorm(x,m0,sg0)
Fe<-seq(1:nn)/nn
D<-max(abs(Fe-Ft));D

#dificil veure model alternatiu
#lleis infinitament divisibles
#normal inversa gaussiana

curve(dnig(x,alpha=gamma,beta=0,delta=delta,mu=0,log=FALSE),add=TRUE,col="darkred",lwd=2)

phi<-var(rendn);phi
w<-3/(3+kurtosis(rendn));w
delta<-sqrt(phi*w);delta
gamma<-sqrt(w/phi);gamma