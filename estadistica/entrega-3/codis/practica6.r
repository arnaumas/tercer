
library(quantmod)
library(zoo)
library(xts)
library(fBasics)



getSymbols("CAT",from="2015-01-01",to="2018-12-31")
getSymbols("^DJI",from="2015-01-01",to="2018-12-31")
preuDJ<-DJI$DJI.Close
rendDJ<-100*diff(log(preuDJ))
renDJ<-rendDJ[2:nrow(rendDJ),]
preuCAT<-CAT$CAT.Close
rendCAT<-100*diff(log(preuCAT))
renCAT<-rendCAT[2:nrow(rendCAT),]

#Ljung box test #Autocorrelacions
acf(renCAT)
acf(renCAT,lag.max=6,plot=F)
Box.test(renCAT,lag=10,type="Ljung")#Acceptarem no-correlacionat
?Box.test

#Ljung box test #Autocorrelacions^2
ret2<-renCAT^2
acf(ret2)
Box.test(ret2,lag=10,type="Ljung")  #Rebutjem no-correlacionat,sortirà correlacionat. Períodes volatilitat alta es mantenen en el temps

#Probabilitat guanyar >10% en un trimestre amb DJ. mu=6.40,sigma=13.77
range(time(preuDJ))
preuDJ["2018-12-28"]
t<-0.25
pnorm(log(1.1),mean=6.4*t,sd=13.77*sqrt(t),lower.tail=FALSE)

#Dibuixem les rèpliques amb random Walk
ks<-1000;nn<-63#Només un trimestre
p0=0
S<-numeric(ks*(nn+1));dim(S)<-c(nn+1,ks)
count=0
for(k in 1:ks){
  vec<-rnorm(nn,6.4/250,13.77/sqrt(250));vec<-c(0,vec);#Dividim 250 per tenir mitjana i desviació de rendiment diaria
  S[,k]<-cumsum(vec+p0)
  if(S[nn+1,k]>log(1.1)){count=count+1}
}
count/ks


