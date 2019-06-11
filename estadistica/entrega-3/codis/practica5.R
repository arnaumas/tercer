library(quantmod)
library(zoo)
library(xts)
library(fBasics)


#Baixar les dades del dow Jones
getSymbols("^DJI",from="2015-01-01",to="2018-12-31")
getSymbols("CAT",from="2015-01-01",to="2018-12-31")

names(CAT)
names(DJI)

preuDJ<-DJI$DJI.Close
preuCAT<-CAT$CAT.Close
plot(preuCAT)
MX<-merge(preuDJ,preuCAT)
plot(MX,multi.panel=2)
P1<-as.numeric(MX[1]);P1

plot(100*MX[,1]/P1[1],ylim=c(70,260),main="Creixement DJI/CAT")#Dow Jones
#legend("center", legend=c("Line 1", "Line 2"),col=c("red", "blue"))
lines(100*MX[,2]/P1[2],col="blue")#Caterpillar

#Pel treball incloure la sharpe ratio!
#Rendibilitats
rendDJ<-100*diff(log(preuDJ))
rendCAT<-100*diff(log(preuCAT))
ts.plot(rendDJ,main="Returns",col="red")

renDJ<-rendDJ[2:nrow(rendDJ),]
renCAT<-rendCAT[2:nrow(rendCAT),]
View(renDJ)

c(250*mean(renDJ),sqrt(250)*sd(renDJ))
c(250*mean(renCAT),sqrt(250)*sd(renCAT))

RDJ<-as.numeric(renDJ[-(1:4)])
nn<-length(RDJ);nn/5
dim(RDJ)<-c(200,5)
W<-apply(RDJ,1,sum)
kurtosis(W)

nn<-length(RDJ);nn/20
dim(RDJ)<-c(50,20)
M<-apply(RDJ,1,sum)
kurtosis(M)

mu<-mean(RDJ);sg<-sd(RDJ)

phi<-var(RDJ)
w<-3/(3+kurtosis(RDJ))
delta<-sqrt(phi*w)
gamma<-sqrt(w/phi)
curve(dnig(x,alpha=gamma,beta=0,delta=delta,mu=0,log=FALSE,add=TRUE,col="darked",lwd=2))
