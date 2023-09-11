#### Ejercicios Variables Aleatorias ####
x<- 0:10
lambda<-10
#### Función de densidad ####
plot(x,dpois(x,lambda),type='l')
par(mfrow=c(2,2))
plot(0:10,dpois(0:10,5),type='l')
plot(0:30,dpois(0:30,20),type='l')
plot(0:100,dpois(0:100,50),type='l')
plot(0:200,dpois(0:200,100),type='l')
#### Probabilidades ####
options(scipen=999)
ppois(5,5)
ppois(5,20)
ppois(5,30)
sum(dpois(0:5,5))
#### Generar muestras aleatorias ####
lambda<- 20
n<- 1000
rpois(n,lambda)
X<-replicate(100,rpois(n,lambda))
View(X)
medias<- colMeans(X)
varianzas<- apply(X,2,var)
porcentaje<-function(x){
  sum(x<=20)/length(x)
}
porcentajes<- apply(X,2,porcentaje)
plot(medias,pch=19)
abline(h=20,lwd=2,lty=2,col=c("purple"))
plot(varianzas,pch=19)
abline(h=20,lty=2,lwd=2,col="orange")
plot(porcentajes,pch=19)

abline(h=ppois(20,20),lty=2,lwd=2,col="red1")
ind<- sample(1:100,10)
tablas<-apply(X[,ind],2,table)
par(mfrow=c(2,5))
sapply(tablas,function(x)barplot(x))
par(mfrow=c(1,1))
x<- 0:40
lambda<-20
#### Función de densidad ####
plot(x,dpois(x,lambda),type='l')
