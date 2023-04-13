#Generó una semilla para que los resultados sean replicables
set.seed(1)
#Genero una matriz nula
monedas<-matrix(0,10000,10)
View(monedas)
#Genero la simulación de una moneda con probababilidades unitarias
#1 para cara 0 para sello
for(i in 1:10000){
monedas[i,]<-sample(c("C","S"),10,1/2)
}
View(monedas)
#Contador de rachas
contador<-c()
for(i in 1:10000){
contador[i]<-rle(monedas[i,])
}
head(contador)
rachas<-c()
for(i in 1:10000){
  rachas[i]<- length(contador[[i]])
}
t<- table(rachas)
t
par(mfrow=c(1,2))
par(mar=c(3,3,3,3))
plot(t,col=c(1:10),xlab="Número de Rachas",ylab="Frecuencia Relativa",panel.first=grid(),main="Simulación 1")

#Seguna vez
set.seed(10)
monedas1<-matrix(0,10000,10)
head(monedas1)
#Genero la simulación de una moneda con probababilidades unitarias
for(i in 1:10000){
  monedas1[i,]<-sample(c(1,0),10,3/4)
}
contador1<-c()
for(i in 1:10000){
  contador1[i]<-rle(monedas1[i,])
}
rachas1<-c()
for(i in 1:10000){
  rachas1[i]<- length(contador1[[i]])
}
t1<- table(rachas1)
plot(t1,col=c(1:10),xlab="Número de Rachas",ylab="Frecuencia Relativa",panel.first=grid(),main="Simulación 2")
t1
###################
library(nortest) ###REALIZA 10 PRUEBAS DE NORMALIDAD###
library(moments) ###REALIZA 1 PRUEBA DE NORMALIDAD###
data<-c(16,191,706,1621,2487,2398,1675,715,177,14)
t3<-as.data.frame(t1)
t3$Fre.Rel<- t3$Freq/sum(t3[,2])
t3$Fre.Rel.Acu<-cumsum(t3$Fre.Rel)
t3$Fre.Abs.Acu <- cumsum(t3$Freq)
t3
par(mfrow=c(1,1))
ad.test(data)
shapiro.test(data)
qqnorm(data,pch=19)
qqline(data,col="red3")
plot(t3$rachas1,t3$Fre.Rel)
######################
#Número de elementos por jugada
########Convertir a Categoricas
monedas[monedas=="C"]<-1
monedas[monedas=="S"]<-0
View(monedas)
n1<-matrix(0,10000,1)
n2<-matrix(0,10000,1)
monedas<-apply(monedas,2,as.numeric)
for(i in 1:10000){
  n1[i,]<-sum(monedas[i,])
  n2[i,]<-10-sum(monedas[i,])
}
conteos<-cbind(n1,n2,rachas)
colnames(conteos)<-c("Número de Caras","Número de Sellos","Número de Rachas")
View(conteos)
############# Segundo Ejemplo

n1.<-matrix(0,10000,1)
n2.<-matrix(0,10000,1)
for(i in 1:10000){
  n1.[i,]<-sum(monedas1[i,])
  n2.[i,]<-10-sum(monedas1[i,])
}
conteos1<-cbind(n1.,n2.,rachas1)
colnames(conteos1)<-c("Número de Caras","Número de Sellos","Número de Rachas")
View(conteos1)
monedas1[monedas1==1]<-"C"
monedas1[monedas1==0]<-"S"
View(monedas1)
############ Estadístico de Prueba
#Inserto el número de observaciones de la v.a #1
#Inserto el número de observaciones de la v.a #2
#Número de rachas
TestRachas<- function(n1,n2,R){
  n<- n1+n2
  mu_r<-((2*n1*n2)/(n1+n2))+1
  sigma_r<- ((2*n1*n2*(2*n1*n2-n1-n2))/((n1+n2)^2*(n1+n2-1)))
  zr<- (R-mu_r)/sqrt(sigma_r)
  valorp<- pnorm(zr)
  return(valorp)
  
}
TestRachas(conteos[1,1],conteos[1,2],conteos[1,3])

