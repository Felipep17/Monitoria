set.seed(sample(1:10000,1))
X<- rnorm(1000)

Xf<-t(t(X))

Y<- rbeta(1000,2,2)
Z<- rexp(1000,1)
W<- rgamma(1000,2,3)
par(mar=c(3.8,3.8,3.8,3.8))
par(mfrow=c(1,4))
hist(X,main="Histograma Población 1",freq=F,panel.first=grid(),xlab="Valores",ylab="Frecuencia Relativa",col="red1")
hist(Y,main="Histograma Población 2",freq=F,panel.first=grid(),xlab="Valores",ylab="Frecuencia Relativa",col="purple")
hist(Z,main="Histograma Población 3",freq=F,panel.first=grid(),xlab="Valores",ylab="Frecuencia Relativa",col="orange1")
hist(W,main="Histograma Población 4",freq=F,panel.first=grid(),xlab="Valores",ylab="Frecuencia Relativa",col="yellow3")
plot(density(X),main="Kernell de la Población 1",panel.first=grid(),xlab="Valores",ylab="Frecuencia Relativa",col="red1",lwd=4)
plot(density(Y),main="Kernell de la Población 2",panel.first=grid(),xlab="Valores",ylab="Frecuencia Relativa",col="purple",lwd=4)
plot(density(Z),main="Kernell de la Población 3",panel.first=grid(),xlab="Valores",ylab="Frecuencia Relativa",col="orange1",lwd=4)
plot(density(W),main="Kernell de la Población 4",panel.first=grid(),xlab="Valores",ylab="Frecuencia Relativa",col="yellow3",lwd=4)
library(car)
par(mar=c(4,4,4,4))
par(mfrow=c(2,2))
qqPlot(X,xlab="Cuántiles Teóricos",ylab="Cuántiles Muestrales",main="Población 1",pch=19)
qqPlot(Y,xlab="Cuántiles Teóricos",ylab="Cuántiles Muestrales",main="Población 2",pch=19)
qqPlot(Z,xlab="Cuántiles Teóricos",ylab="Cuántiles Muestrales",main="Población 3",pch=19)
qqPlot(W,xlab="Cuántiles Teóricos",ylab="Cuántiles Muestrales",main="Población 4",pch=19)

Y<-as.data.frame(Tiempo)
Yf<- Y[,1]
par(mfrow=c(1,1))
hist(Yf)
plot(density(Yf),main="Kernell de Tiempos",panel.first=grid(),xlab="Valores",ylab="Frecuencia Relativa",col="red1",lwd=4)
qqPlot(Yf,xlab="Cuantiles Teóricos",ylab="Cuantiles Muestrales",main="QQ-Plot")
shapiro.test(Yf)
library(nortest)
ad.test(Yf)
############################### Ejercicio En R
library(tidyverse)
library(broom)
theme_set(theme_classic())
View(notasmatesingles)
plot(notasmatesingles$Matematicas,notasmatesingles$Ingles,pch=19,col="red1", ylab="Notas de Matemáticas",xlab="Notas de Inglés",main="Diagrama de dispersión de las notas")
abline(model)
#Modelo de regresion lineal
model <- lm(Ingles ~ Matematicas, data = notasmatesingles)
model
#Resiudos calculados con la mejor línea de ajuste
model.diag.metrics <- augment(model)
head(model.diag.metrics)
library(ggplot2)
#Grafico linea de regresion sobre los datos
ggplot(model.diag.metrics, aes(Matematicas, Ingles)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Matematicas, yend = .fitted), color = "red", size = 0.1)

#Graficos de diagnosis
par(mar=c(3,3,3,3))
par(mfrow = c(2, 2))
plot(model,col="red1")
library(ggfortify)
autoplot(model)[c(1,2)]
summary(model)
#Normalidad de los residuos selección de gráfico de interes que en este caso sería el qq
par(mfrow = c(1, 1))
par(mar=c(4,4,4,4))
plot(model, 2,col="red1",pch=19)

#Residuos
res=model$residuals

#Shapiro-Wilks
x.test <- shapiro.test(res)

library(nortest)
#Anderson
y.test<- ad.test(res)
p_value_thresh=0.05
#Definiendo la función que me devuelve si se cumple o no la hipotesis
sw_test_results <- function(x.test,p_value_thresh) {
  if(x.test$p.value > p_value_thresh){
    print('Asumimos Normalidad')
  } else {
    print('No Asumimos Normalidad')
    print('Intenta aplicar transformaciones no lineales')
  }
  
}
sw_test_results(y.test,p_value_thresh)
sw_test_results(x.test,p_value_thresh)
#Evaluando potencia con el for
Shapi<- c()
Anderson<- c()
for(i in 1:10000){
Shapi[i]<-shapiro.test(rnorm(100))$p.value
Anderson[i]<- ad.test(rnorm(100))$p.value
}
Potencia1<-length(Shapi[Shapi<0.05])/10000
Potencia2<- length(Anderson[Anderson<0.05])/10000
#Evaluando potencia con Mapply
Shapi1<-mapply(Shapiro<-function(x){
  shapiro.test(rnorm(100))$p.value  
},1:1000)
length(Shapi1[Shapi1<0.05])/1000
Anderson1<-mapply(Anderson<-function(x){
  ad.test(rnorm(100))$p.value  
},1:1000)
length(Anderson1[Anderson1<0.05])/1000

