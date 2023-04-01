########### Importación de la base de datos################
###########################################################
library(alr4)
library(readr)
Data <- read_csv("Data.csv")
X<- Data
#Visualización breve de los datos
#Análisis visual y explotatorio de la base de datos antes del modelo
# Se observan las 10 primeras filas del dataset
head(X)
#Volvemos mas fácil el proceso de llamar las variabes con la función attach
attach(X)
names(X)
View(X)
# Evaluamos el coeficiente de correlación lineal de pearson
cor(X)
# Observamos la relación entre las variables
plot(X, pch=19,col='black')
##### Modelo de regresión lineal
model<- lm(weight~age,data=X)
################### Informe Modelo
x.nuevo = data.frame(age=seq(min(X[,4]),max(X[,4]),length.out=nrow(X)))
pred.media = predict(model,x.nuevo,interval = 'confidence')
pred.nuev.obs= predict(model,x.nuevo,interval = 'prediction')
plot(weight~age,pch=19,xlab="",
     ylab=""
     ,main='Modelo de regresión',type='n')
plot(weight~age,pch=19,panel.first=grid(),xlab="",
     ylab=""
     ,main='Modelo de regresión')

abline(model)
length(x.nuevo$age)
lines(x.nuevo$age,pred.media[,2],lty=2,col="purple",lwd=2)
lines(x.nuevo$age,pred.media[,3],lty=2,col="purple",lwd=2)
lines(x.nuevo$age,pred.nuev.obs[,2],lty=3,col="red",lwd=2)
lines(x.nuevo$age,pred.nuev.obs[,3],lty=3,col="red",lwd=2)
legend(x = "bottomright",legend=c("Modelo","Intervalo de confianza 95%","Intervalo de predicción 95%"),
       col = c("black","purple","red"),lty = c(1, 2,3),pt.cex=1,
       box.lwd=0.6,text.font =15,cex=0.3)

