#### Andrés Felipe Palomino Montezuma ####
############################# Clase modelos lineales#######################
library(alr4) ###### Importación de la librería############################
data("UBSprices") ###### Base de datos#####################################
?UBSprices
View(UBSprices) # Visualización de base datos
X<- UBSprices #Guardo la matriz 
####################
pairs(UBSprices,pch=19,col='red1') #Exploración inicial de las variables
plot(UBSprices$rice2003,UBSprices$rice2009)######### Gráfico de dispersión de mi variable de interés
##########
summary(X)# Resumen de la matriz, con estadísticos 
##############Variable auxiliar para ver el cambio en los precios
diferencia<- as.data.frame(X$rice2009-X$rice2003)
X$diferencia<- diferencia
names(X)
rownames(diferencia)<- rownames(X)
max(diferencia[diferencia>0]) 
min(diferencia[diferencia<0])
########### Coeficiente de correlación lineal de pearson
cor(UBSprices$rice2003,UBSprices$rice2009)
######################### #Gráficon con la variable auxiliar

plot(UBSprices$rice2003,UBSprices$rice2009,xlab="Precio del arroz en minutos de trabajo en el año 2003",ylab="Precio del arroz en minutos de trabajo en el año 2009",
     ylim=c(0,100),xlim=c(0,100))
points(UBSprices$rice2003[diferencia>0],UBSprices$rice2009[diferencia>0],col="blue4",pch=19)
points(UBSprices$rice2003[diferencia<0],UBSprices$rice2009[diferencia<0],col="red4",pch=19)
points(UBSprices$rice2003[diferencia==0],UBSprices$rice2009[diferencia==0],col="black",pch=19)
legend(x = "bottomright",legend=c("Aumento","Disminuyo","Se mantuvo"),
       col = c("blue4","red4","black"),pt.cex=1,pch=c(19,19,19),
       box.lwd=0.6,text.font =15,cex=0.8)
summary(diferencia)
abline(h=0,v=0)
indices4<-c(which(diferencia[,1]>40),which(diferencia[,1]< -10))
text(UBSprices$rice2003[indices4],UBSprices$rice2009[indices4],labels=rownames(X)[indices4],cex=0.9,pos=2)
#
##### Modelo de regresión lineal
model<- lm(rice2009~rice2003,data=X)
abline(model,lwd=2,lty=2)
summary(model)
coefficients(model)
################### Informe Modelo
x.nuevo = data.frame(rice2003=seq(0,90,length.out=nrow(X)))
pred.media = predict(model,x.nuevo,interval = 'confidence')
pred.nuev.obs= predict(model,x.nuevo,interval = 'prediction')
plot(UBSprices$rice2003,UBSprices$rice2009,xlab="Precio del arroz en minutos de trabajo en el año 2003",ylab="Precio del arroz en minutos de trabajo en el año 2009")
abline(model)
#Pintamos con la función point las diferencias de precios
points(UBSprices$rice2003[diferencia>0],UBSprices$rice2009[diferencia>0],col="blue4",pch=19)
points(UBSprices$rice2003[diferencia<0],UBSprices$rice2009[diferencia<0],col="red4",pch=19)
points(UBSprices$rice2003[diferencia==0],UBSprices$rice2009[diferencia==0],col="black",pch=19)
text(X$rice2003[indices4],X$rice2009[indices4],labels=rownames(X)[indices4],cex=0.9,pos=2)

lines(x.nuevo$rice2003,pred.media[,2],lty=2,col="purple",lwd=2)
lines(x.nuevo$rice2003,pred.media[,3],lty=2,col="purple",lwd=2)
lines(x.nuevo$rice2003,pred.nuev.obs[,2],lty=3,col="red",lwd=2)
lines(x.nuevo$rice2003,pred.nuev.obs[,3],lty=3,col="red",lwd=2)
legend(x = "bottomright",legend=c("Modelo","Intervalo de confianza 95%","Intervalo de predicción 95%"),
       col = c("black","purple","red"),lty = c(1, 2,3),pt.cex=1,
       box.lwd=0.6,text.font =15,cex=0.5)
confint(model)
summary(X)
#### Validación de supuestos
residuos<- residuals(model)
valoresajustados<- fitted.values(model)
# Validación visual
library(lmtest)
plot(valoresajustados,residuos,pch=19)
lines(lowess(residuos~valoresajustados),col="red")
abline(h=0,lty=2)
#prueba formal
bptest(model)
#Normalidad
library(car)
car::qqPlot(residuos)
hist(residuos)
shapiro.test(residuos)
#
durbinWatsonTest(model,method='resample',reps=1000)
##
library(ggfortify)
autoplot(model)
