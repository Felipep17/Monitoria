#Importación de librerías
library(MASS)
library(lmtest)
library(car)
library(ggfortify)
library(gvlma)
library(car)
#Inicialización de la matriz de variable y covaraibles
X<- tip
X<- na.omit(X)
attach(X)
names(X)
min(Tip)
par(mfrow=c(1,2))
plot(Tip~Bill,pch=19,col="red1",ylab="Propina (en doláres)",xlab="Monto total (en doláres)")
boxplot(Tip~Meal,pch=19,col=c("red4","blue4","red4"),las=2,main="Meal",ylab="Propina (en doláres)",xlab="")
par(mfrow=c(1,3))
boxplot(Tip~Age,pch=19,col=c("red4","blue4","red4"),las=2,main="Age",ylab="Propina (en doláres)",xlab="")
boxplot(Tip~Alcohol,pch=19,col=c("red4","blue4","red4"),las=2,main="Alcohol",ylab="Propina (en doláres)",xlab="")
boxplot(Tip~weekend,pch=19,col=c("red4","blue4","red4"),las=2,main="weekend",ylab="Propina (en doláres)",xlab="")
######## Creación del modelo
#Modelo solo con la variable cuantitativa
par(mfrow=c(1,1))
model<- lm(Tip+1~Bill,data=X)
autoplot(model)
summary(model)
hist(residuals(model),freq=F)
lines(density(residuals(model)))
shapiro.test(residuals(model))
qqPlot(residuals(model))
############

model<- lm(Tip~Bill*Meal+Bill*Age+Bill*Alcohol+Bill*weekend,data=X)
autoplot(model)
summary(model)
#Multicolinealidad
vif(model)
#############
#Validación formal de los supuestos
#Evaluación supuestos
#Varianza y linealidad
par(mfrow=c(1,2))
studi<- studres(model)
plot(fitted.values(model),studi,pch=19,panel.first=grid(),col="aquamarine4",ylab="Residuos Estudentizados",xlab="Valores Ajustados",ylim=c(-4,4),main="A")
lines(lowess(studi~fitted.values(model)),col="red1")
abline(h=0,lty=2,lwd=2)
bptest(model,~Bill*Meal+Bill*Age+Bill*Alcohol+Bill*weekend+I(Bill^2),data=X)
#Normalidad
shapiro.test(residuals(model))
qqPlot(model,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",main="B",id=F)
#Transformaciones para hacer la correción
#Calculo del lambda más óptimo
model<- lm(Tip+0.1~Bill*Meal+Bill*Age+Bill*Alcohol+Bill*weekend,data=X)
box.cox<- boxcox(model,lambda=seq(-3,3,length.out = 1000),
                 ylab='log-verosimilitud')
#Selección del Lambda
bc<-round(box.cox$x[box.cox$y ==max(box.cox$y)],2)
#Ajuste del modelo
model.box<-lm(I(Tip^bc)~Bill*Meal+Bill*Age+Bill*Alcohol+Bill*weekend,data=X)
#Resumen del modelo
summary(model.box)
plot(density(residuals(model.box)))
hist(residuals(model.box))
shapiro.test(residuals(model.box))
qqPlot(residuals(model.box))
influencePlot(model.box)
#Supuetos
#Validación de supuestos del modelo con BOX-COX
studenti.box<- studres(model.box)
ajustados.box<- model.box$fitted.values
par(mfrow=c(1,1))
plot(ajustados.box,studenti.box, ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="Residuos Estudentizados VS Ajustados")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti.box~ajustados.box), col = "red1")
par(mfrow=c(1,1))
qqPlot(model,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19,main="QQPLOT")
shapiro.test(residuals(model.box))
bptest(model.box,~Bill*Meal+Bill*Age+Bill*Alcohol+Bill*weekend+I(Bill^2),data=X)
########
res.mcp<- residuals(model)
varianza<- lm(abs(res.mcp)~Bill*Meal+Bill*Age+Bill*Alcohol+Bill*weekend,data=na.omit(X))
w = 1/fitted.values(varianza)^2
model.ponderados<- lm(Tip~Bill*Meal+Bill*Age+Bill*Alcohol+Bill*weekend,data=na.omit(X),weights = w)
summary(model.ponderados)
X<- na.omit(X)
sum(is.na(X))
#Validación Supuestos
par(mfrow=c(1,2))
res.ponderados<- residuals(model.ponderados)*sqrt(w)
plot(fitted.values(model.ponderados),res.ponderados,
     xlab='Valores ajustados',ylab='Residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4",ylim=c(-3,3),main="A")
lines(lowess(res.ponderados~fitted.values(model.ponderados)),col=2,lty=2,lwd=4)
abline(h=0,lty=2,lwd=2)
qqPlot(res.ponderados,xlab="Cuantiles Teóricos",ylab="Residuos Ponderados",main="B",id=F)
shapiro.test(res.ponderados)
################
summary(model.ponderados)
model.c<- lm(Tip~Bill,data=X)
summary(model.c)
##########
res.mcp.c<- residuals(model.c)
varianza.c<- lm(abs(res.mcp.c)~Bill,data=na.omit(X))
w. = 1/fitted.values(varianza.c)^2
model.ponderados.c<- lm(Tip~Bill,data=na.omit(X),weights = w.)
summary(model.ponderados.c)
X<- na.omit(X)
sum(is.na(X))
#Validación Supuestos
par(mfrow=c(1,2))
res.ponderados.c<- residuals(model.ponderados.c)*sqrt(w.)
plot(fitted.values(model.ponderados.c),res.ponderados.c,
     xlab='Valores ajustados',ylab='Residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4",ylim=c(-3,3),main="A")
lines(lowess(res.ponderados.c~fitted.values(model.ponderados.c)),col=2,lty=2,lwd=4)
abline(h=0,lty=2,lwd=2)
qqPlot(res.ponderados.c,xlab="Cuantiles Teóricos",ylab="Residuos Ponderados",main="B",id=F)
shapiro.test(res.ponderados.c)
anova(model.ponderados.c,model.ponderados)
#############3
par(mfrow=c(1,1))
plot(Tip~Bill,pch=19,col="aquamarine4",ylab="Propina (doláres)",xlab="Monto (doláres)")
abline(model.ponderados.c)
summary(model.ponderados.c)
################
################### Informe Modelo
x.nuevo = data.frame(Bill=seq(min(X[,4]),max(X[,4]),len=nrow(X)))
pred.media = predict(model,x.nuevo,interval = 'confidence')
pred.nuev.obs= predict(model,x.nuevo,interval = 'prediction')
abline(model.ponderados.c)
lines(x.nuevo$Bill,pred.media[,2],lty=2,col="purple",lwd=2)
lines(x.nuevo$Bill,pred.media[,3],lty=2,col="purple",lwd=2)
lines(x.nuevo$Bill,pred.nuev.obs[,2],lty=3,col="red",lwd=2)
lines(x.nuevo$Bill,pred.nuev.obs[,3],lty=3,col="red",lwd=2)
legend(x = "bottomright",legend=c("Modelo","Intervalo de confianza 95%","Intervalo de predicción 95%"),
       col = c("black","orange","red"),lty = c(1, 2,3),pt.cex=1,
       box.lwd=0.6,text.font =15,cex=0.3)

