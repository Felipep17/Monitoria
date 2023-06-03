##############################MINIMOS CUADRADOS PONDERADOS#####################
library(ggfortify)
library(MASS)
library(car)
#Estimación varianza
set.seed(1)
b0<- 2
b1<- -2
x<- 1:1000
y<- b0+b1*x+rnorm(1000,0,1:1000)
plot(y~x,axes=F,xlab="",ylab="")
# Color de fondo gris
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "aquamarine1")

# Grid blanco
grid(nx = NULL, ny = NULL,
     col = "white", lwd = 3)
par(new=TRUE)
plot(y~x,pch=19)
model<- lm(y~x)
abline(model,lwd=3,lty=2,col="red1")
summary(model)
#
#Validación rápida de supuestos
autoplot(model) # No es recomendable para informes pero nos da un vistazo
# General para de la situación
#Transformación
#Errores modelo original
res.mcp<- residuals(model)
# Estimación de la varianza
varianza<- lm(abs(res.mcp)~x)
#Cálculo de pesos para ponderar
w = 1/fitted.values(varianza)^2
#Modelo con pesos
model.ponderados<- lm(y~x,weights = w)
summary(model.ponderados)
autoplot(model.ponderados)
#Validación Supuestos
res.ponderados<- residuals(model.ponderados)*sqrt(w)
car::qqPlot(res.ponderados,xlab="Cuantiles Teóricos",ylab=" Residuos ponderados",pch=19)
shapiro.test(res.ponderados)
plot(fitted.values(model.ponderados),res.ponderados,
     xlab='valores ajustados',ylab='residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4")
lines(lowess(res.ponderados~fitted.values(model.ponderados)),col=2,lty=2,lwd=4)
abline(h=0,lty=2,lwd=2)
car::crPlots(model.ponderados,pch=19)
