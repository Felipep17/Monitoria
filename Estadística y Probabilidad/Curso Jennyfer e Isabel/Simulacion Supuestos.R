#Simulación supuestos
x<- 1:100
y<- -2+2*x+rnorm(100,0,20)
plot(x,y)
#
model<- lm(y~x)
#
residuos<- residuals(model)
valoresajustados<- fitted.values(model)
plot(valoresajustados,residuos)
hist(residuos)
abline(h=0,lty=2)
library(lmtest)
bptest(model)
## Normalidad
library(car)
car::qqPlot(residuos)

