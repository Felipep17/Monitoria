#Cargo el DATAFRAME Y LO VISUALIZO
head(datospesos)
X<- datospesos
#
pairs(X[,-1])
#
plot(X$age,X$weight)
plot(X$mppwt,X$weight)
#
cor(X[,-1])
# Realizó la regresión Lineal Múltiple
linearMod <- lm(weight ~age+mppwt, data=datospesos)  # build linear regression model on full data
summary(linearMod)
#intervalos de confianza para los párametros con un 95%
confint(linearMod)
#representación 3D de la regresión con dos predictoras
library(scatterplot3d)
library(plot3D)
library(plotly)
library(scatterplot3d)
library(rgl)
library(plot3Drgl)
z<-datospesos$weight
y<-datospesos$age
x<-datospesos$mppwt
scatter3D(x, y, z, phi = 0, bty = "b",
          pch = 20, cex = 2, ticktype = "detailed",xlab = "mppwt",
          ylab ="Edad Gestacional(Semanas)", zlab = "Peso al Nacer(Kg)")
plotrgl()
#La variable Z es la variable a predecir
#Creamos un objeto para realizar las predicciones con elmodelo
objr<-lm(z ~ x+y)
objr
#preparamos el modelado 3d
grid.lines = 42
x.pred <- seq(min(x), max(x), length.out = grid.lines)


y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(objr, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# Marcamos las líneas de iteracción para que busquen la recta de regresión
fitpoints <- predict(objr)
#ploteamos la gráfica en 3d con recta de regresión
scatter3D(x, y, z, pch = 19, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "mppwt",
          ylab ="Edad Gestacional(Semanas)", zlab = "Peso al Nacer(Kg)",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "")
#Gráfico dinámico
plotrgl()
library(effects)
plot(allEffects(linearMod))
# Validación supuestos
library(lmtest);library(car)
residuos<- residuals(linearMod)
valoresajustados<- fitted.values(linearMod)
plot(valoresajustados,residuos)
lines(lowess(residuos~valoresajustados),lty=2,col="red")
abline(h=0,lty=2)
#Varianza
bptest(linearMod)
#Normalidad
car::qqPlot(residuos)
hist(residuos,freq=F)
lines(density(residuos))
shapiro.test(residuos)
?predict
predict(linearMod,newdata=data.frame(age=38,mppwt=53),interval='confidence')
