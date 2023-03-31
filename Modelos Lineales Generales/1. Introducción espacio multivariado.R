set.seed(1)
library(mixtools)
library(mvtnorm)
#Generación de DATOS ALEATORIOS MULTIVARIANTES T-STUDENT
par(mfrow=c(1,1))
bivn<- rmvt(1000,sigma=diag(2),df=100)
plot(bivn)
#Descriptivos multivariantes
clcenter<- colMeans(bivn)
clcov<- cov(bivn)
#Gráfico de elipses
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.025,col="blue",lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.25,col="green",lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.5,col="red",lty=3,lwd=3)
#Densidades Kernell
library(MASS)
bivn.kde<- kde2d(bivn[,1],bivn[,2],n=50)
#gráfico tridimensional
library(rgl)
col1<- heat.colors(length(bivn.kde$z))[rank(bivn.kde$z)]
persp3d(x=bivn.kde,col=col1)
bivn.kde<- kde2d(bivn[,1],bivn[,2],n=50)
filled.contour(x=bivn.kde$x,y=bivn.kde$y,z=bivn.kde$z,plot.axes = {axis(1);axis(2);points(0,0)})
image(bivn.kde)
contour(bivn.kde,add=TRUE)
#Con gg
library(ggplot2)
a<-data.frame(a1=bivn[,1],a2=bivn[,2])
#Con correlación 
#Gráfica de solo contorno
ggplot(a,aes(x=a1,y=a2)) + geom_density2d()
#Sólo area
ggplot(a,aes(x=a1,y=a2)) + stat_density2d(aes(fill=..level..),geom="polygon")
#área más contorno
ggplot(a,aes(x=a1,y=a2)) + stat_density2d(aes(fill=..level..),geom="polygon",colour="white")
####################
#Importación de Libería
library(MASS)
#Generación de normal multivariada con correlación moderada
bivn<- mvrnorm(5000,mu=c(0,0),Sigma=matrix(c(1,0.5,0.5,1),2))
bivn
#Generacion de normal multivariada independientes
bivn2<- mvrnorm(1000,mu=c(0,5),Sigma=matrix(c(1,0,0,1),2))
bivn2
#Generación de normal multivariada con alta correlación
bivn3<- mvrnorm(1000,mu=c(0,5),Sigma=matrix(c(1,0.9,0.9,1),2))
bivn3
#Graficas
plot(bivn[,1],bivn[,2],xlab = "",ylab="")
plot(bivn2[,1],bivn2[,2],xlab = "",ylab="")
plot(bivn3[,1],bivn3[,2],xlab = "",ylab="")
#Cálculo de Densidades Kernell:
bivn.kde<- kde2d(bivn[,1],bivn[,2],n=50)
bivn2.kde<- kde2d(bivn2[,1],bivn2[,2],n=50)
bivn3.kde<- kde2d(bivn3[,1],bivn3[,2],n=50)
#plot de calor
image(bivn.kde)
contour(bivn.kde,add=TRUE)
image(bivn2.kde)
contour(bivn2.kde,add=TRUE)
image(bivn3.kde)
contour(bivn3.kde,add=TRUE)
#Puntos con contornos
plot(bivn[,1],bivn[,2],col="aquamarine1",xlab = "",ylab="")
contour(bivn.kde,add=TRUE)
plot(bivn2[,1],bivn2[,2],col="khaki",xlab = "",ylab="")
contour(bivn2.kde,add=TRUE)
plot(bivn3[,1],bivn3[,2],col="orange",xlab = "",ylab="")
contour(bivn3.kde,add=TRUE)
#Visualización de normal multivariada en 3 dimensiones básica
persp(bivn.kde,phi=45,theta=30,shade=0.5,border=NA)
#Multidimensional Animado
library(rgl)
col1<- heat.colors(length(bivn.kde$z))[rank(bivn.kde$z)]
persp3d(x=bivn.kde,col=col1)
col2<- rainbow(length(bivn2.kde$z))[rank(bivn.kde$z)]
persp3d(x=bivn2.kde,col=col2)
col3<- rainbow(length(bivn3.kde$z))[rank(bivn.kde$z)]
persp3d(x=bivn3.kde,col=col3)
###########################
#Cargo el DATAFRAME Y LO VISUALIZO
head(datospesos)
X<- datospesos
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
###########
names(X)
modelcompleto<- lm(weight~.,data=X[,-1])
summary(modelcompleto)
############
names(X)
model1<- lm(weight~age+mnocig,data=X)
model2<- lm(weight~+age,data=X)
summary(model1)
summary(model2)
anova(model1,model2)
#############
#Importo la librería
library(readr)
X <- read_csv("C:/Users/sebas/OneDrive/Escritorio/Proyectos/Modelos/Bases de datos/grasa.csv")
head(X)
#Breve descriptivas de la variable de respuesta
pairs(X)
summary(X[,1])
# Exploración gráfica de las variables de interés para el modelo
#######
par(mfrow=c(2,3))
plot(X$age,X$siri,ylab="Porcentaje de masa corporal",xlab="Edad en años",pch=19,col="blue3",panel.first = grid())
plot(X$bmi,X$siri,ylab="Porcentaje de masa corporal",xlab="IMC Kg/Ms 2",pch=19, col="red1",panel.first = grid())
plot(X$abdomen,X$siri, ylab="Porcentaje de masa corporal",xlab="Circuferencia del abdomen en cm",pch=19, col="#CD1076",panel.first = grid())
plot(X$neck,X$siri,ylab="Porcentaje de masa corporal", xlab="Circuferencia del cuello en cm",pch=19,col="blue3",panel.first = grid())
plot(X$thigh,X$siri,ylab="Porcentaje de masa corporal",xlab="Circuferencia del muslo en cm",pch=19,col="red1",panel.first = grid())
plot(X$hip,X$siri,ylab="Porcentaje de masa corporal",xlab="Circuferencia de la cadera en cm",pch=19,col="#CD1076",panel.first = grid())
#Modelo
modc<- lm(siri~age+bmi+abdomen+neck+thigh+hip,data=X)
summary(modc)
############Estandarizado
y = X$siri
Z = apply(X[,-1],2,function(x){(x-mean(x))/sqrt(sum((x-mean(x))^2))}) #Puede hacerse con Scale
Z<- scale(X[,-1])*(1/sqrt(nrow(X)-1))
ys = (y-mean(y))/sqrt(sum((y-mean(y))^2))
mod.std = lm(ys~Z-1)
summary(mod.std)
