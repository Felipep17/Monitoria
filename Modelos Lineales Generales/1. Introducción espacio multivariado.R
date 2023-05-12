set.seed(1)
library(mixtools)
library(mvtnorm)
#Generación de DATOS ALEATORIOS MULTIVARIANTES T-STUDENT
par(mfrow=c(1,1))
bivn<- rmvt(1000,sigma=diag(2),df=100)
plot(bivn,xlab='',ylab='') 
#Descriptivos multivariantes Matriz de varianza y covarianzas junto con vector de medias.
clcenter<- colMeans(bivn)
clcov<- cov(bivn)
#Gráfico de elipses
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.01,lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.025,lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.25,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.5,lty=3,lwd=3)
#Densidades Kernell
library(MASS)
bivn.kde<- kde2d(bivn[,1],bivn[,2],n=50)
#gráfico tridimensional
library(rgl)
col1<- c('orange4','orange3','orange2','orange1')
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
norm1<- mvrnorm(5000,mu=c(0,0),Sigma=matrix(c(1,0.5,0.5,1),2))
#Generacion de normal multivariada independientes
norm2<- mvrnorm(1000,mu=c(0,5),Sigma=matrix(c(1,0,0,1),2))
#Generación de normal multivariada con alta correlación
norm3<- mvrnorm(1000,mu=c(0,5),Sigma=matrix(c(1,0.9,0.9,1),2))
#Graficas
plot(norm1[,1],norm1[,2],xlab = "",ylab="")
plot(norm2[,1],norm2[,2],xlab = "",ylab="")
plot(norm3[,1],norm3[,2],xlab = "",ylab="")
#Cálculo de Densidades Kernell:
bivn.kde<- kde2d(norm1[,1],norm1[,2],n=100)
bivn2.kde<- kde2d(norm2[,1],norm2[,2],n=100)
bivn3.kde<- kde2d(norm3[,1],norm3[,2],n=100)
#plot de calor
image(bivn.kde)
contour(bivn.kde,add=TRUE)
image(bivn2.kde)
contour(bivn2.kde,add=TRUE)
image(bivn3.kde)
contour(bivn3.kde,add=TRUE)
#Puntos con contornos
plot(norm1[,1],norm1[,2],col="aquamarine4",xlab = "",ylab="")
contour(bivn.kde,add=TRUE)
plot(norm2[,1],norm2[,2],col="aquamarine4",xlab = "",ylab="")
contour(bivn2.kde,add=TRUE)
plot(norm3[,1],norm3[,2],col="aquamarine4",xlab = "",ylab="")
contour(bivn3.kde,add=TRUE)
#Percepción tridimensional
library(rgl)
persp3d(x=bivn.kde,col=col1)
persp3d(x=bivn2.kde,col=col1)
persp3d(x=bivn3.kde,col=col1)
###########################
#Cargo el DATAFRAME Y LO VISUALIZO
head(datospesos)
X<- datospesos
# Realizó la regresión Lineal Múltiple
linearMod <- lm(weight ~age+mppwt, data=datospesos)  # build linear regression model on full data
summary(linearMod)
#intervalos de confianza para los párametros con un 95%
confint(linearMod)
plot(weight~age,data=datospesos)
plot(age~mppwt,data=datospesos)
#representación 3D de la regresión con dos predictoras
library(scatterplot3d)
library(plot3D)
library(plotly)
library(scatterplot3d)
library(rgl)
library(plot3Drgl)
z<-datospesos$weight
y<-datospesos$age
x<-datospesos$mnocig
scatter3D(x, y, z, phi = 0, bty = "b",
          pch = 20, cex = 2, ticktype = "detailed",xlab = "mnocig",
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
          xlab = "mnocig",
          ylab ="Edad Gestacional(Semanas)", zlab = "Peso al Nacer(Kg)",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "")
#Gráfico dinámico
plotrgl()
###########
names(X)
summary(datospesos)
modelcompleto<- lm(weight~.,data=X[,-1])
summary(modelcompleto)
############
names(X)
model1<- lm(weight~age+mnocig,data=X)
model2<- lm(weight~+age,data=X)
summary(model1)
summary(model2)
plot(age~mnocig,data=X)
#############
X<- preciosCasas
library(MPV)
data("table.b4")
X<- table.b4
View(X)
model<- lm(y~.,data=X)
summary(model)
summary(model)
anova(model)
#
modelc<- lm(y~x1+x2+x4+x5+x7+x8+x9,data=X)
summary(modelc)
anova(modelc,model)
modelc1<- lm(y~x1+x2+x5+x7+x9,data=X)
summary(modelc1)
anova(modelc1,modelc)
modelc2<- lm(y~x1+x2+x7,data=X)
anova(modelc2,modelc1)
summary(modelc2)
modelc3<- lm(y~x1+x2,data=X)
anova(modelc2,modelc3)
modelc4<- lm(y~x1+x2,data=X)
summary(modelc4)
modelc5<- lm(y~x1,data=X)
anova(modelc5,modelc4)
summary(model)
car::vif(model)
plot(X)
car::vif(modelc4)
############
attach(X)
z<-X$y
y<-X$x1
x<-X$x2
scatter3D(x, y, z, phi = 0, bty = "b",
          pch = 20, cex = 2, ticktype = "detailed",xlab = "x2",
          ylab ="x1", zlab = "y")
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
          xlab = "x1",
          ylab ="x2", zlab = "y",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "")

plot(allEffects(model)) # Crear los gráficos de los efectos

y<- alEffec
library(lmridge)

# Cargar datos de ejemplo (el conjunto de datos iris)
data(iris)

# Ajustar el modelo de regresión ridge
fit <- lmridge(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris, lambda = 0.1)

# Hacer predicciones y obtener intervalos de confianza del 95%
newdata <- data.frame(Sepal.Width = 3.5, Petal.Length = 1.5, Petal.Width = 0.5)
pred <- predict(fit, newdata, interval = "confidence", level = 0.95)

# Mostrar los resultados
# Cargar la librería lmridge
library(lmridge)

# Cargar datos de ejemplo (el conjunto de datos iris)
data(iris)
head(iris)
# Ajustar el modelo de regresión ridge
fit <- lmridge(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris, lambda = 0.1)
summary(fit)
# Obtener los coeficientes estimados y la matriz de diseño
beta_hat <- coef(fit)
X <- as.matrix(cbind(rep(1,nrow(iris)),iris[,-c(1,ncol(iris))]))

# Calcular la varianza del error
sigma2_hat <- sum(residuals(fit)^2) / (nrow(iris)-4)

# Calcular la matriz de covarianza de los coeficientes
V_beta_hat <- diag(residuals(fit)^2)*solve(t(X) %*% X + 0.1*diag(ncol(X))) %*% t(X) %*%X %*% solve(t(X) %*% X + 0.1 * diag(ncol(X)))
# Hacer una predicción para nuevas observaciones
newdata <- data.frame(Sepal.Width = 3.5, Petal.Length = 1.5, Petal.Width = 0.5)
X_new <- cbind(1, as.matrix(newdata))
pred <- X_new %*% beta_hat
se_pred <- sqrt(diag(X_new %*% V_beta_hat %*% t(X_new))) * sqrt(sigma2_hat)

# Calcular los intervalos de confianza del 95%
lower <- pred - qt(0.975, nrow(iris)-4) * se_pred
upper <- pred + qt(0.975, nrow(iris)-4) * se_pred

# Mostrar los resultados
data.frame(lower, pred, upper)

