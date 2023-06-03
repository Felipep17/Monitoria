#Primer Punto
#Importó la librería e inicializó un vector con la información
library(alr4)
X<- oldfaith
#Evaluó el tipo de relación entre las variables:
par(mfrow=c(1,1))
#Estimo el modelo
plot(X$Duration,X$Interval,pch=19,col="turquoise3",panel.first=grid(),xlab="Duración en segundos",ylab="Intervalo en minutos",
     main=' Modelo regresión lineal simple')
#####Gráfica para el intervalo de confianza y predicción##
#Estimación del modelo
model<- lm(Interval~Duration,data=X)
summary(model)
#Creación del data.frame con los valores para el modelo
x.nuevo = data.frame(Duration=seq(min(X[,1]),max(X[,1]),1))
#Predicción del intervalo de confianza
pred.media = predict(model,x.nuevo,interval = 'confidence')
#Predicción del intervalo de predicción
pred.nuev.obs= predict(model,x.nuevo,interval = 'prediction')
#Gráficas de las lineas
lines(x.nuevo$Duration,pred.media[,2],lty=2,col="purple",lwd=3)
lines(x.nuevo$Duration,pred.media[,3],lty=2,col="purple",lwd=3)
lines(x.nuevo$Duration,pred.nuev.obs[,2],lty=3,col="red",lwd=3)
lines(x.nuevo$Duration,pred.nuev.obs[,3],lty=3,col="red",lwd=3)
#Caja de enunciados
legend(x = "bottomright",legend=c("Modelo","Intervalo de confianza 95%","Intervalo de predicción 95%"),
       col = c("black","purple","red"),lty = c(1, 2,3),pt.cex=1,
       box.lwd=0.6,text.font =15,cex=0.6)
#Gráfica del modelo
abline(model,lwd=2)
##################### Empezamos con la validación de los supuestos.
#Gráfica de R
#Evaluación de heterocedasticidad
par(mfrow=c(1,1))
library(MASS)
studenti<- studres(model)
ajustados<- model$fitted.values
plot(ajustados,studenti, ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="Residuos Estudentizados VS Ajustados")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti~ajustados), col = "red1")
abline(h=c(-2,2))
library(lmtest)
bptest(model,~Duration+I(Duration^2),data=X)
#Gráfica utilizando 
library(ggfortify)
par(mfrow=c(1,1))
autoplot(model)
fitted.values(model)
#Residuos parciales para evaluar Linealidad y asegurar el supuesto de que la esperanza de los errores es igual a 0
par(mfrow=c(1,1))
crPlots(model,pch=19,xlab="Duración en segundos")
#Gráfico de normalidad
#Prueba de normalidad
res<- model$residuals
shapiro.test(res)
hist(res)
hist(rnorm(1000,0,1))
x<-rnorm(1000,0,1)
shapiro.test(x)
x[1001]<- 20
hist(x)
shapiro.test(x)
qqPlot(x)
res[271]<- 50
qqPlot(model,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19)
#Prueba temporalidad y correlación de los errores en el tiempo
par(mfrow=c(1,2))
plot(studenti,type="b",xlab="Tiempo",ylab="Residuos Estudentizados",main="A")
length(studenti)
plot(studenti[-270],studenti[-1],pch=19,panel.first = grid(),col="turquoise3",xlab="Residuos(t-1)",ylab="Residuos(t)",main="B")
abline(lm(studenti[-1]~studenti[-270]))
cor(studenti[-270],studenti[-1])
durbinWatsonTest(model,method='resample',reps=1000)
# Cluster
km_clusters <- kmeans(scale(X), centers = 2, nstart = 50)
km_clusters
# Se representa el número de cluster al que se ha asignado cada observación y
# se muestra con un código de color el grupo real al que pertenece.

library(factoextra)
fviz_nbclust(x = scale(X), FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(scale(X), method = "euclidean"), nstart = 50)
set.seed(123)
km_clusters <- kmeans(x = scale(X), centers = 2, nstart = 50)

# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = scale(X), show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

#################################################
#Segundo Punto
Y<- read.csv('grasa.csv')
colnames(Y)<-c("Siri","Edad","IMC","Cir.Abdomen","Cir.Cuello","Cir.Muslo","Cir.Cadera")
#Estimación del modelo
model.<- lm(Siri~.,data=Y)
##################### Empezamos con la validación de los supuestos.
#Gráfica de R
#Evaluación de heterocedasticidad
par(mfrow=c(1,1))
library(MASS)
library(zoom)
studenti.1<- studres(model.)
ajustados.1<- model.$fitted.values
par(mfrow=c(1,2))
plot(ajustados.1,studenti.1, ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="(A) Residuos Estudentizados VS Ajustados ")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti.1~ajustados.1), col = "red1",lwd=2)
res.<- model.$residuals
shapiro.test(res.)
qqPlot(model.,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19,main="(B) QQPlot")
zm()
library(lmtest)
bptest(model.,~Edad+IMC+Cir.Abdomen+Cir.Cuello+Cir.Muslo+Cir.Cadera+
         I(Edad^2)+I(IMC^2)+I(Cir.Abdomen^2)+I(Cir.Cuello^2)+
         I(Cir.Muslo^2)+I(Cir.Cadera^2),data=Y)
#Gráfica utilizando 
library(ggfortify)
par(mfrow=c(1,1))
autoplot(model.)
#Residuos parciales para evaluar Linealidad y asegurar el supuesto de que la esperanza de los errores es igual a 0
crPlots(model.,pch=19,main='')
###################################
# Estandarización univariante

X.s <- scale(X)


Kmeans.2 <- kmeans(X.s,2,nstart=25)
Cl.kmeans <- Kmeans.2$cluster
# Scatterplot matrix con la división en grupos resultante
col.cluster <- c("turquoise3","red2")[Cl.kmeans]
plot(X$Duration,X$Interval,pch=19,col=col.cluster,panel.first=grid(),xlab="Duración en segundos",ylab="Intervalo en minutos",
     main=' Modelo regresión lineal simple')
boxplot(X$Duration~Cl.kmeans)
