#### Introducción a la Regresión Lineal Simple y Múltiple ####
## Andrés Felipe Palomino Montezuma ##
## Escuela de Estadística - Centro de Estudios de Estadística Sigma##
#### Análisis Exploratorio ####
#install.packages("easypackages")
library(easypackages) #Librería especializada en carga de paquetes
lib<- c("MASS","lmtest","car","corrplot","ggplot2","plotly","scatterplot3d","GGally",
        "plot3D","rgl","scatterplot3d","plot3Drgl","alr4","effects","ggfortify","reshape2",
        "patchwork")
easypackages::libraries(lib)
## Carga de base de datos ##
data("ais")
?ais
X<- ais;View(X);names(X)
Y <- subset(X, select = c(Sex, Hg,BMI,SSF))
Y$Sex<- factor(Y$Sex,levels=c(0,1),labels=c("Male","Female"))
### Estadística Descriptivas ###
coef.var=function(x){sd(x)/mean(x)}
Resumen <- rbind(apply(Y[,sapply(Y,is.numeric)], 2, "mean"), apply(Y[,sapply(Y,is.numeric)], 2, "sd"), apply(Y[,sapply(Y,is.numeric)], 2, "coef.var"))
rownames(Resumen) <- c("Promedio", "Desviación", "Coef. Var");Resumen
Boxplot<- ggplot(Y,aes(y=Hg,x=Sex))+geom_boxplot(fill = "white", color = "aquamarine4")+ylim(0,25)+theme_minimal()+
  labs(title="BoxPlot por Sexo",x="Sexo",y="Hg")
Hist<- ggplot(Y,aes(x=Hg,fill=Sex))+geom_histogram()+ylim(0,25)+xlim(10,20)+theme_minimal()+
  labs(title="Histograma por Sexo",x="Sexo",y="Hg")
Densidad<-ggplot(Y,aes(x=Hg,fill=Sex))+ylim(0,1)+xlim(10,20)+theme_minimal()+
  labs(title="Histograma por Sexo",x="Sexo",y="Hg")+geom_density()
Boxplot
Hist
Densidad
ggplotly(Densidad)
ggplotly(Boxplot)
ggplotly(Hist)
P<-Y %>%
  group_by(Sex) %>%
  summarise(
    media_hg = mean(Hg),
    desviacion_estandar = sd(Hg),
    coef_var=sd(Hg)/mean(Hg),
    minimo = min(Hg),
    maximo = max(Hg),
    mediana = median(Hg)
  )
View(P)
install.packages("openxlsx")
library(openxlsx)
write.xlsx(P,"DescriptivasGenero.xlsx")
### Estructuras de Correlación ###
CorGraph<- function(x,method,colneg="aquamarine",colpos="purple"){
cor<- round(cor(x,method = method),2)
melted_cor <- melt(cor)
l<-ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = colneg, high = colpos,
                       limit = c(-1,1), name=paste0("Correlation ", method)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())
l
}
Correlacion<-CorGraph(Y[,sapply(Y,is.numeric)],"pearson",colneg = "red",colpos="orange")
Correlacion
ggplotly(ggpairs(Y)) #Relaciones Bivariadas Rápidas
ggplotly(Correlacion)
## Manera mas detallada ##
L<- ggplot(Y,aes(x=BMI,y=Hg))+geom_point()+theme_minimal()
G<- ggplot(Y,aes(x=SSF,y=Hg))+geom_point()+theme_minimal()
(L/G)
L
#### Regresion Lineal Simple ####
model<- lm(Hg~BMI,data=Y)
options(scipen=999)
anova(model);summary(model)
MODEL<-L+geom_smooth(method='lm');MODEL
ggplotly(MODEL)
#### Modelo de regresión lineal múltiple ####
# La descriptiva debe pensarse bien
# En R3 podemos hacer algo
z<-Y$Hg
y<-Y$SSF
x<-Y$BMI
scatter3D(x, y, z, phi = 0, bty = "b",
          pch = 20, cex = 2, ticktype = "detailed",xlab = "BMI",
          ylab ="SSF", zlab = "Hg")
plotrgl()
#Ajustamos el modelo de regresion
model2<- lm(Hg~BMI+SSF,data=Y);anova(model2)
summary(model2)
#Creamos un objeto para realizar las predicciones con elmodelo
objr<-lm(z ~ x+y)
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
          xlab = "BMI",
          ylab ="SSF", zlab = "Hg",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "")
#Gráfico dinámico
plotrgl()
plot(allEffects(model2))
# Inferencia sobre los párametros
confint(model)
# Predicciones
predict(model,newdata = data.frame(BMI=c(20,24,30)),interval='confidence')
predict(model2,newdata=data.frame(BMI=c(20,24,30),SSF=c(100,150,90)),interval='confidence')
# Extrapolación oculta
newPoints = cbind(x0=rep(1,3),BMI=c(20,24,30),SSF=c(80,150,90))
Auxiliar = model.matrix(model2)
XtX.inv = solve(t(Auxiliar)%*%Auxiliar)
h.values = hatvalues(model2)
hmax = max(h.values)
h0 = apply(newPoints,1,function(x){t(x)%*%XtX.inv%*%x})
h0 >hmax
#### Validación de supuestos ####
#Simulación
x<- 1:100
y<- 2+2*x+rexp(100,1)
Datos<- data.frame(x=x,y=y)
Simular<-ggplot(Datos,aes(x=x,y=y))+geom_point()+theme_minimal()
ggplotly(Simular)
Simular+geom_smooth(method='lm')
modelo2<- lm(y~x)
mean(residuals(modelo2))
Varianza<- function(x){
  Residuos<- data.frame(Residuos=studres(x),Ajustados=fitted.values(x))
  Homocedasticidad<- ggplot(Residuos,aes(x=Ajustados,y=Residuos))+geom_point()+theme_minimal()+geom_smooth()
  print(bptest(x))
  (Homocedasticidad)

}
Varianza(modelo2)
ggplotly(Varianza(modelo2))
Normalidad<- function(x){
  Residuos<- data.frame(Residuos=studres(x))
  Normales<- ggplot(Residuos, aes(sample =Residuos))+stat_qq() + stat_qq_line()+theme_minimal()+xlab("Normal Theoretical Quantiles")+ylab("Sample Quantiles")
  print(shapiro.test(Residuos$Residuos))
  Normales
}
ggplotly(Normalidad(modelo2))
Temporal<- function(x){
  Residuos<- data.frame(Residuos=studres(x),tiempo=1:nrow(x$model))
  Temporal<- ggplot(Residuos, aes(y =Residuos,x=tiempo))+geom_point()+theme_minimal()+geom_line(color = "black") +  # Línea
    geom_point(color = "black")   # Puntos

  print(durbinWatsonTest(x))
  Temporal
}
Temporal(modelo2)
#### Aplicación a nuestro caso
model2<- lm(Hg~BMI+SSF,data=Y)
Varianza(model2)
Normalidad(model2)
Boxplot<- ggplot(Y,aes(y=Hg,x=Sex))+geom_boxplot(fill = "white", color = "aquamarine4")+ylim(0,25)+theme_minimal()+
  labs(title="BoxPlot por Sexo",x="Sexo",y="Hg")
Hist<- ggplot(Y,aes(x=Hg,fill=Sex))+geom_histogram()+ylim(0,25)+xlim(10,20)+theme_minimal()+
  labs(title="Histograma por Sexo",x="Sexo",y="Hg")
Densidad<-ggplot(Y,aes(x=Hg,fill=Sex))+ylim(0,1)+xlim(10,20)+theme_minimal()+
  labs(title="Histograma por Sexo",x="Sexo",y="Hg")+geom_density()
(Boxplot|Hist|Densidad)
Dispersion<-ggplot(Y,aes(x=BMI,y=Hg,color=Sex))+geom_point()
model<- lm(Hg~BMI+Sex,data=Y)
summary(model)
#Variables Clasificadoras
data(ais)
par(mfrow=c(1,2))
boxplot(ais$Hg~ais$Sex,pch=19,col=c(3,2))
plot(density(ais$Hg))
plot(density(ais$Hg[ais$Sex==0]),xlim=c(11,20),lwd=2,main = '',ylab='Densidad',xlab='Hg (g/dl)')
lines(density(ais$Hg[ais$Sex==1]),col=2,lwd=2)
plot(Hg~BMI,data=ais,col=ais$Sex+1,ylab='Hg (g/dl)',xlab='BMI',pch=19)
#
ais$Sex<- factor(ais$Sex)
ais$Sex<- relevel(ais$Sex,"1")
mod.ais = lm(Hg~Sex*BMI, data=ais)
summary(mod.ais)
# Gráfico
#Pendientes que intersecan
plot(Hg~BMI,data=ais,col=ais$Sex+1,ylab='Hg (g/dl)',xlab='BMI',pch=19,xlim=c(-20,50),ylim=c(0,30))
abline(a=mod.ais$coefficients[1],b=mod.ais$coefficients[3],lwd=2)
abline(a=mod.ais$coefficients[1]+mod.ais$coefficients[2],b=mod.ais$coefficients[3]+mod.ais$coefficients[4],col=2,lwd=2)
abline(v=0,h=0,lty=2,lwd=2)

grid()
legend(x = "bottomright",legend=c("Hombres","Mujeres"),
       col=c('black',2),pt.cex=1,pch=15,title='Género',
       box.lwd=1,text.font =20,cex=0.8)
##
mod.ais.red = lm(Hg~BMI, data=ais)
summary(mod.ais.red)
anova(mod.ais.red,mod.ais)
##
mod.ais.lp = lm(Hg~Sex+BMI, data=ais)
summary(mod.ais.lp)
###
data("ais")
plot(Hg~BMI,data=ais,col=ais$Sex+1,ylab='Hg (g/dl)',xlab='BMI',pch=19,xlim=c(-10,40),ylim=c(-10,30))
abline(a=mod.ais$coefficients[1],b=mod.ais$coefficients[3],lwd=2)
abline(a=mod.ais$coefficients[1]+mod.ais$coefficients[2],b=mod.ais$coefficients[3]+mod.ais$coefficients[4],col=2,lwd=2)
abline(a=mod.ais.lp$coefficients[1],b=mod.ais.lp$coefficients[3],lwd=2,lty=2)
abline(a=mod.ais.lp$coefficients[1]+mod.ais.lp$coefficients[2],b=mod.ais.lp$coefficients[3],col=2,lwd=2,lty=2)
abline(h=0,v=0,lty=2,lwd=2)
grid()
legend(x = "bottomright",legend=c("Hombres1","Mujeres1","Hombres2","Mujeres2"),
       col=c('black',2),pt.cex=1,title='Género',lty=c(1,1,2,2),
       box.lwd=1,text.font =20,cex=0.8)
#Hipotésis
anova(mod.ais,mod.ais.lp)
#Nos quedamos con 
plot(Hg~BMI,data=ais,col=ais$Sex+1,ylab='Hg (g/dl)',xlab='BMI',pch=19,xlim=c(-100,100),ylim=c(0,20))
abline(h=0,v=0,lty=2,lwd=2)
abline(a=mod.ais.lp$coefficients[1],b=mod.ais.lp$coefficients[3],lwd=2,lty=2,col=2)
abline(a=mod.ais.lp$coefficients[1]+mod.ais.lp$coefficients[2],b=mod.ais.lp$coefficients[3],col=1,lwd=2,lty=2)
#
influencePlot(mod.ais.lp)
################################################
par(mfrow=c(1,1))
p<- length(coefficients(mod.ais.lp))
n<- nrow(X)
hii.c<- 2*p/n
influencePlot(mod.ais.lp,panel.first=grid())
hii<- hatvalues(mod.ais.lp)
hii.ind<- hii[hii>hii.c]
plot(hii,ylab="Valores diagonal de la matriz Hat",pch=19,xlab="Indíces",ylim=c(0,0.3),panel.first=grid())
points((1:nrow(ais))[hii>hii.c],hii.ind,col="red",pch=19)
text((1:nrow(ais))[hii>hii.c],hii.ind,labels=rownames(ais)[(1:nrow(ais))[hii>hii.c]],pos=c(1,2,3,3,3,1,3,3,1),cex=0.8)
abline(h=2*p/n,lty=2)
n<- length(residuals(mod.ais.lp))
p<- length(coefficients(mod.ais.lp))
res.ponderados<-studres(mod.ais.lp)
hii.c<-2*(p/n)
abline(h=hii.c,lty=2,lwd=2)
indices.1<-(1:nrow(ais))[hii<hii.c & abs(res.ponderados)>2]
indices.2<-(1:nrow(ais))[hii>hii.c & abs(res.ponderados)> qt(0.95,n-p-1)]
indices.3<- (1:nrow(ais))[hii>hii.c & abs(res.ponderados)< 2]
plot(hii,res.ponderados,pch=19,xlab="Valores de la diagonal de la matriz hat", ylab=" Residuos estudentizados",ylim=c(-3,3),xlim=c(0,0.1),panel.first=grid())
abline(h=c(1,0,-1)*2,lty=2,v=hii.c)
points(hii[indices.3],res.ponderados[indices.3],col="yellow",pch=19)
text(hii[indices.3],res.ponderados[indices.3],labels=rownames(ais)[indices.3],pos=3)
points(hii[indices.2],res.ponderados[indices.2],col="red",pch=19)
text(hii[indices.2],res.ponderados[indices.2],labels=rownames(ais)[indices.2],pos=4)
points(hii[indices.1],res.ponderados[indices.1],col="aquamarine",pch=19)
text(hii[indices.1],res.ponderados[indices.1],labels=rownames(ais)[indices.1],pos=c(1,3,4))
legend(x = "topright",legend=c("Influyente","Balanceo","Atípico"),
       col = c("red","yellow","aquamarine"),pch=c(19,19,19),pt.cex=2,
       box.lwd=0.6,title="Identificación de puntos",text.font =15,cex=0.6)
#####
View(ais[(1:nrow(ais))[hii>hii.c],])
############## Distancia de Cook
ck<- cooks.distance(mod.ais.lp)
plot(ck,ylab="Distancia de Coock",pch=19,ylim=c(min(ck),max(ck)+0.1),panel.first=grid())
ck.c<- 4/n
abline(h=ck.c,lty=2)
indices<- (1:nrow(ais))[ck>ck.c]
ck<- ck[ck>ck.c]
points(indices,ck,col="red",pch=19)
text(indices,ck,labels=rownames(ais)[indices],pos=3,cex=0.6)
########### DfBetas
#Beta 1
par(mfrow=c(1,1))
DFBETAS = dfbetas(mod.ais.lp)
head(DFBETAS)
plot(DFBETAS[,2],ylab=quote('DFBETA'~(beta[1])),xlab="Indíce",pch=19,ylim=c(-0.4,0.5),xlim=c(0,150),panel.first=grid())
ind = (1:nrow(ais))[abs(DFBETAS[,2]) > 2/sqrt(nrow(ais))]
dfb = DFBETAS[abs(DFBETAS[,2]) > 2/sqrt(nrow(X)) ,2]
abline(h=c(1,-1)*2/sqrt(nrow(ais)))
text(ind,dfb,rownames(ais)[abs(DFBETAS[,2]) > 2/sqrt(nrow(ais))],pos=c(1,4,3,4),
     cex=0.8)
points(ind,dfb,col="red",pch=19)
################ Dffits
par(mfrow=c(1,1))
DFFITS = dffits(mod.ais.lp)
plot(DFFITS,xlab="Indíces",pch=19,xlim=c(-30,250),ylim=c(-1,1),panel.first=grid())
abline(h=c(-1,1)*2*sqrt(p/n))
ind = (1:nrow(ais))[abs(DFFITS) > 2*sqrt(p/n)]
dfb = DFFITS[abs(DFFITS) > 2*sqrt(p/n)]
text(ind,dfb,rownames(ais)[abs(DFFITS) > 2*sqrt(p/n)],pos=2)
points(ind,dfb,col="purple4",pch=19)
################ CovRatio
COVR = covratio(mod.ais.lp)
plot(COVR,pch=19,ylab="Covratio",xlab="Indíce",panel.first=grid())
abline(h=1+c(-1,1)*3*(p/n))
covr = COVR[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
ind = (1:nrow(ais))[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
text(ind,covr,rownames(ais)[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n)],pos=4)
points(ind,covr,col="purple4",pch=19)
#
influenceIndexPlot(mod.ais.lp)
influence.measures(mod.ais.lp)
