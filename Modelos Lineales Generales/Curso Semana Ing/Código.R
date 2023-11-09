#### Introducción a la Regresión Lineal Simple y Múltiple ####
## Andrés Felipe Palomino Montezuma ##
## Escuela de Estadística - Centro de Estudios de Estadística Sigma##
#### Análisis Exploratorio ####
#install.packages("easypackages")
library(easypackages) #Librería especializada en carga de paquetes
lib<- c("MASS","lmtest","car","corrplot","ggplot2","plotly","scatterplot3d","GGally",
        "plot3D","rgl","scatterplot3d","plot3Drgl","alr4","effects","ggfortify","reshape2",
        "patchwork")
easypackages::install_packages(lib)
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
