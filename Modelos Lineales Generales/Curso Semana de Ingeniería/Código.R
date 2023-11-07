#### Introducción a la Regresión Lineal Simple y Múltiple ####
## Andrés Felipe Palomino Montezuma ##
## Escuela de Estadística - Centro de Estudios de Estadística Sigma##
#### Análisis Exploratorio ####
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
(L|G)
L
#### Regresion Lineal Simple ####
model<- lm(Hg~BMI,data=Y)
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
