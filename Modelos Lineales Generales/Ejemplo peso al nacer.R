########### Importación de la base de datos################
###########################################################
library(alr4)
library(readr)
Data <- read_csv("Data.csv")
X<- Data
#Visualización breve de los datos
#Análisis visual y explotatorio de la base de datos antes del modelo
# Se observan las 10 primeras filas del dataset
head(X)
#Volvemos mas fácil el proceso de llamar las variabes con la función attach
attach(X)
names(X)
View(X)
# Evaluamos el coeficiente de correlación lineal de pearson
cor(X)
# Observamos la relación entre las variables
plot(X, pch=19,col='black')
##### Modelo de regresión lineal
model<- lm(weight~age,data=X)
summary(model)
################### Informe Modelo
x.nuevo = data.frame(age=seq(min(X[,4]),max(X[,4]),length.out=nrow(X)))
pred.media = predict(model,x.nuevo,interval = 'confidence')
pred.nuev.obs= predict(model,x.nuevo,interval = 'prediction')
plot(weight~age,pch=19,xlab="",
     ylab=""
     ,main='',type='n')
abline(model)
length(x.nuevo$age)
lines(x.nuevo$age,pred.media[,2],lty=2,col="purple",lwd=2)
lines(x.nuevo$age,pred.media[,3],lty=2,col="purple",lwd=2)
lines(x.nuevo$age,pred.nuev.obs[,2],lty=3,col="red",lwd=2)
lines(x.nuevo$age,pred.nuev.obs[,3],lty=3,col="red",lwd=2)
legend(x = "bottomright",legend=c("Modelo","Intervalo de confianza 95%","Intervalo de predicción 95%"),
       col = c("black","purple","red"),lty = c(1, 2,3),pt.cex=1,
       box.lwd=0.6,text.font =15,cex=0.3)
#### Con GGPLOT2
library(ggplot2)
library(plotly)
library(reshape2)
# De manera mas elaborada
h<-ggplot(X,aes(x=weight,y=age))+
  geom_point()+
  theme_minimal()+geom_smooth(method = "lm")
ggplotly(h)  
cor<- cor(X$age,X$weight)
melted_cor <- melt(cor)
l<-ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "purple", high = "aquamarine4",
                       limit = c(-1,1), name=paste0("Correlation ", "Pearson")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())