#################### Exploratorio de Datos####################
#################### Introducción al manejo gráfico en R###############
######### Andrés Felipe Palomino Monteuma - Escuela de estadística#####
####### Universidad del Valle - Modelo Lineal General I################
#Visualización datos catégoricos
set.seed(10) #Semilla para que los resultados sean replicables
x<- sample(c('Excelente','Regular','Malo'),size=10,replace=T,prob=c(0.2,0.4,0.4))
y<- sample(c('Excelente','Regular','Malo'),size=10,replace=T,prob=c(0.8,0.1,0.1))
z<- sample(c('Excelente','Regular','Malo'),size=10,replace=T,prob=c(0.9,0.05,0.05))
X<- as.data.frame(cbind(x,y,z))
colnames(X)<- c('Per.Clase','Per.Monitoria','Per.Calif')
View(X)
tablas<- apply(X,2,table)
for( i  in 1:ncol(X)){
  barplot(tablas[[i]],las=2,panel.first=grid(),main=colnames(X)[i],col=c("aquamarine","aquamarine4","red1")) 
}
prop.tablas<- lapply(tablas,prop.table)
prop.tablas
########################################################################
#Genero los nombres en orden de lo que me interesa de la variable
rotulos<- colnames(X)
#Ahora genero los rotulos con el número de datos y proporciones
#Diagrama de Torta Organizado
for( i in 1:ncol(X)){
pie(tablas[[i]], labels = rotulos,col=c("aquamarine1","red1","brown","purple"),lty=1,cex=1,main=paste("Diagrama de Torta",colnames(X)[i]),panel.first=grid())
}
########### Análisis Variables Cuantitativas
#Diagramas de Caja
X<- as.data.frame(cbind(rnorm(50,10,2),rpois(50,10),rweibull(50,2,6)))
colnames(X)<- c('Indíce','Conteos','Tiempos')
par(mar=c(2,2,2,2))
par(mfrow=c(1,3))
sapply(seq(1,3),function(j)boxplot(X[,j],main=colnames(X)[j],xlab="",col="blue"))
X$Grupos<- as.factor(c(rep(1,20),rep(2,20),rep(3,10)))
View(X)
par(mfrow=c(1,3))
colnames(X)
sapply(seq(1,3),function(j)boxplot(X[,j]~X[,4],main=colnames(X)[j],xlab="",col="blue"))
###########################################################################
par(mfrow=c(1,3))
par(mar=c(4,2,4,4))
sapply(seq(1,3),function(j)hist(X[,j],main=colnames(X)[j],xlab="",col="khaki4",xlim=c(0,25),breaks="Sturges"))
#####################
# Densidades Kernell
par(mfrow=c(1,3))
sapply(seq(1,3),function(j){
  plot(density(X[,j],kernel="gaussian"),main=colnames(X)[j],xlab="",col="brown",lwd=2)
  lines(density(X[,j],kernel="epanechnikov"),main=colnames(X)[j],xlab="",col="cadetblue",lwd=2)
  lines(density(X[,j],kernel="triangular"),main=colnames(X)[j],xlab="",col="firebrick",lwd=2)
  lines(density(X[,j],kernel="cosine"),main=colnames(X)[j],xlab="",col="black",lwd=2)}
)
class(X[,1])
################
X<- as.data.frame(cbind(rnorm(50,10,2),rpois(50,10),rweibull(50,2,6)))
View(X)
colnames(X)<- c('Indíce','Conteos','Tiempos')
y<- 2+5*(1:100)+rnorm(100,50,50)
x<- 1:100
par(mfrow=c(1,1))
par(mar=c(4,4,4,4))
plot(x,y)
plot(x,y,panel.first=grid(),pch=19,col=c('aquamarine3','red1','purple'),xlab='Porcentaje de Admitidos',ylab=' Número de inscritos',main='Diagrama de dispersión')
#Formato latéx
#main = expression(alpha[1] ^ 2 + frac(beta, 3))
#Texto dentro de los gráficos
plot(x, y, main = "Título principal", cex = 1, col = "blue",panel.first = grid())

#---------------
# Función mtext
#---------------

# Abajo centro
mtext("Texto abajo", side = 1)

# Izquierda centro
mtext("Texto izquierda", side = 2)

# Arriba centro
mtext("Texto arriba", side = 3)

# Derecha centro
mtext("Texto derecha", side = 4)


# Abajo izquierda
mtext("Texto abajo izquierda", side = 1, adj = 0)

# Arriba derecha
mtext("Texto arriba derecha", side = 3, adj = 1)


# Arriba, con separación
mtext("Texto arriba", side = 3, line = 2.5)

#--------------
# Función text
#--------------

# Texto en las coordenadas (-2, 2)
text(20, 100, "Más texto")

# Fórmula en las coordenadas (3, -3)
text(20, 200, expression(frac(alpha[1], 4)))
############## Texto
# Fuentes

plot(x, y, family = "mono")
text(20, 300, "Un texto", family = "sans")
text(10, 200, "Más texto", family = "serif")
text(17, 60, "Otro texto", family = "HersheySymbol")
####### Estilos de fuentes de los rotulos del eje
plot(x, y,
     main = "Mi título",
     sub = "Subtítulo",
     font.main = 1, # Estilo de fuente del título
     font.sub  = 2, # Estilo de fuente del subtítulo
     font.axis = 3, # Estilo de fuente de los ejes X e Y
     font.lab  = 4) # Estilo de fuente de los ticks de los ejes
############## Tamaños
plot(x, y, main = "Mi título", sub = "Mi subtítulo",
     cex.main = 2,   # Tamaño del título
     cex.sub = 1.5,  # Tamaño del subtítulo
     cex.lab = 3,    # Tamaño de las etiquetas de los ejes X e Y
     cex.axis = 0.5) # Tamaño de las etiquetas de los ticks de los ejes
library(latex2exp)
plot(x, y, main = TeX('$\\beta^3, \\mu_{1} ,\\lambda_{10}$'))
############# Función de Resumen 
summary(X)
resumen<- function(x){
  X<- matrix(0,9,1)
  resumen<- round(c(mean(x),median(x),min(x),max(x),var(x),sd(x),quantile(x,0.25),quantile(x,0.75),sd(x)/mean(x)),4)
  for( i  in 1:7){
    X[i,]<- resumen[i]
  }
  rownames(X)<-c('Media','Mediana','Min','Max','Var','Sd','1st Qu.','3rd Qu','Coef.Var')
  colnames(X)<- ('Estadística Descriptivas')
  return(X)
}
resumen(x)
library(xtable)
xtable(resumen(x))
View(X)
######################## Introducción a la imputación
X[1,3]<- NA
X[10,4]<- NA
X[2,3]<- NA
colnames(X)<- c('Indíce','Conteos','Tiempos','Grupos')
View(X)
for(i in 1:ncol(X)){
  X[,i]<-ifelse(is.na(X[,i]),sample(X[,i],1),X[,i])
}
View(X)
#######################################################
############################# Clase modelos lineales#######################
library(alr4) ###### Importación de la librería############################
data("UBSprices") ###### Base de datos#####################################
?UBSprices
View(UBSprices) # Visualización de base datos
X<- UBSprices #Guardo la matriz 
####################
pairs(UBSprices) #Exploración inicial de las variables
plot(UBSprices$rice2003,UBSprices$rice2009)######### Gráfico de dispersión de mi variable de interés
##########
summary(X)# Resumen de la matriz, con estadísticos 
##############Variable auxiliar para ver el cambio en los precios
diferencia<- as.data.frame(X$rice2009-X$rice2003)
X$diferencia<- diferencia
names(X)
rownames(diferencia)<- rownames(X)
max(diferencia[diferencia>0]) 
min(diferencia[diferencia<0])
########### Coeficiente de correlación lineal de pearson
cor(UBSprices$rice2003,UBSprices$rice2009)
######################### #Gráficon con la variable auxiliar
plot(UBSprices$rice2003,UBSprices$rice2009,xlab="Precio del arroz en minutos de trabajo en el año 2003",ylab="Precio del arroz en minutos de trabajo en el año 2009")
points(UBSprices$rice2003[diferencia>0],UBSprices$rice2009[diferencia>0],col="blue4",pch=19)
points(UBSprices$rice2003[diferencia<0],UBSprices$rice2009[diferencia<0],col="red4",pch=19)
points(UBSprices$rice2003[diferencia==0],UBSprices$rice2009[diferencia==0],col="black",pch=19)
legend(x = "bottomright",legend=c("Aumento","Disminuyo","Se mantuvo"),
       col = c("blue4","red4","black"),pt.cex=1,pch=c(19,19,19),
       box.lwd=0.6,text.font =15,cex=0.8)
