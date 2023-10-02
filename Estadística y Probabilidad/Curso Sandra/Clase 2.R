#### Introducción a gráficos de variables cualitativas ####
## Andrés Felipe Palomino Montezuma - Universidad del Valle ##
#### Importación de la base de datos ####
#Recordemos que debemos utilizar una librería
Datos<-data.frame(read.table("alumnos0607.txt",dec=",",header=TRUE));head(Datos)
str(Datos)
#Dimensión del dataset
dim(Datos); n<- dim(Datos)[1]; p<- dim(Datos)[2];attach(Datos);names(Datos)
#Creación de tablas
tabla<- table(Equipo);tabla
tabla.prop<- round(prop.table(tabla),3); tabla.prop
#Frecuencias acumuladas
tablaacumulada<- cumsum(tabla);tablaacumulada
tablaacumuladaprop<- cumsum(tabla.prop);tablaacumuladaprop
#Procedemos a gráficar
#### Sesión 1 ####
barplot(tabla)# Lo podemos mejorar
x11()
par(mfrow=c(2,3))

barras<- barplot(tabla,ylim=c(0,20),panel.first=grid(),xlab="Equipos",
                 ylab="Frecuencia Absoluta",col=c("red1","red2","red3","red4"));barras
text(x = barras,
     y = tabla,
     labels = tabla, 
     pos = 3,
     col = "black",
     cex = 1.5)
#Frecuencia Relativa
barras2<- barplot(tabla.prop,ylim=c(0,1),panel.first=grid(),xlab="Equipos",
                 ylab="Frecuencia Absoluta",col=c("red1","red2","red3","red4"));barras
text(x = barras2,
     y = tabla.prop,
     labels = tabla.prop, 
     pos = 3,
     col = "black",
     cex = 1.5)
#Polígono de frecuencias absolutas
plot(tabla,type='b',panel.first=grid(),ylab="Frecuencias Absolutas",
     ylim=c(0,20),pch=19,xlim=c(0,10))
text(x = barras,
     y = tabla,
     labels = tabla, 
     pos = 3,
     col = "black",
     cex = 1.5)
plot(tabla.prop,type='b',panel.first=grid(),ylim=c(0,0.5),xlim=c(0,10),
     ylab="Frecuencias Relativas",pch=19)
text(x = barras2,
     y = tabla.prop,
     labels = round(tabla.prop,2), 
     pos = 3,
     col = "black",
     cex = 1.5)
#Dotchart
dotchart(as.numeric(tabla.prop),labels=c("BAR","CEL","DEP","NIN","RMA","VAL"),panel.first=grid(),pch=19)
#Gráfico de Torta
#Diagrama de Torta Organizado
pie(tabla,lty=1,cex=1,panel.first=grid(),col=c("red1","red3","aquamarine4","aquamarine3","purple3","purple4"))
tabla
legend(1,1.2,legend=c("BAR: 6.8%","CEL: 9.1%","DEP: 29.5%",
"NIN: 34.1%" , "RMA: 18.2%",   "VAL: 2.3%"),col=c("red1","red3","aquamarine4","aquamarine3","purple3","purple4"),pch=15,cex=0.6)
#Ahora es tu turno con otra categoría
####Variables Continuas####
summary(IMC)
x11()
hist(Datos$IMC,xlab="IMC",ylab="Frecuencia Relativa",freq=F)
lines(density(IMC),lwd=2,lty=2)
#### Descriptivas ####
summary(Hermanos)
var(Hermanos);sd(Hermanos);sd(Hermanos)/abs(mean(Hermanos))*100
#### Boxplot####
boxplot(Hermanos)# Puede mejorar 
#
x11()
par(mfrow=c(1,2))
# Vertical box plot
boxplot(Hermanos,main="",xlab="",ylab="Número de Hermanos",pch=19,col="white",border=c("blue4"))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(Hermanos,xlab="",ylab="Número de Hermanos",pch=19,col="white",border=c("blue4"),add=T)
# Points
stripchart(Hermanos,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = c("blue4","pink"),           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   
# Vertical box plot
boxplot(Peso,main="",xlab="",ylab="Peso en Kilogramos",pch=19,col="white",border=c("blue4"))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(Peso,xlab="",ylab="Peso en Kilogramos",pch=19,col="white",border=c("pink4"),add=T)
# Points
stripchart(Peso,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = ("pink"),           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   
#### Plus ####
# Agrupa los valores de la variable millas por galÃ³n (mpg) en 9 clases y calcula su frecuencia absoluta
TablaCut<- table(Peso = factor(cut(Peso, breaks=9)))

# Muestra el valor de la hoja de datos tablaMpg
# La clase modal es la clase con mayor frecuencia absoluta
barplot(TablaCut)
TablaCut<- data.frame(TablaCut)
# Muestra la tabla de frecuencias completa redondeando las frecuencias relativas a 3 decimales
Tabla1<-transform(TablaCut, 
                  FreqAc = cumsum(Freq), 
                  Rel = round(prop.table(Freq), 3), 
                  RelAc = round(cumsum(prop.table(Freq)), 3))
colnames(Tabla1)<-c("Intervalos de Peso en Kg","Frecuencia Absoluta","Frecuencia Absoluta Acumulada","Frecuencia Relativa","Relativa Acumulada")
TablaT<- as.data.frame(Tabla1)
