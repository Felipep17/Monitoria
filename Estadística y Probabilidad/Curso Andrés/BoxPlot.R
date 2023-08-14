X<- DataTxt
X<- t(X)
colnames(X)<- X[1,];X<- X[-1,]
View(X)
x<-as.numeric(X)
X<- data.frame(cbind(x[1:12],x[13:24]))
colnames(X)<- c("Colombia","Brasil")
boxplot(X)
boxplot(X,xlab="",ylab="Calidad global de café",pch=19,col="white",border=c("blue4","aquamarine4"),
        main="Diagramas de caja y bigotes para la Calidad Global de Café por países")
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(X,xlab="",ylab="Calidad global de café",pch=19,col="white",border=c("blue4","aquamarine4"),add=T)
# Points
stripchart(X,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = c("blue4","aquamarine4"),           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   
#### Histrogama
W=c(6, 5, 7.5, 8, 10, 4, 5.5, 6.3, 7, 7, 8, 8.5, 9, 5, 5.7, 6.5, 6.7, 4, 5, 4.8)
summary(W)
var(W)#Varianza
sd(W)# Desviación estándar
mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}
mode(W) #Moda
round(sd(W)/abs(mean(W))*100,2)
hist(W,xlab="Tiempo promedio en minutos",ylab="Frecuencia Relativa",col="aquamarine4",border="black",
     freq=F,ylim=c(0,0.5),panel.first=grid(),main="Histograma del Tiempo promedio en minutos",xlim=c(3,12))
lines(density(W),lwd=2,lty=2)
abline(v=c(mode(W),mean(W),median(W)),lwd=2,lty=2,col=c("red1","purple4","orange4"))
legend("topright",legend=c("Moda","Media","Mediana"),col=c("red1","purple4","orange4"),lwd = 2,lty=2,cex=0.6)
