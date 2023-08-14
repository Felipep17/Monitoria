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
