#### Contraste de Hipotésis e Intervalos de Confianza ####
set.seed(1)
library(BSDA)
p1<- rnorm(100,5,1)
p2<- rnorm(100,3,1)
X<-data.frame(cbind(c(p1,p2),c(rep(1,100),rep(2,100))))
colnames(X)<- c("Var","Group")
boxplot(X$Var~X$Group,xlab="Grupo",ylab="Variable")
boxplot(rnorm(100,0,1))
p0<- rnorm(1000,10,1)
hist(p0)
sd(p1)
sd(p2)
options(scipen=999)
z.test(
  x=p1,
  y = NULL,
  alternative = "greater",  mu = 3,  sigma.x = 1.080836,  sigma.y = NULL,  conf.level = 0.95)
t.test(
  x           =p0 ,
  y           = p1,
  alternative = "two.sided",
  mu          = 0,
  var.equal   = T,
  conf.level  = 0.95
)
#### Mejoremos los gráficos ####
boxplot(X$Var~X$Group,main="",xlab="Grupo",ylab="Variable",pch=19,col="white",border=c("blue4","pink"))
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
boxplot(X$Var~X$Group,xlab="Grupo",ylab="Varible",pch=19,col="white",border=c("blue4","pink"),add=T)
# Points
stripchart(X$Var~X$Group,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = c("blue4","pink"),           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)   

                                    