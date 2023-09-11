#### Contraste de Hipotésis e Intervalos de Confianza ####
set.seed(1)
library(BSDA)
p1<- rnorm(100,50,1)
p2<- rnorm(100,10,1)
X<-data.frame(cbind(c(p1,p2),c(rep(1,100),rep(2,100))))
colnames(X)<- c("Var","Group")
boxplot(X$Var~X$Group,xlab="Grupo",ylab="Variable")
boxplot(rnorm(100,0,1))
p0<- rnorm(1000,10,1)
hist(p0)
sd(p0)
sd(p2)
z.test(
  x=p1,
  y = p2,
  alternative = "two.sided",  mu = 0,  sigma.x = 1.05717,  sigma.y = 1.038411,  conf.level = 0.95)
t.test(
  x           =p0 ,
  y           = NULL,
  alternative = "two.sided",
  mu          = 0,
  var.equal   = TRUE,
  conf.level  = 0.95
)
                                    