#Simulación
x<- 1:100
y<- -2+2*x+rnorm(100,0,20)
z<- -2+4*x+rnorm(100,0,400)
#Gráfico de manera de dispersión simple
plot(y~x,xlab="",ylab="",pch=19)
#Coeficiente de correlación
cor(y,x)
X<- data.frame(cbind(y,x,z))
rownames(X)[1:5]<- c("Andrés","Felipe","Sandra","Tatiana","Johana")
#Múltiple dispersión
plot(X)
pairs(X,pch=19)
cor(X)
model<- lm(y~x,data=X)
summary(model)
anova(model)
abline(model,lwd=2,lty=2)
### Puntos influyentes
plot(model)
influencePlot(model)
x<-influence.measures(model)
x$is.inf
#Validación de supuestos
qqnorm(residuals(model))
qqline(residuals(model))
library(MASS)
library(lmtest)
res<-studres(model)
plot(res~fitted.values(model),pch=19)
shapiro.test(residuals(model))
bptest(model)
car::qqPlot(residuals(model))
# MCP
res<- residuals(model)
varianza<- lm(abs(res)~x,data=Z)
w<- 1/(fitted.values(varianza)^2)
model<- lm(y~x,weights=w)
responderados<- residuals(model)*sqrt(w)
plot(responderados~fitted.values(model))
plot(model)
#Box-Cox
library(MASS)
summary(y)
model<- lm(y+260~x)
sec<-seq(-3,3,length.out=1000)
# Box-Cox
u<-boxcox(model,sec)
u$x[u$y==max(u$y)]
#Truquito
model<- lm(y+260~1)
