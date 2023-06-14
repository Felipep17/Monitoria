#Clase taller
#Importación de librerías necesarias
rm(list=ls())
par(mar=c(1,1,1,1))
library(easypackages)
lib_req<-c("glmnet","lmridge","scatterplot3d","plot3D","plotly","rgl","plot3Drgl",
           'effects','psych',
           'car','lmtest','MASS','latex2exp','orcutt',
           'nlme',"zoom",'ggfortify','readxl','pls',"alr4","aod","mixtools","ddalpha")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)
#
options(scipen=999)
#Directorio de trabajo
setwd("C:/Users/sebas/OneDrive/Escritorio/Monitoria Modelos")
source("Source.R")
#Modelos Lineales Generales
# Simulación para entender la relación
# Mejor visualmentehttp://127.0.0.1:41271/graphics/773c3d8b-b028-4ff4-9b64-2759c7abbf19.png
X <- data.frame(matrix(c(yarn$NIR[,1:30],yarn$density),nrow =28, ncol= 31))
colnames(X) <- c(paste("NIR",1:30,sep=""),"density")
Corre = cor(X,method="pearson")
corrplot::corrplot(Corre[,1:10] , method = "ellipse",addCoef.col = "black",type="upper")
pairs(X[,1:10],lower.panel = panel.smooth, pch = 15)
#########################
par(mfrow=c(1,2))
summary(lm(density~NIR4,data=X))
summary(lm(density~NIR29,data=X))
psych::pairs.panels(X[,c(31,4,29)], 
                    method = "pearson", # correlation method
                    hist.col = "aquamarine1",
                    density = TRUE,  # show density plots
                    ellipses = TRUE # show correlation ellipses
)
z<-X[,31];y<-X[,4];x<-X[,29];
scatter3D(x, y, z, phi = 0, bty = "b2",col = c('aquamarine','aquamarine2','aquamarine3','dodgerblue',
                                               'dodgerblue2','dodgerblue3','dodgerblue4'),pch = 20, cex = 2, 
          ticktype = "detailed",xlab='NIR29',ylab='NIR 4', zlab='Density')
plotrgl()
#Creamos un objeto para realizar las predicciones con elmodelo
objr<-lm(z ~ x+y)
summary(objr)
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
          surf = list(x = x.pred, y = y.pred, z = z.pred,facets = NA, fit = fitpoints), 
          main = "",xlab='NIR29 ',zlab="Density",ylab='NIR4', 
          col = c('aquamarine','aquamarine2','aquamarine3',
                  'dodgerblue','dodgerblue2','dodgerblue3','dodgerblue4'))
plotrgl()
summary(objr)
######################
car::vif(objr)
########
plot(allEffects(objr))
######################
#######
## Regresión lineal múltiple
library(alr4)
data(cakes)
Y<- cakes
x<- Y$X1
y<- Y$X2
z<- Y$Y
plot(Y[,-1])
#Modelo polinómico
objr<-lm(z ~ I(x^2)+I(y^2)+y+x)
summary(objr)
#preparamos el modelado 3d
grid.lines = 80
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
          surf = list(x = x.pred, y = y.pred, z = z.pred,facets = NA, fit = fitpoints), 
          main = "",xlab='NIR29 ',zlab="Density",ylab='NIR4', 
          col = c('aquamarine','aquamarine2','aquamarine3',
                  'dodgerblue','dodgerblue2','dodgerblue3','dodgerblue4'))
plotrgl()
##############
data("cement")
U<- cement
######
plot(U)
###
model.multi<- lm(y~.,data=U)
summary(model.multi)
car::vif(model.multi)
##################SIMULACIÓN SUPUESTOS
x<- 1:100
y<- -2+2*x+rnorm(100,0,20)
plot(y~x)
modelsimu<- lm(y~x)
plot(fitted.values(modelsimu),MASS::studres(modelsimu),pch=19,xlab="Valores Ajustados",ylab="Residuos Estudentizados")
lines(lowess(MASS::studres(modelsimu)~fitted.values(modelsimu)),col="red",lwd=2,lty=2)
abline(h=c(0,-2,2),lty=2)
car::qqPlot(studres(modelsimu))
bptest(modelsimu)
shapiro.test(studres(modelsimu))
#####
data("UN11")
pairs(UN11[,-c(1:2,5)])
Z<- na.omit(UN11)
####
modele<- lm(fertility~ppgdp+pctUrban,data=Z)
car::vif(modele)
summary(modele)
plot(allEffects(modele))
validaciongrafica(modele,cor=F)
#
lambda(modele,-3,3)
#
modele.box<- lm(I(fertility^(-0.5))~ppgdp+pctUrban,data=Z)
validaciongrafica(modele.box,cor=F)
##########################Modelo inicial
hist(MASS::studres(modele),freq=F,axes=T,xlab=" Residuos estudentizados",ylab="Frecuencia",main="")
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "azure1")
hist(MASS::studres(modele),add=T,freq=F)
lines(density(studres(modele)))
curve(dnorm(x,mean(studres(modele),sd(studres(modele)))),add=T)
#Modelo BOX-COX
hist(MASS::studres(modele.box),freq=F,axes=T,xlab=" Residuos estudentizados",ylab="Frecuencia",main="")
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "azure1")
hist(MASS::studres(modele.box),add=T,freq=F)
lines(density(studres(modele.box)))
curve(dnorm(x,mean(studres(modele.box),sd(studres(modele.box)))),add=T)
###
model.log<- lm(log(fertility)~log(ppgdp)+pctUrban,data=Z)
validaciongrafica(model.log)
# Minimos cuadrados ponderados
#Errores modelo original
res.mcp<- residuals(model.log)
# Estimación de la varianza
varianza<- lm(abs(res.mcp)~log(ppgdp)+pctUrban,data=Z)
#Cálculo de pesos para ponderar
w = 1/fitted.values(varianza)^2
#Modelo con pesos
model.ponderados<- lm(log(fertility)~log(ppgdp)+pctUrban,data=Z,weights = w)
summary(model.ponderados)
summary(modele)
summary(modele.box)
summary(model.log)
#
validacionmcp(model.ponderados)
#
modelpe<- lm(log(fertility)~log(ppgdp),data=Z,weights = w)
#
validacionmcp(modelpe)
############ Contraste de hipotésis
anova(model.ponderados,modelpe,test="LRT")
anova(model.ponderados,modelpe)
wald.test(vcov(model.ponderados),coef(model.ponderados),L=matrix(c(0,0,1),ncol=3))
######### Introducción a la indentificación de datos atítpicos e influyentes
model<- lm(log(fertility)~log(ppgdp)+pctUrban,data=Z)
x<- log(Z$ppgdp)
y<- Z$pctUrban
z<- Z$fertility
scatter3D(x, y, z, pch = 19, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed", 
          main = "",xlab='Log(ppgdp) ',zlab="Fertilty",ylab='pctUrban', 
          col = c('aquamarine','aquamarine2','aquamarine3',
                  'dodgerblue','dodgerblue2','dodgerblue3','dodgerblue4'))
plotrgl()
# 2D
plot(pctUrban~log(ppgdp),data=Z,pch=19)
attach(Z)
Y1<- data.frame(cbind(log(ppgdp),pctUrban))
#Estimaciones muestrales multivariados
clcov<- cov(Y1)
covind<- matrix(c(2.42664,0,0,548.94452),ncol=2,byrow=T)
clcenter<- as.vector(colMeans(Y1))
#Mediana multivariada
depth.y<-depth.halfspace(Y1,Y1,num.directions=10000,seed=1)
sort.depth.Y<-sort(depth.y,decreasin=TRUE,index.return=TRUE)
depth.Y.sort<-sort.depth.Y$x
depth.Y.sort.index<-sort.depth.Y$ix
median=sort.depth.Y$ix[1]
#Gráfico
par(mar=c(3,3,3,3))
par(mfrow=c(1,1))
plot(Y1,ylim=c(0,120),xlim=c(4,12),xlab="log(ppgdp)")
points(log(ppgdp)[median],pctUrban[median],pch=19,lwd=2,cex=1,col="aquamarine")
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.1,lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.25,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.5,lty=3,lwd=3)
hii<-hatvalues(model)
p<- length(coefficients(model))
n<- nrow(Y1)
hii.c<- 2*(p/n)
indices<- (1:nrow(Y1))[hii>hii.c]
points(log(ppgdp)[indices],pctUrban[indices],col="red",pch=19)
text(log(ppgdp)[indices],pctUrban[indices],labels=rownames(UN11)[indices],pos=3)
#Matrix Robusta
library(robustbase)
require(rrcov) 
mcd <- rrcov::CovMcd(Y1) 
mean_mcd <- mcd$raw.center
cov_mcd <- mcd$raw.cov
#Ellipse 97.5% with robust MCD estimators
mixtools::ellipse(mu = mean_mcd, sigma = cov_mcd, alpha = 0.025,col = "red", lty = 2,lwd=2)
#########
# get inverse of scatter 
cov_mcd_inv <- solve(cov_mcd) 
# compute distances 
# compute the robust distance 
robust_dist <- apply(Y1, 1, function(x){
  x <- (x - mean_mcd) 
  dist <- sqrt((t(x) %*% cov_mcd_inv %*% x)) 
  return(dist) 
}) 
# set cutoff using chi square distribution 
threshold <- sqrt(qchisq(p = 0.975, df = ncol(Y1))) 
# df = no of columns # find outliers 
outliers <- which(robust_dist >= threshold) 
# gives the row numbers of outli
points(log(ppgdp)[outliers],pctUrban[outliers],pch=19,col="purple")
text(log(ppgdp)[outliers],pctUrban[outliers],labels=rownames(UN11)[outliers],pos=3)
zm()
#Independencia
#Independencia entre las observaciones
plot(Y1,xlab="log(ppdgp)")
mixtools::ellipse(mu=clcenter,sigma=covind,alpha=0.1,lty=2,lwd=3,col="red1")
mixtools::ellipse(mu=clcenter,sigma=covind,alpha=0.25,lty=3,lwd=3,col="red1")
mixtools::ellipse(mu=clcenter,sigma=covind,alpha=0.5,lty=3,lwd=3,col="red1")
############# Identificación de puntos atípicos e influyentes
################################################
par(mfrow=c(1,1))
p<- length(coefficients(model))
n<- nrow(Y1)
hii.c<- 2*p/n
influencePlot(model,panel.first=grid())
hii<- hatvalues(model)
hii.ind<- hii[hii>hii.c]
plot(hii,ylab="Valores diagonal de la matriz Hat",pch=19,xlab="Indíces",ylim=c(0,0.3),panel.first=grid())
points((1:nrow(X))[hii>hii.c],hii.ind,col="red",pch=19)
text((1:nrow(X))[hii>hii.c],hii.ind,labels=rownames(Z)[(1:nrow(Z))[hii>hii.c]],pos=c(1,2,3,3,3,1,3,3,1),cex=0.8)
abline(h=2*p/n,lty=2)
n<- length(residuals(model))
p<- length(coefficients(model))
res.ponderados<-studres(model)
hii.c<-2*(p/n)
abline(h=hii.c,lty=2,lwd=2)
indices.1<-(1:nrow(Z))[hii<hii.c & abs(res.ponderados)>2]
indices.2<-(1:nrow(Z))[hii>hii.c & abs(res.ponderados)> qt(0.95,n-p-1)]
indices.3<- (1:nrow(Z))[hii>hii.c & abs(res.ponderados)< 2]
par(mar=c(5,5,5,5))
x11()
plot(hii,res.ponderados,pch=19,xlab="Valores de la diagonal de la matriz hat", ylab=" Residuos estudentizados",ylim=c(-4,4),xlim=c(0,0.1),panel.first=grid())
abline(h=c(1,0,-1)*2,lty=2,v=hii.c)
points(hii[indices.3],res.ponderados[indices.3],col="yellow",pch=19)
text(hii[indices.3],res.ponderados[indices.3],labels=rownames(Z)[indices.3],pos=3)
points(hii[indices.2],res.ponderados[indices.2],col="red",pch=19)
text(hii[indices.2],res.ponderados[indices.2],labels=rownames(Z)[indices.2],pos=4)
points(hii[indices.1],res.ponderados[indices.1],col="aquamarine",pch=19)
text(hii[indices.1],res.ponderados[indices.1],labels=rownames(Z)[indices.1],pos=c(1,3,4))
legend(x = "topright",legend=c("Influyente","Balanceo","Atípico"),
       col = c("red","yellow","aquamarine"),pch=c(19,19,19),pt.cex=2,
       box.lwd=0.6,title="Identificación de puntos",text.font =15,cex=0.6)
#####
View(X[(1:nrow(X))[hii>hii.c],])
############## Distancia de Cook
ck<- cooks.distance(model)
plot(ck,ylab="Distancia de Coock",pch=19,ylim=c(min(ck),max(ck)+0.1),panel.first=grid())
ck.c<- 4/n
abline(h=ck.c,lty=2)
indices<- (1:nrow(X))[ck>ck.c]
ck<- ck[ck>ck.c]
points(indices,ck,col="red",pch=19)
text(indices,ck,labels=rownames(X)[indices],pos=3,cex=0.6)
########### DfBetas
#Beta 1
par(mfrow=c(1,2))
DFBETAS = dfbetas(model)
head(DFBETAS)
plot(DFBETAS[,2],ylab=quote('DFBETA'~(beta[1])),xlab="Indíce",pch=19,ylim=c(-0.4,0.5),xlim=c(0,150),panel.first=grid())
ind = (1:nrow(X))[abs(DFBETAS[,2]) > 2/sqrt(nrow(X))]
dfb = DFBETAS[abs(DFBETAS[,2]) > 2/sqrt(nrow(X)) ,2]
abline(h=c(1,-1)*2/sqrt(nrow(X)))
text(ind,dfb,rownames(X)[abs(DFBETAS[,2]) > 2/sqrt(nrow(X))],pos=c(1,4,3,4),
     cex=0.8)
points(ind,dfb,col="red",pch=19)
################
#Beta2
plot(DFBETAS[,3],pch=19,ylab=quote('DFBETA'~(beta[2])),ylim=c(-1,1),xlim=c(-30,160),xlab="Indíce",panel.first=grid())
ind = (1:nrow(X))[abs(DFBETAS[,3]) > 2/sqrt(nrow(X))]
dfb = DFBETAS[abs(DFBETAS[,3]) > 2/sqrt(nrow(X)),3]
abline(h=c(1,-1)*2/sqrt(nrow(X)))
text(ind,dfb,rownames(X)[abs(DFBETAS[,3]) > 2/sqrt(nrow(X))],pos=c(2,1,3))
points(ind,dfb,col="blue",pch=19)
################ Dffits
par(mfrow=c(1,1))
DFFITS = dffits(model)
plot(DFFITS,xlab="Indíces",pch=19,xlim=c(-30,150),ylim=c(-1,1),panel.first=grid())
abline(h=c(-1,1)*2*sqrt(p/n))
ind = (1:nrow(X))[abs(DFFITS) > 2*sqrt(p/n)]
dfb = DFFITS[abs(DFFITS) > 2*sqrt(p/n)]
text(ind,dfb,rownames(X)[abs(DFFITS) > 2*sqrt(p/n)],pos=2)
points(ind,dfb,col="purple4",pch=19)
################ CovRatio
COVR = covratio(model)
plot(COVR,pch=19,ylab="Covratio",xlab="Indíce",panel.first=grid())
abline(h=1+c(-1,1)*3*(p/n))
covr = COVR[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
ind = (1:nrow(X))[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
text(ind,covr,rownames(X)[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n)],pos=4)
points(ind,covr,col="purple4",pch=19)
vif(model)
#
influenceIndexPlot(model)
influence.measures(model)
autoplot(model)
point<-influence.measures(model)