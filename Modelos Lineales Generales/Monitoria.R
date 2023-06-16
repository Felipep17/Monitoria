#Clase taller
#Importación de librerías necesarias
rm(list=ls())
par(mar=c(5,5,5,5))
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
?cakes
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
?cement
######
plot(U)
###
model.multi<- lm(y~.,data=U)
summary(model.multi)
car::vif(model.multi)
##################SIMULACIÓN SUPUESTOS
x<- 1:100
y<- -2+2*x+rnorm(100,0,20)+I(x^2)
plot(y~x)
plot(residuals(modelsimu))
modelsimu<- lm(y~x+I(x^2))
plot(fitted.values(modelsimu),MASS::studres(modelsimu),pch=19,xlab="Valores Ajustados",ylab="Residuos Estudentizados")
lines(lowess(MASS::studres(modelsimu)~fitted.values(modelsimu)),col="red",lwd=2,lty=2)
abline(h=c(0,-2,2),lty=2)
car::qqPlot(studres(modelsimu))
bptest(modelsimu)
shapiro.test(studres(modelsimu))
w<- rnorm(1000)
w[1001]<- 10
hist(w)
shapiro.test(w)
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
par(mar=c(5,5,5,5))
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
zm()
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
points((1:nrow(Z))[hii>hii.c],hii.ind,col="red",pch=19)
text((1:nrow(Z))[hii>hii.c],hii.ind,labels=rownames(Z)[(1:nrow(Z))[hii>hii.c]],pos=c(1,2,3,3,3,1,3,3,1),cex=0.8)
abline(h=2*p/n,lty=2)
n<- length(residuals(model))
p<- length(coefficients(model))
res.ponderados<-studres(model)
hii.c<-2*(p/n)
abline(h=hii.c,lty=2,lwd=2)
indices.1<-(1:nrow(Z))[hii<hii.c & abs(res.ponderados)>2]
indices.2<-(1:nrow(Z))[hii>hii.c-0.01 & abs(res.ponderados)> 2]
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
View(Z[(1:nrow(Z))[hii>hii.c],])
############## Distancia de Cook
ck<- cooks.distance(model)
plot(ck,ylab="Distancia de Coock",pch=19,ylim=c(min(ck),max(ck)+0.1),panel.first=grid())
ck.c<- 4/n
abline(h=ck.c,lty=2)
indices<- (1:nrow(Z))[ck>ck.c]
ck<- ck[ck>ck.c]
points(indices,ck,col="red",pch=19)
text(indices,ck,labels=rownames(Z)[indices],pos=3,cex=0.6)
########### DfBetas
#Beta 1
par(mfrow=c(1,2))
DFBETAS = dfbetas(model)
head(DFBETAS)
plot(DFBETAS[,2],ylab=quote('DFBETA'~(beta[1])),xlab="Indíce",pch=19,ylim=c(-0.4,0.5),xlim=c(0,150),panel.first=grid())
ind = (1:nrow(Z))[abs(DFBETAS[,2]) > 2/sqrt(nrow(Z))]
dfb = DFBETAS[abs(DFBETAS[,2]) > 2/sqrt(nrow(Z)) ,2]
abline(h=c(1,-1)*2/sqrt(nrow(Z)))
text(ind,dfb,rownames(Z)[abs(DFBETAS[,2]) > 2/sqrt(nrow(Z))],pos=c(1,4,3,4),
     cex=0.8)
points(ind,dfb,col="red",pch=19)
################
#Beta2
plot(DFBETAS[,3],pch=19,ylab=quote('DFBETA'~(beta[2])),ylim=c(-1,1),xlim=c(-30,160),xlab="Indíce",panel.first=grid())
ind = (1:nrow(Z))[abs(DFBETAS[,3]) > 2/sqrt(nrow(Z))]
dfb = DFBETAS[abs(DFBETAS[,3]) > 2/sqrt(nrow(Z)),3]
abline(h=c(1,-1)*2/sqrt(nrow(Z)))
text(ind,dfb,rownames(Z)[abs(DFBETAS[,3]) > 2/sqrt(nrow(Z))],pos=c(1,1,3))
points(ind,dfb,col="blue",pch=19)
################ Dffits
par(mfrow=c(1,1))
DFFITS = dffits(model)
plot(DFFITS,xlab="Indíces",pch=19,ylim=c(-1,1),xlim=c(-20,200),panel.first=grid())
abline(h=c(-1,1)*2*sqrt(p/n))
ind = (1:nrow(Z))[abs(DFFITS) > 2*sqrt(p/n)]
dfb = DFFITS[abs(DFFITS) > 2*sqrt(p/n)]
text(ind,dfb,rownames(Z)[abs(DFFITS) > 2*sqrt(p/n)],pos=2)
points(ind,dfb,col="purple4",pch=19)
################ CovRatio
COVR = covratio(model)
plot(COVR,pch=19,ylab="Covratio",xlab="Indíce",panel.first=grid())
abline(h=1+c(-1,1)*3*(p/n))
covr = COVR[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
ind = (1:nrow(Z))[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
text(ind,covr,rownames(Z)[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n)],pos=4)
points(ind,covr,col="purple4",pch=19)
vif(model)
#
influenceIndexPlot(model)
influence.measures(model)
autoplot(model)
influencePlot(model)
#
model2<- lm(log(fertility)~log(ppgdp)+pctUrban,data=Z[-indices.2,])
autoplot(model2)
#Variables Clasificadoras
data(ais)
par(mfrow=c(1,2))
plot(density(ais$Hg[ais$Sex==0]),xlim=c(11,20),lwd=2,main = '',ylab='Densidad',xlab='Hg (g/dl)')
lines(density(ais$Hg[ais$Sex==1]),col=2,lwd=2)
plot(Hg~BMI,data=ais,col=ais$Sex+1,ylab='Hg (g/dl)',xlab='BMI',pch=19)
#
mod.ais = lm(Hg~Sex*BMI, data=ais)
summary(mod.ais)
# Gráfico
#Pendientes que intersecan
plot(Hg~BMI,data=ais,col=ais$Sex+1,ylab='Hg (g/dl)',xlab='BMI',pch=19,xlim=c(-100,100),ylim=c(0,20))
abline(a=mod.ais$coefficients[1],b=mod.ais$coefficients[3],lwd=2)
abline(a=mod.ais$coefficients[1]+mod.ais$coefficients[2],b=mod.ais$coefficients[3]+mod.ais$coefficients[4],col=2,lwd=2)
grid()
legend(x = "bottomright",legend=c("Hombres","Mujeres"),
       col=c('black',2),pt.cex=1,pch=15,title='Género',
       box.lwd=1,text.font =20,cex=0.8)
##
mod.ais.red = lm(Hg~BMI, data=ais)
anova(mod.ais.red,mod.ais)
##
mod.ais.lp = lm(Hg~Sex+BMI, data=ais)
summary(mod.ais.lp)
###
plot(Hg~BMI,data=ais,col=ais$Sex+1,ylab='Hg (g/dl)',xlab='BMI',pch=19,xlim=c(-100,100),ylim=c(0,20))
abline(a=mod.ais$coefficients[1],b=mod.ais$coefficients[3],lwd=2)
abline(a=mod.ais$coefficients[1]+mod.ais$coefficients[2],b=mod.ais$coefficients[3]+mod.ais$coefficients[4],col=2,lwd=2)
abline(a=mod.ais.lp$coefficients[1],b=mod.ais.lp$coefficients[3],lwd=2,lty=2)
abline(a=mod.ais.lp$coefficients[1]+mod.ais.lp$coefficients[2],b=mod.ais.lp$coefficients[3],col=2,lwd=2,lty=2)
abline(h=0,v=0,lty=2,lwd=2)
grid()
legend(x = "bottomright",legend=c("Hombres1","Mujeres1","Hombres2","Mujeres2"),
       col=c('black',2),pt.cex=1,title='Género',lty=c(1,1,2,2),
       box.lwd=1,text.font =20,cex=0.8)
#Hipotésis
anova(mod.ais,mod.ais.lp,test="LRT")
#Nos quedamos con 
plot(Hg~BMI,data=ais,col=ais$Sex+1,ylab='Hg (g/dl)',xlab='BMI',pch=19,xlim=c(-100,100),ylim=c(0,20))
abline(h=0,v=0,lty=2,lwd=2)
abline(a=mod.ais.lp$coefficients[1],b=mod.ais.lp$coefficients[3],lwd=2,lty=2)
abline(a=mod.ais.lp$coefficients[1]+mod.ais.lp$coefficients[2],b=mod.ais.lp$coefficients[3],col=2,lwd=2,lty=2)
#
summary(mod.ais.lp)
validaciongrafica(mod.ais.lp)
lambda(mod.ais.lp,-3,3)
mod.ais.lp.box = lm(I(Hg^0.23)~Sex*BMI, data=ais)
validaciongrafica(mod.ais.lp)
#Más de una categoría
plot(density(fertility[group=="oecd"]),xlim=c(0,10),main="",lwd=2,ylab="Densidad",xlab="Fertility")
lines(density(fertility[group=="other"]),lty=2,col="red1",lwd=2)
lines(density(fertility[group=="africa"]),col="aquamarine3",lwd=2)
grid()
legend(x = "topright",legend=c("oecd","other","africa"),
       col=c('black',"red1","aquamarine3"),pt.cex=1,title='Group',lty=c(1,2,1),
       box.lwd=1,text.font =20,cex=0.8)
mod.UN11 = lm(log(fertility)~group*log(ppgdp), data=UN11)
summary(mod.UN11)
table(group)
autoplot(mod.UN11)
########Gráfico
Beta.UN11 = mod.UN11$coefficients
plot(log(fertility)~log(ppgdp),data=UN11,col=UN11$group,xlab='log PNB per cápita (dólares)', ylab='log # esperado de nacidos vivos por mujer',pch=19)
abline(a=Beta.UN11[1],b=Beta.UN11[4],lwd=2)
abline(a=Beta.UN11[1]+Beta.UN11[2],b=Beta.UN11[4]+Beta.UN11[5],col=2,lwd=2,pch=19)
abline(a=Beta.UN11[1]+Beta.UN11[3],b=Beta.UN11[4]+Beta.UN11[6],col=3,lwd=2,pch=19)
legend(x = "topright",legend=c("oecd","other","africa"),
       col=c('black',2,3),pt.cex=1,title='Group',lty=c(1,2,1),
       box.lwd=1,text.font =20,cex=0.8)
#Renombrar
UN11.alt = UN11
UN11.alt$group =  relevel(UN11.alt$group,ref ='other')
mod.UN11.alt = lm(log(fertility)~group*log(ppgdp), data=UN11.alt)
summary(mod.UN11.alt)
#cambio intercepto
mod.UN11.red = lm(log(fertility)~group+log(ppgdp), data=UN11)
anova(mod.UN11.red,mod.UN11)
#Hipotésis por partes
L = matrix(c(0,0,0,0,1,-1),1,6,byrow = T)
linearHypothesis(mod.UN11, hypothesis.matrix=L)
#Simulación pruebas de hipotésis
Shapi1<-mapply(Shapiro<-function(x){
  x<- 1:300
  y<- -2+2*x+rnorm(300,0,2*(1:100))
  mod<- lm(y~x)
  U<- cbind(shapiro.test(MASS::studres(mod))$p.value,
  lmtest::bptest(mod)$p.value)
},1:100)
length(Shapi1[1,][Shapi1[1,]<0.05])/100
length(Shapi1[2,][Shapi1[2,]<0.05])/100
Anderson1<-mapply(Anderson<-function(x){
  ad.test(rnorm(1000,0,1))$p.value  
},1:1000)
length(Anderson1[Anderson1<0.05])/1000
Shapi2<-mapply(Anderson<-function(x){
  shapiro.test(rnorm(1000,0,1))$p.value  
},1:1000)
length(Shapi2[Shapi2<0.05])/1000
#Box-Cox
lambda((lm(rexp(100,1)~1)),-3,3)
################ Intuición
#Importó la librería e inicializó un vector con la información
library(alr4)
X<- oldfaith
#Evaluó el tipo de relación entre las variables:
par(mfrow=c(1,1))
#Estimo el modelo
plot(X$Duration,X$Interval,pch=19,col="turquoise3",panel.first=grid(),xlab="Duración en segundos",ylab="Intervalo en minutos",
     main=' Diagrama de dispersión')
#####Gráfica para el intervalo de confianza y predicción##
#Estimación del modelo
model<- lm(Interval~Duration,data=X)
summary(model)
#Creación del data.frame con los valores para el modelo
x.nuevo = data.frame(Duration=seq(min(X[,1]),max(X[,1]),1))
#Predicción del intervalo de confianza
pred.media = predict(model,x.nuevo,interval = 'confidence')
#Predicción del intervalo de predicción
pred.nuev.obs= predict(model,x.nuevo,interval = 'prediction')
#Gráficas de las lineas
lines(x.nuevo$Duration,pred.media[,2],lty=2,col="purple",lwd=3)
lines(x.nuevo$Duration,pred.media[,3],lty=2,col="purple",lwd=3)
lines(x.nuevo$Duration,pred.nuev.obs[,2],lty=3,col="red",lwd=3)
lines(x.nuevo$Duration,pred.nuev.obs[,3],lty=3,col="red",lwd=3)
#Caja de enunciados
legend(x = "bottomright",legend=c("Modelo","Intervalo de confianza 95%","Intervalo de predicción 95%"),
       col = c("black","purple","red"),lty = c(1, 2,3),pt.cex=1,
       box.lwd=0.6,text.font =15,cex=0.6)
#Gráfica del modelo
abline(model,lwd=2)
##################### Empezamos con la validación de los supuestos.
#Gráfica de R
#Evaluación de heterocedasticidad
par(mfrow=c(1,1))
library(MASS)
studenti<- studres(model)
ajustados<- model$fitted.values
plot(ajustados,studenti, ylab='Residuos Estudentizados',
     xlab='Valores Ajustados',pch=19,col="aquamarine4",main="Residuos Estudentizados VS Ajustados")
abline(h=0,lty=2,lwd=2)
lines(lowess(studenti~ajustados), col = "red1")
abline(h=c(-2,2))
library(lmtest)
bptest(model,~Duration+I(Duration^2),data=X)
#Gráfica utilizando 
library(ggfortify)
par(mfrow=c(1,1))
autoplot(model)
fitted.values(model)
#Residuos parciales para evaluar Linealidad y asegurar el supuesto de que la esperanza de los errores es igual a 0
par(mfrow=c(1,1))
crPlots(model,pch=19,xlab="Duración en segundos")
#Gráfico de normalidad
#Prueba de normalidad
res<- model$residuals
shapiro.test(res)
hist(res)
hist(rnorm(1000,0,1))
x<-rnorm(1000,0,1)
shapiro.test(x)
x[1001]<- 20
hist(x)
shapiro.test(x)
qqPlot(x)
res[271]<- 50
qqPlot(model,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",pch=19)
#Prueba temporalidad y correlación de los errores en el tiempo
par(mfrow=c(1,2))
plot(studenti,type="b",xlab="Tiempo",ylab="Residuos Estudentizados",main="A")
length(studenti)
plot(studenti[-270],studenti[-1],pch=19,panel.first = grid(),col="turquoise3",xlab="Residuos(t-1)",ylab="Residuos(t)",main="B")
abline(lm(studenti[-1]~studenti[-270]))
cor(studenti[-270],studenti[-1])
durbinWatsonTest(model,method='resample',reps=1000)
# Cluster
km_clusters <- kmeans(scale(X), centers = 2, nstart = 50)
km_clusters
# Se representa el número de cluster al que se ha asignado cada observación y
# se muestra con un código de color el grupo real al que pertenece.

library(factoextra)
fviz_nbclust(x = scale(X), FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(scale(X), method = "euclidean"), nstart = 50)
set.seed(123)
km_clusters <- kmeans(x = scale(X), centers = 2, nstart = 50)

# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = scale(X), show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = F) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")
X<- cbind(X,km_clusters$cluster)
colnames(X)[3]<- c("Cluster")
plot(X$Interval~X$Duration,col=X$Cluster)
################
###### Regresión Robusta
library(ggfortify)
?kootenay
data(kootenay, package="robustbase")
X<- kootenay
plot(X,pch=19,col="aquamarine4",panel.first=grid())
points(X$Libby[4],X$Newgate[4],col="red1",pch=19)
model<- lm(Newgate~Libby,data=X)
abline(model,lwd=2,lty=2,col="black")
abline(model1,lwd=2,lty=2,col="red1")
summary(model)
Influ<-influence.measures(model)
Influ$is.inf
model1<- lm(Newgate~Libby,data=X[-4,])
validaciongrafica(model1)
