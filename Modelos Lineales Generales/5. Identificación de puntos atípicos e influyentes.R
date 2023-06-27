library(MASS)
library(lmtest)
library(car)
library(ggfortify)
library(gvlma)
library(car)
X<- baseball
View(X)
attach(X)
rownames(X)<- X[,ncol(X)]
X<- X[,-ncol(X)]
par(mfrow=c(2,2))
plot(homeruns,salary,pch=19,col="red1",panel.first=grid(),ylab="Salario (miles de doláres)",xlab="Número de jonrones")
plot(RBI,salary,pch=19,col="purple4",panel.first=grid(),ylab="Salario (miles de doláres)",xlab="Número de carreras impulsadas")
plot(stolenbases,salary,pch=19,col="aquamarine4",panel.first=grid(),ylab="Salario (miles de doláres)",xlab="Número de bases robadas")
plot(errors,salary,pch=19,col="yellow4",panel.first=grid(),ylab="Salario (miles de doláres)",xlab="Número de errores")
model<- lm(salary~homeruns+RBI+stolenbases+errors,data=X)
autoplot(model)
summary(model)
par(mfrow=c(1,1))
influencePlot(model)
#Evaluación supuestos
#Varianza y linealidad
par(mfrow=c(1,2))
studi<- studres(model)
plot(fitted.values(model),studi,pch=19,panel.first=grid(),col="aquamarine4",ylab="Residuos Estudentizados",xlab="Valores Ajustados",ylim=c(-4,4),main="A")
lines(lowess(studi~fitted.values(model)),col="red1")
abline(h=0,lty=2,lwd=2)
bptest(model,~homeruns+RBI+stolenbases+errors+I(homeruns^2)+I(RBI^2)+I(stolenbases^2)+I(errors^2),data=X)
#Normalidad
shapiro.test(residuals(model))
qqPlot(model,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",main="B",id=F)

#####################################################################
#Correción del problema de heterocedasticidad
#Estimación varianza
res.mcp<- residuals(model)
varianza<- lm(abs(res.mcp)~homeruns+RBI+stolenbases+errors,data=X)
w = 1/fitted.values(varianza)^2
model.ponderados<- lm(salary~homeruns+RBI+stolenbases+errors,data=X,weights = w)
summary(model.ponderados)
#Validación Supuestos
par(mfrow=c(1,2))
res.ponderados<- residuals(model.ponderados)*sqrt(w)
plot(fitted.values(model.ponderados),res.ponderados,
     xlab='Valores ajustados',ylab='Residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4",ylim=c(-3,3),main="A")
lines(lowess(res.ponderados~fitted.values(model.ponderados)),col=2,lty=2,lwd=4)
abline(h=0,lty=2,lwd=2)
qqPlot(res.ponderados,xlab="Cuantiles Teóricos",ylab="Residuos Ponderados",main="B",id=F)
crPlots(model.ponderados,main="")
shapiro.test(res.ponderados)
hist(res.ponderados)
#################### Identificación de puntos atípicos,balanceo e influyentes
#Puntos de Balanceo, Influyentes y Atípicos
par(mfrow=c(1,1))
p<- length(coefficients(model.ponderados))
n<- nrow(X)
hii.c<- 2*p/n
influencePlot(model.ponderados,panel.first=grid())
hii<- hatvalues(model.ponderados)
hii.ind<- hii[hii>hii.c]
plot(hii,ylab="Valores diagonal de la matriz Hat",pch=19,xlab="Indíces",ylim=c(0,0.3),panel.first=grid())
points((1:nrow(X))[hii>hii.c],hii.ind,col="red",pch=19)
text((1:nrow(X))[hii>hii.c],hii.ind,labels=rownames(X)[(1:nrow(X))[hii>hii.c]],pos=c(1,2,3,3,3,1,3,3,1),cex=0.8)
abline(h=2*p/n,lty=2)
n<- length(residuals(model.ponderados))
p<- length(coefficients(model.ponderados))
hii.c<-2*(p/n)
abline(h=hii.c,lty=2,lwd=2)
indices.1<-(1:nrow(X))[hii<hii.c & abs(res.ponderados)>2.5]
indices.2<-(1:nrow(X))[hii>hii.c & abs(res.ponderados)> qt(0.95,n-p-1)]
indices.3<- (1:nrow(X))[hii>hii.c+0.1& abs(res.ponderados)< 2]
plot(hii,res.ponderados,pch=19,xlab="Valores de la diagonal de la matriz hat", ylab=" Residuos Ponderados",ylim=c(-3,3),xlim=c(0,0.3),panel.first=grid())
abline(h=c(1,0,-1)*2,lty=2,v=hii.c)
points(hii[indices.3],res.ponderados[indices.3],col="yellow",pch=19)
text(hii[indices.3],res.ponderados[indices.3],labels=rownames(X)[indices.3],pos=3)
points(hii[indices.2],res.ponderados[indices.2],col="red",pch=19)
text(hii[indices.2],res.ponderados[indices.2],labels=rownames(X)[indices.2],pos=4)
points(hii[indices.1],res.ponderados[indices.1],col="aquamarine",pch=19)
text(hii[indices.1],res.ponderados[indices.1],labels=rownames(X)[indices.1],pos=c(1,3,4))
legend(x = "topright",legend=c("Influyente","Balanceo","Atípico"),
       col = c("red","yellow","aquamarine"),pch=c(19,19,19),pt.cex=2,
       box.lwd=0.6,title="Identificación de puntos",text.font =15,cex=0.6)
View(X[indices.2,])
indices.6<-which(X$salary>3200 & X$salary<3500)
View(X[indices.6,])
summary(model.ponderados)
############## Distancia de Cook
ck<- cooks.distance(model.ponderados)
plot(ck,ylab="Distancia de Coock",pch=19,ylim=c(min(ck),max(ck)+0.1),panel.first=grid())
ck.c<- 4/n
abline(h=ck.c,lty=2)
indices<- (1:nrow(X))[ck>ck.c]
ck<- ck[ck>ck.c]
points(indices,ck,col="red",pch=19)
text(indices,ck,labels=rownames(X)[indices],pos=3,cex=0.6)
influencePlot(model.ponderados)
########### DfBetas
#Beta 1
par(mfrow=c(2,2))
DFBETAS = dfbetas(model.ponderados)
DFBETAS
plot(DFBETAS[,2],ylab=quote('DFBETA'~(beta[1])),xlab="Indíce",pch=19,ylim=c(-0.4,0.5),xlim=c(0,150),panel.first=grid())
ind = (1:nrow(X))[abs(DFBETAS[,2]) > 2/sqrt(nrow(X))]
dfb = DFBETAS[abs(DFBETAS[,2]) > 2/sqrt(nrow(X)) ,2]
abline(h=c(1,-1)*2/sqrt(nrow(X)))
text(ind,dfb,rownames(X)[abs(DFBETAS[,2]) > 2/sqrt(nrow(X))],pos=c(1,3,1,4,3,2,1,4,3,4),
     cex=0.8)
points(ind,dfb,col="red",pch=19)
head(DFBETAS)
View(X[ind,])
################
#Beta2
plot(DFBETAS[,3],pch=19,ylab=quote('DFBETA'~(beta[2])),ylim=c(-1,1),xlim=c(-30,160),xlab="Indíce",panel.first=grid())
ind = (1:nrow(X))[abs(DFBETAS[,3]) > 2/sqrt(nrow(X))]
dfb = DFBETAS[abs(DFBETAS[,3]) > 2/sqrt(nrow(X)),3]
abline(h=c(1,-1)*2/sqrt(nrow(X)))
text(ind,dfb,rownames(X)[abs(DFBETAS[,3]) > 2/sqrt(nrow(X))],pos=c(2,1,3,1,3,4,1,4))
points(ind,dfb,col="blue",pch=19)
#Beta 3
par(mfrow=c(1,1))
plot(DFBETAS[,4],pch=19,ylab=quote('DFBETA'~(beta[3])),ylim=c(-1,1),panel.first=grid(),xlab="Indíce")
ind = (1:nrow(X))[abs(DFBETAS[,4]) > 2/sqrt(nrow(X))]
dfb = DFBETAS[abs(DFBETAS[,4]) > 2/sqrt(nrow(X)),4]
abline(h=c(1,-1)*2/sqrt(nrow(X)))
text(ind,dfb,rownames(X)[abs(DFBETAS[,4]) > 2/sqrt(nrow(X))],pos=3)
points(ind,dfb,col="yellow4",pch=19)
#Beta 4
plot(DFBETAS[,5],pch=19,ylab=quote('DFBETA'~(beta[4])),ylim=c(-1,1),xlim=c(-30,150),xlab="Indíce",panel.first=grid())
ind = (1:nrow(X))[abs(DFBETAS[,5]) > 2/sqrt(nrow(X))]
dfb = DFBETAS[abs(DFBETAS[,5]) > 2/sqrt(nrow(X)),5]
abline(h=c(1,-1)*2/sqrt(nrow(X)))
text(ind,dfb,rownames(X)[abs(DFBETAS[,5]) > 2/sqrt(nrow(X))],pos=c(1,2,1,3,4,3,2,1))
points(ind,dfb,col="purple4",pch=19)
################ Dffits
par(mfrow=c(1,1))
DFFITS = dffits(model.ponderados)
plot(DFFITS,xlab="Indíces",pch=19,xlim=c(-30,150),ylim=c(-1,1),panel.first=grid())
abline(h=c(-1,1)*2*sqrt(p/n))
ind = (1:nrow(X))[abs(DFFITS) > 2*sqrt(p/n)]
dfb = DFFITS[abs(DFFITS) > 2*sqrt(p/n)]
text(ind,dfb,rownames(X)[abs(DFFITS) > 2*sqrt(p/n)],pos=2)
points(ind,dfb,col="purple4",pch=19)
################ CovRatio
COVR = covratio(model.ponderados)
plot(COVR,pch=19,ylab="Covratio",xlab="Indíce",panel.first=grid())
abline(h=1+c(-1,1)*3*(p/n))
covr = COVR[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
ind = (1:nrow(X))[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n) ]
text(ind,covr,rownames(X)[COVR > 1 +3*(p/n) | COVR < 1 -3*(p/n)],pos=4)
points(ind,covr,col="purple4",pch=19)
vif(model.ponderados)
######### Elimiminación de puntos influyentes
model.inf<- lm(salary~homeruns+RBI+stolenbases+errors,data=X[-indices.2,])
summary(model.inf)
#Evaluación supuestos
#Varianza y linealidad
par(mfrow=c(1,2))
studi<- studres(model.inf)
plot(fitted.values(model.inf),studi,pch=19,panel.first=grid(),col="aquamarine4",ylab="Residuos Estudentizados",xlab="Valores Ajustados",ylim=c(-4,4),main="A")
lines(lowess(studi~fitted.values(model.inf)),col="red1")
abline(h=0,lty=2,lwd=2)
bptest(model.inf,~homeruns+RBI+stolenbases+errors+I(homeruns^2)+I(RBI^2)+I(stolenbases^2)+I(errors^2),data=X[-indices.2,])
#Normalidad
shapiro.test(residuals(model.inf))
qqPlot(model.inf,xlab="Cuantiles Teóricos",ylab="Residuos Estudentizados",main="B",id=F)
###########
res.mcp.1<- residuals(model.inf)
varianza.1<- lm(abs(res.mcp.1)~homeruns+RBI+stolenbases+errors,data=X[-indices.2,])
w = 1/varianza.1$fitted.values^2
model.ponderados.1<- lm(salary~homeruns+RBI+stolenbases+errors,data=X[-indices.2,],weights = w)
summary(model.ponderados.1)
#Validación Supuestos
par(mfrow=c(1,2))
res.ponderados<- residuals(model.ponderados)*sqrt(w)
plot(fitted.values(model.ponderados),res.ponderados,
     xlab='Valores ajustados',ylab='Residuos ponderados',pch=19,panel.first = grid(),col="aquamarine4",ylim=c(-3,3),main="A")
lines(lowess(res.ponderados~fitted.values(model.ponderados)),col=2,lty=2,lwd=4)
abline(h=0,lty=2,lwd=2)
qqPlot(res.ponderados,xlab="Cuantiles Teóricos",ylab="Residuos Ponderados",main="B",id=F)
crPlots(model.ponderados)
shapiro.test(res.ponderados)
hii<- hatvalues(model.ponderados)
##############################
summary(X)
coefficients(model.ponderados)
summary(X)
predict(model.ponderados,data.frame(homeruns=10,RBI=50.5,stolenbases=4,errors=5),interval = 'confidence')
predict(model.ponderados.1,data.frame(homeruns=10,RBI=50.5,stolenbases=4,errors=5),interval='confidence')
X[indices.2,]
summary(model.ponderados)
summary(model.ponderados.1)
