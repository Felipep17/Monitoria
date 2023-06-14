library(alr4)
X<- UN11
Names = rownames(UN11)
mod.UN11 = lm(log(fertility)~log(ppgdp)+pctUrban,data=UN11)
plot(log(fertility)~log(ppgdp),data=UN11)
plot(log(fertility)~pctUrban,data=UN11)
plot(log(ppgdp)~pctUrban,data=UN11)
library(mixtools)
library(alr4)
library(depth)
attach(X)
Y<- cbind(log(ppgdp),pctUrban)
clcov<- cov(Y)
clcenter<- as.vector(colMeans(Y))
model<- lm(fertility~log(ppgdp)+pctUrban,data=X)
####################
library(ggfortify)
autoplot(model)
lmtest::bptest(model)
##########################
library(ddalpha)
depth.y<-depth.halfspace(Y,Y,num.directions=10000,seed=1)
sort.depth.Y<-sort(depth.y,decreasin=TRUE,index.return=TRUE)
depth.Y.sort<-sort.depth.Y$x
depth.Y.sort.index<-sort.depth.Y$ix
median=sort.depth.Y$ix[1]
#Gráfica de profundidad tukey general
par(mar=c(5,5,5,5))
par(mfrow=c(1,1))
plot(Y,ylim=c(0,120),xlim=c(4,12),xlab="logppgdp")
points(log(ppgdp)[median],pctUrban[median],pch=19,lwd=2,cex=1,col="aquamarine")
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.1,lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.25,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=clcov,alpha=0.5,lty=3,lwd=3)
plot(Y,ylim=c(0,120),xlim=c(4,12),xlab="logppgdp")
points(log(ppgdp)[median],pctUrban[median],pch=19,lwd=2,cex=1,col="aquamarine")
mixtools::ellipse(mu=clcenter,sigma=matrix(c(2.42,0,0,548.9),ncol=2,byrow=T),alpha=0.1,lty=2,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=matrix(c(2.42,0,0,548.9),ncol=2,byrow=T),alpha=0.25,lty=3,lwd=3)
mixtools::ellipse(mu=clcenter,sigma=matrix(c(2.42,0,0,548.9),ncol=2,byrow=T),alpha=0.5,lty=3,lwd=3)
hii<-hatvalues(model)
p<- length(coefficients(model))
n<- nrow(X)
hii.c<- 2*(p/n)
indices<- (1:nrow(UN11))[hii>hii.c]
points(log(ppgdp)[indices],pctUrban[indices],col="red",pch=19)
text(log(ppgdp)[indices],pctUrban[indices],labels=rownames(UN11)[indices],pos=3)
plot(hii,type="h")
abline(h=hii.c,lty=2)
################################################
par(mfrow=c(1,1))
p<- length(coefficients(model))
n<- nrow(X)
hii.c<- 2*p/n
influencePlot(model,panel.first=grid())
hii<- hatvalues(model)
hii.ind<- hii[hii>hii.c]
plot(hii,ylab="Valores diagonal de la matriz Hat",pch=19,xlab="Indíces",ylim=c(0,0.3),panel.first=grid())
points((1:nrow(X))[hii>hii.c],hii.ind,col="red",pch=19)
text((1:nrow(X))[hii>hii.c],hii.ind,labels=rownames(X)[(1:nrow(X))[hii>hii.c]],pos=c(1,2,3,3,3,1,3,3,1),cex=0.8)
abline(h=2*p/n,lty=2)
n<- length(residuals(model))
p<- length(coefficients(model))
res.ponderados<-studres(model)
hii.c<-2*(p/n)
abline(h=hii.c,lty=2,lwd=2)
indices.1<-(1:nrow(X))[hii<hii.c & abs(res.ponderados)>2]
indices.2<-(1:nrow(X))[hii>hii.c & abs(res.ponderados)> qt(0.95,n-p-1)]
indices.3<- (1:nrow(X))[hii>hii.c & abs(res.ponderados)< 2]
plot(hii,res.ponderados,pch=19,xlab="Valores de la diagonal de la matriz hat", ylab=" Residuos estudentizados",ylim=c(-3,3),xlim=c(0,0.1),panel.first=grid())
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
##########
ind<-which((rownames(X))=="Equatorial Guinea"|(rownames(X))=="North Korea")
model1<- lm(fertility~log(ppgdp)+pctUrban,data=X[-ind,])
autoplot(model1)
autoplot(model)
summary(model)
summary(model1)
####
y<- 2-2*1:100+rnorm(100)
Z<- cbind(1:100,2-2*1:100+rnorm(1000,0,20))
plot(Z)
Z[1:100,2]<- NA
plot(Z)
Z[which(is.na(Z[,2])),2]<- mean(Z[,2],na.rm=T)
plot(Z)
