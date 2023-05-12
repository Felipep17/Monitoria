#Importo la librerC-a
library(readr)
X <- read_csv("C:/Users/sebas/OneDrive/Escritorio/Proyectos/Modelos/Bases de datos/grasa.csv")
head(X)
#Breve descriptivas de la variable de respuesta
pairs(X)
summary(X[,1])
# ExploraciC3n grC!fica de las variables de interC)s para el modelo
#######
par(mfrow=c(2,3))
plot(X$age,X$siri,ylab="Porcentaje de masa corporal",xlab="Edad en aC1os",pch=19,col="blue3",panel.first = grid())
plot(X$bmi,X$siri,ylab="Porcentaje de masa corporal",xlab="IMC Kg/Ms 2",pch=19, col="red1",panel.first = grid())
plot(X$abdomen,X$siri, ylab="Porcentaje de masa corporal",xlab="Circuferencia del abdomen en cm",pch=19, col="#CD1076",panel.first = grid())
plot(X$neck,X$siri,ylab="Porcentaje de masa corporal", xlab="Circuferencia del cuello en cm",pch=19,col="blue3",panel.first = grid())
plot(X$thigh,X$siri,ylab="Porcentaje de masa corporal",xlab="Circuferencia del muslo en cm",pch=19,col="red1",panel.first = grid())
plot(X$hip,X$siri,ylab="Porcentaje de masa corporal",xlab="Circuferencia de la cadera en cm",pch=19,col="#CD1076",panel.first = grid())
#Modelo
modc<- lm(siri~age+bmi+abdomen+neck+thigh+hip,data=X)
library(effects)
plot(allEffects(modc))
summary(modc)
library(car)
#Evaluar multicolinealidad
vif(modc)
#Podemos evidenciar que se presentan problemas de multicolinealidad
#ComparaciC3n de las variables
#Escalono la matriz
y = X$siri
Z = apply(X[,-1],2,function(x){(x-mean(x))/sqrt(sum((x-mean(x))^2))}) #Puede hacerse con Scale
ys = (y-mean(y))/sqrt(sum((y-mean(y))^2))
mod.std = lm(ys~Z-1)
summary(mod.std)
#Reducido
mods<-lm(siri~abdomen+neck+hip,data=X)
summary(mods)
vif(mods)
anova(modc,mods)
anova(mods,modc)
#Predicciones modelo completo
#ExtrapolaciC3n
newPoints = as.data.frame(cbind(x0=rep(1,4),age=c(55,30,40,70),bmi=c(25,23,30,21),abdomen=c(90,110,99,82),
                  neck=c(35,40,35,35),thig=c(60,55,60,49),hip=c(100,88,107,88)))
X. = model.matrix(modc)
XtX.inv = solve(t(X.)%*%X.)
h.values = hatvalues(modc)
hmax = max(h.values)
h0 = apply(newPoints,1,function(x){t(x)%*%XtX.inv%*%x})
h0
h0 >hmax
# Predicciones modelo reducido
newPoints.1 = cbind(x0=rep(1,4),abd=c(90,110,99,82),
                  neck=c(35,40,35,35),hip=c(100,88,107,88))
X.1 = model.matrix(mods)
XtX.inv.1 = solve(t(X.1)%*%X.1)
h.values = hatvalues(mods)
hmax.1 = max(h.values)
h0.1 = apply(newPoints.1,1,function(x){t(x)%*%XtX.inv.1%*%x})
h0.1 >hmax.1
# Escalonando el modelo reducido

Y<- cbind(X$abdomen,X$neck,X$hip)
colnames(Y)<- c("abdomen","neck","hip")
y = X$siri
Z = apply(Y,2,function(x){(x-mean(x))/sqrt(sum((x-mean(x))^2))})
ys = (y-mean(y))/sqrt(sum((y-mean(y))^2))
mod.std = lm(ys~Z-1)
summary(mod.std)
Y.1<- cbind(X$abdomen,X$neck,X$hip)
Y.1<- scale(Y.1)
Y.2<- scale(X$siri)
mod.std.1 = lm(Y.2~Y.1-1)
mod.std.1$coefficients
summary(mod.std.1)
####
summary(modc)
summary(mods)
###########Predicciones summary(mod)
predict(modc,data.frame(age=c(55,40,70),bmi=c(25,30,21),abdomen=c(90,99,82),
                                  neck=c(35,35,35),thigh=c(60,60,49),hip=c(100,107,88)),interval='confidence')
predict(modc,data.frame(age=c(55,40,70),bmi=c(25,30,21),abdomen=c(90,99,82),
                        neck=c(35,35,35),thigh=c(60,60,49),hip=c(100,107,88)),interval='prediction')

predict(mods,data.frame(abdomen=c(90,99,82),
                        neck=c(35,35,35),hip=c(100,107,88)),interval='confidence')

predict(mods,data.frame(abdomen=c(90,99,82),
                        neck=c(35,35,35),hip=c(100,107,88)),interval='prediction')

