#### Andrés Felipe Palomino Universidad del Valle ####
## Monitoria No.3 ##
#### Cargar base de datos y seleccionar muestra al azar ####
library(readxl)
library(ggplot2)
Datos_Rotación <- read_excel("Datos_Rotación.xlsx")
View(Datos_Rotación)
set.seed(1) #Sembrar una semilla para que los resultados sean replicables
ind<-sample(1:1470,100,replace=F)
X<- Datos_Rotación[ind,]
View(X)
attach(X)
#### Estadística Descriptiva ####
summary(Ingreso_Mensual);print("Varianza");var(Ingreso_Mensual)
print("Desviación Estándar");sd(Ingreso_Mensual)
print("Coef Var %"); sd(Ingreso_Mensual)/abs(mean(Ingreso_Mensual))*100
## Gráficos ##
par(mfrow=c(1,3))

# con ggplot

ggplot(X,aes(y=X$Ingreso_Mensual,x=X$Genero))+geom_boxplot(fill=c("red","blue"))+theme_classic()

#-- Polígono de frecuencia


ggplot(X, aes(Ingreso_Mensual, colour = Genero)) +geom_freqpoly()+theme_classic()+xlab("Ingresos")+ylab("Frecuencia")

#-- Ojiva

ggplot(X, aes(Ingreso_Mensual, colour = Genero)) +geom_step(stat = "ecdf")+theme_classic()+xlab("Ingresos")+ylab("Frecuencia")
#### Parte De tablas cruzadas ####
tabla_cruzada<- data.frame(table(Genero,Estado_Civil))
# Ejemplo de dataframe con variables genéricas
datos <- data.frame(
  estado_civil = c("Casado", "Soltero", "Casado", "Soltero", "Divorciado"),
  genero = c("Hombre", "Mujer", "Mujer", "Hombre", "Hombre")
)

# Cargar el paquete ggplot2 (si aún no está cargado)
# library(ggplot2)

# Tabla cruzada
# Tabla cruzada
tabla_cruzada <- prop.table(table(X$Estado_Civil, X$Genero))

barplot(tabla_cruzada,beside=T,legend=rownames(tabla_cruzada),col=c("aquamarine4","orange4","red4"),panel.first=grid())
#### Dispersión ####
plot(Porcentaje_aumento_salarial~Ingreso_Mensual,pch=19)
ggplot(X,aes(x=Porcentaje_aumento_salarial,y=Ingreso_Mensual))+geom_point()+geom_point(aes(colour=Campo_Educación))
