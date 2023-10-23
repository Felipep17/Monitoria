library(readr) #Librería necesarias
Datos <- read_table("C:/Users/sebas/OneDrive/Escritorio/Datos.txt", 
                    col_names = FALSE) #Cargar con la base de datos
View(Datos)
Datos<- t(Datos) # Transponer la matriz 
colnames(Datos)<- Datos[1,] # Renombrar
Datos<- Datos[-1,] # Borrar la primera fila
# 
Datos<- data.frame(Datos)
Datos<-apply(Datos,2,as.numeric)
Datos<- data.frame(Datos)
colnames(Datos)<- c("Estatura","Peso")
## Diagramas de dispersión
#De manera sencilla
plot(Datos$Estatura,Datos$Peso,pch=19,panel.first = grid(),xlab="Peso en Kilogramos",ylab="Estatura en Cm")
library(ggplot2)
library(plotly)
# De manera mas elaborada
h<-ggplot(Datos,aes(x=Estatura,y=Peso))+
  geom_point()+
  theme_minimal()+geom_smooth(method = "lm")
ggplotly(h)  
#
# Ajuste del modelo
model<- lm(Peso~Estatura,data=Datos)
options(scipen=999)
summary(model) #Resumen del modelo
abline(model)
#Realizar predicciones con una confianza del 95%
predict(model,newdata = data.frame(Estatura=c(120,140,180)),interval='confidence')
library(ggplot2);library(reshape2)
cor<- round(cor(Datos),2)
melted_cor <- melt(cor)
l<-ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "purple", high = "aquamarine4",
                       limit = c(-1,1), name=paste0("Correlation ", "Pearson")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())
#Matriz de correlación mas elaborada
ggplotly(l)        
#### Segundo Punto ####
library(readxl)
SegundoPunto <- read_excel("SegundoPunto.xlsx", 
                           col_types = c("numeric", "numeric"))
cor<- round(cor(SegundoPunto),2)
melted_cor <- melt(cor)
l<-ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "purple", high = "aquamarine4",
                       limit = c(-1,1), name=paste0("Correlation ", "Pearson")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())
ggplotly(l)    
h<-ggplot(SegundoPunto,aes(x=Cred,y=Porcentaje))+
  geom_point()+
  theme_minimal()
ggplotly(h)  # Crédito 
model<- lm(Porcentaje~Cred,data=SegundoPunto)
options(scipen=999)
summary(model)
h<-ggplot(SegundoPunto,aes(x=Cred,y=Porcentaje))+
  geom_point()+
  theme_minimal()+geom_smooth(method = "lm")
ggplotly(h)  
predict(model,newdata = data.frame(Cred=c(9800,10000,12000)),interval = 'confidence')
