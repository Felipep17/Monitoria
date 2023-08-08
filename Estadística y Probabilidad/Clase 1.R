#### Monitoria de Estadística y Probabilidad ####
# Andrés Felipe Palomino Montezuma - Estadística - Universidad del Valle #
### Clase 1: Introducción a R-Studio ###
#### Introducción ####
# R trabaja como una cálculadora #
2+2
7/0
2-10
2*2
#Podemos crear objetos para guardar elementos de diferentes clases
x<- 2
y<- 2
# Puedo operar los objetos
x+y
z<- x+y
# También guardar carácteres
u<- "Hola"
print(u)
#### Tipos de Elementos ####
x1<- c(1,2,4,6);x1
#Seleccionar elementos de un vector
x1[1]
#Puedo operar los vectores
x1<- x1+1
z2<- c("Hola","","¿Como Estás?"); print(z2)
class(x1);class(z2)
#Pero debe tener sentido la operación
x1+z2
# Ahora un objeto que cuenta con elementos númericos y de cáracter 
# Guarda su clase como cáracter
y1<- c(2,"Hola");y1;class(y1)
matrix<- matrix(0,3,2,byrow=T)
matrix;View(matrix)
lista<- list()
lista[[1]]<- "Hola"
lista[[2]]<- 2;class(lista)
lista
# Puedo operar listas respetando su selección de elementos
lista[[1]]+lista[[1]]
lista[[2]]+lista[[2]]
#### Manipulación de matrices ####
matrix[,1] #Seleccionar Columna 1
matrix[1,] #Seleccionar Fila 1
matrix[1,1] # Seleccionar elemento de la Fila 1 y Columna 1
matrix[1,1]<- 10 #Sobreescribir elemento de la matrix
#Operación matrices#
#install.packages("matrixcalc")
matrix2<- matrix(c(1,0,0,4),nrow=2,ncol=2,byrow=T)
library(matrixcalc)
matrix.power(matrix2,2)
dim(matrix);dim(matrix2)
matrix%*%matrix2
####Operadores logicos ####
ejercicio<- c(-10,8,9,10,15,19,20,12,0,0,0,0,1,12-10,-10,-12)
ejercicio>0
ejercicio<0
ejercicio!=0
ejercicio[ejercicio>0]
#Evaluar datos faltantes
is.nan(log(ejercicio))
#Es suficiente?
h<-na.omit(log(ejercicio))
#Correcta inspección
h[h!=-Inf]
#### Secuencias ####
seq1<- 1:10; seq1
seq2<-seq(1,10,length=10); seq2
seq3<- c(1,2,3,4,5,6,7,8,9,10); seq3
seq4<- seq(1,10,by=1); seq4
#### Factores ####
factores<- c("Azul","Azul","Rojo","Verde");class(factores);factores
#No es un factor
summary(factores)
factores<- factor(factores);class(factores);factores
summary(factores)
#¿Pero si son ordinales?
factores2<- c(rep(1,5),rep(2,10),5,5,4,3,2,6);class(factores2);factores2
summary(factores2)# ¿Tiene Sentido?
factor()
factores2 <- factor(factores2, levels = c(1, 2, 3, 4, 5, 6),
                    labels = c("Estrato 1", "Estrato 2", "Estrato 3",
                               "Estrato 4", "Estrato 5", "Estrato 6"),
                    ordered = TRUE)
factores2;summary(factores2)
####Importación de bases de datos ####
#install.packages("readxl");install.packages("readr")                                                                                
library(readxl)
CampusCrime <- read_excel("CampusCrime.xlsx")
View(CampusCrime)
X<- data.frame(CampusCrime)
rownames(X)<- X[,1];View(X)
#Eliminar Columnas
X<- X[,-1]
#Clases de las columnas 
str(X)
#Visualización inicial data.frame
head(X)
# resumen
summary(X)
#Es necesario convertir factores. ¿Cómo lo harías?
