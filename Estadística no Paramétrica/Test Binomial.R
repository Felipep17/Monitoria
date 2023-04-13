#Test Binomial
#n sera el tamaño de la prueba observada
#theta el valor de la hipotésis
#T.obs es el valor observado
#tipo de prueba:
#1 para bilateral
#2 para unilateral con región de rechazó hacia la derecha (cola superior)
#3 para unilateral con región de rechazó hacia la izquierda (cola inferior)
#Siempre obtendremos el ValorP como criterio de Rechazo
TestSignos(8,4,2)
TestBinomial<-function(n,theta,T.Obs,hipo){
  if(n>20 & hipo==1){
    valorp<- 2*min(pnorm((T.Obs-n*theta+0.5)/sqrt(n*theta*(1-theta))),
                   (1-pnorm((T.Obs-n*theta-0.5)/sqrt(n*theta*(1-theta)))))
  }
  if(n>20 & hipo==2){
    valorp<- 1-pnorm((T.Obs-n*theta-0.5)/sqrt(n*theta*(1-theta)))
    
  }
  if(n>20 & hipo==3){
    valorp<-pnorm((T.Obs-n*theta+0.5)/sqrt(n*theta*(1-theta)))
  }
  if(n<=20 & hipo==1){
    valorp<- 2*min(pbinom(T.Obs,n,theta),1-pbinom(T.Obs-1,n,theta))
  }
  if(n<=20 & hipo==2){
    valorp<- 1-pbinom(T.Obs-1,n,theta)
    
  }
  if(n<=20 & hipo==3){
    valorp<-pbinom(T.Obs,n,theta)
  }
  return(valorp)  
}
#Aplicación del TEST
TestBinomial(16,0.1,13,1)
################################################################################
######## TEST SOBRE CUANTILES###############
#TestSobreCuantiles
#Datos será el vector con los datos que queremos comprobar su cuantil
#Theta sera el párametro de la hipotésis
#cuantil será el valor que queremos coloborar
#hipotésis es el tipo de hipotésis
#1 para bilateral
#2 para unilateral con región de rechazó hacia la derecha (cola superior)
#3 para unilateral con región de rechazó hacia la izquierda(cola inferior)
#Obtenemos el valor p como criterio de decisión

TestCuantiles<- function(datos,theta,cuantil,hipo){
  T1<- as.numeric(length(datos[datos<=cuantil]))
  T2<- as.numeric(length(datos[datos<cuantil]))
  n<- length(datos)
  if(n>20 & hipo==1){
    valorp<- 2*min(pnorm((T1-n*theta+0.5)/sqrt(n*theta*(1-theta))),
                   (1-pnorm((T2-n*theta-0.5)/sqrt(n*theta*(1-theta)))))
  }
  if(n>20 & hipo==2){
    valorp<- 1-pnorm((T2-n*theta-0.5)/sqrt(n*theta*(1-theta)))
    
  }
  if(n>20 & hipo==3){
    t<-0:n
    valorp<- pnorm((T1-n*theta+0.5)/sqrt(n*theta*(1-theta)))
    valorp<-c()
  }
  if(n<=20 & hipo==1){
    valorp<- 2*min(pbinom(T1,n,theta),1-pbinom(T2-1,n,theta))
  }
  if(n<=20 & hipo==2){
    valorp<- 1-pbinom(T2-1,n,theta)
    
  }
  if(n<=20 & hipo==3){
    valorp<-pbinom(T1,n,theta)
  }
  return(valorp)  
}
ejercicio1<- c(189,223,195,160,212,176,231,185,199,213,202,193,174,166,248)
TestCuantiles(ejercicio1,0.75,193,1)
###############################################################
#Test de Signos
#n es el número de interés de los signos sin empates
#tobs es el valor de nuestro evento de interés a coloborar, mayoritariamente de 
# +
#hipo es el tipo de hipotésis
#1 para bilateral
#2 para unilateral con región de rechazó hacia la derecha (cola superior)
#3 para unilateral con región de rechazó hacia la izquierda(cola inferior)
TestSignos<-function(n,T.Obs,hipo){
  if(n>20 & hipo==1){
    valorp<- 2*min(pnorm((T.Obs-n*0.5+0.5)/sqrt(n*0.5*(1-0.5))),
                   (1-pnorm((T.Obs-n*0.5-0.5)/sqrt(n*0.5*(1-0.5)))))
  }
  if(n>20 & hipo==2){
    valorp<- 1-pnorm((T.Obs-n*0.5-0.5)/sqrt(n*0.5*(1-0.5)))
    
  }
  if(n>20 & hipo==3){
    valorp<-pnorm((T.Obs-n*0.5+0.5)/sqrt(n*0.5*(1-0.5)))
  }
  if(n<=20 & hipo==1){
    valorp<- 2*min(pbinom(T.Obs,n,0.5),1-pbinom(T.Obs-1,n,0.5))
  }
  if(n<=20 & hipo==2){
    valorp<- 1-pbinom(T.Obs-1,n,0.5)
    
  }
  if(n<=20 & hipo==3){
    valorp<-pbinom(T.Obs,n,0.5)
  }
  return(valorp)  
}
#Probando el Test
TestSignos(9,8,2)
###################### Test McNemar
TestMcNemar<-function(Tabla){
  T.obs<- Tabla[1,2]+Tabla[2,1]
  if(T.obs>20){
    T1<- ((Tabla[1,2]-Tabla[2,1])^2/(Tabla[1,2]+Tabla[2,1]))
    valorp<- 1-pchisq(T1,1)
  }
  
  if(T.obs<=20){
    T2<- Tabla[1,2]
    valorp<- 2*min(pbinom(T2,T.obs,0.5),1-pbinom(T2-1,T.obs,0.5))
  }
  return(valorp)  
}
Datos<- matrix(c(30,67,10,43),2,2,T)
Datos
Datos1<- matrix(c(14,2,5,2),2,2,T)
Datos1
Datos3<-matrix(c(22,24,18,15),2,2,T)
Datos4<-matrix(c(63,21,4,12),2,2,T)
View(Datos4)
colnames(Datos4)<- c("Democráta(0)","Republicano (1)")
rownames(Datos4)<- c("Democrata(0)","Republicano(1)")
Datos4
Datos4
TestMcNemar(Datos)
TestMcNemar(Datos1)
TestMcNemar(Datos3)
TestMcNemar(Datos4)
#Comprobación
1-pchisq((Datos[1,2]-Datos[2,1])^2/(Datos[1,2]+Datos[2,1]),1)
qchisq(1-0.05,1)
T5<-(Datos[1,2]-Datos[2,1])^2/(Datos[1,2]+Datos[2,1])
T6<-(Datos3[1,2]-Datos3[2,1])^2/(Datos3[1,2]+Datos3[2,1])
TestSignos(8,3,3)
1-pbinom(3-1,8,1/2)

datos<-matrix(c(16,14,13,13,14,6,10,8),2,4,2)
chisq.test(datos)
datos2<- matrix(c(8,4,10,2,7,5),3,2,2)
datos2
chisq.test(datos2)
datos7<-matrix(c(62,30,36,7),2,2,2)
datos7
TestMcNemar(datos7)
datos8<-matrix(c(54,72,83,96,20,6,18,6,17,3,12,0,0,12,14,1,0,10,0,0),5,4,2)
datos8
chisq.test(datos8)
-.