DICNORM<- function(y,postDraws){
  y <- y
  postDraws <- postDraws
  
  thetaBayes <- mean(postDraws)
  
  logLikelihood <- function(theta) sum(dnorm(y,thetaBayes,sd, log=T))
  
  pDIC <- 2*(logLikelihood(thetaBayes) - mean(sapply(postDraws, logLikelihood) ))
  dic <- -2*logLikelihood(thetaBayes) + 2*pDIC
  return(dic)
}
x<- rnorm(10,90,2)
sd<-2
u<- rnorm(10000,90,2)
l<- rnorm(10000,91,2)
DICNORM(x,u)
DICNORM(x,l)
