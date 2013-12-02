#code to run simulations for interim monitoring


#simulate data 

SimulateData <- function( n, a, mu, sd, square=TRUE){
  
  a0 <- a[1] 
  a1 <- a[2]
  a2 <- a[3]
  a3 <- a[4]
  
  #make sure we have both cases and controls
  getSample <- TRUE
  while(getSample){
    Y <- rnorm(n, mean = mu, sd = sd)
    if(square) Y <- Y^2 
    trt <- rbinom(n,size=1,prob=0.5) #randomly assign treatment/non treatment
    D   <- numeric(n)
    D[trt == 0] <- rbinom(sum(trt==0),size=1,prob=expit(a0      + a2*Y[trt==0]))
    D[trt==1]   <- rbinom(sum(trt==1),size=1,prob=expit(a0 + a1 + a2*Y[trt==1] + a3*Y[trt==1]))
    
    getSample <- ifelse(all(table(D,trt)) > 0, FALSE, TRUE)
    if(getSample) print("Had to resample")
  }
  out <- data.frame("event" = D,"marker" = Y, "trt" = trt)
  return(out)
  
} 
 


expit = function(x) exp(x)/(1+exp(x))

#calculate the true value of theta given logistic pars


#calculate the ci for alpha

#estimate the coefficients in the logistic model
EstCoefs= function (X){
 
  sum.coefs = summary(glm(event~trt*marker, data =X,  family=binomial()))$coef;
  #pars = mean(X[X[,3]==1,1]) - mean(X[X[,3]==0,1])

  return (
    list(coefs = sum.coefs[,1], 
         ci = sum.coefs[,1] + cbind(-1.96*sum.coefs[,2], +1.96*sum.coefs[,2]), 
         p.value = sum.coefs[,4]) 
  )
}

cover <- function(low, high, val) return(low < val & high > val)





