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






estEta = function (X){
  y = X[,1]; ## X has form cbind(R, Y, T)
  dMat = cbind (1, X[,2], X[,3], X[,3]*X[,2]);
  etaHat = glm.fit (dMat, y, family=binomial())$coefficients;
  
  pars = mean(X[X[,3]==1,1]) - mean(X[X[,3]==0,1])
  
  return (list(out = etaHat, pars = pars)); 
}


#estimate Theta 
### alternatively you could use fXEtaThetaC for a function that uses C to calculate this (found in ../C/Code.cpp)
estTheta = function (x, eta){

  eta = eta$out
  dPos = cbind(1, x, 1, x);
  dNeg = cbind(1, x, 0, x*0);
  mPos = dPos%*%eta;
  mNeg = dNeg%*%eta;
  deltaVal = exp (mPos)/(1+exp(mPos)) - exp (mNeg)/(1+exp(mNeg));
  
  mean(pmax(deltaVal, 0), na.rm =TRUE);
}

bootEstTheta<- function(X){
  
  sampleind <- sample.int(nrow(X), replace = TRUE)
  
  estTheta(X[sampleind,2], estEta(X[sampleind,]))
  
}


EstimateTheta <- function(X, bootstraps = 500){
  
  bootThetas <- (replicate(bootEstTheta(X), n = bootstraps))
  ci = quantile(bootThetas, probs = c(.025, 0.975), na.rm=TRUE)
  
  theta <- estTheta(X[,2], estEta(X))
  
  list(thetaEst = theta, ci = ci)
}

cover <- function(low, high, val) return(low < val & high > val)

calcTruth <- function(a, f, lower.bound = -10, upper.bound = 10){

 risk.t0 <- function(x) expit(a[1] + a[3]*x)
 risk.t1 <- function(x) expit(a[1] + a[2] + (a[3]+a[4])*x)
 pneg = integrate(f, lower = lower.bound, upper= -a[2]/a[4])
 theta <- integrate(f = function(x, a){(risk.t1(x)-risk.t0(x))*(f(x))*I(x < -a[2]/a[4])}, lower = lower.bound, upper = upper.bound,  a = a)
 
 list(theta = theta$value, 
      pneg = pneg$value, 
      bneg = theta$value/pneg$value)
}


f.y.str = function(y) dnorm(y, mean = 0, sd = 1)
f.y.onc = function(y) (0.5*(y^(-.5)))*(dnorm(sqrt(y), mean=4.84, sd=1.8)+ dnorm(-sqrt(y), mean=4.84, sd=1.8))

