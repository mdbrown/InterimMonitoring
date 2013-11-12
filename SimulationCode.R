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
  out <- data.frame("event" = D, "trt" = trt, "marker" = Y)
  return(out)
  
} 
  
#calculate the true value of theta given logistic pars


