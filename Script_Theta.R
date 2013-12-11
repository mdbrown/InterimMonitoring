# In 'Script.R' we performed the interim monitoring by looking at confidence bounds about a3, 
# so that for an expected alternative a3 = a_exp we only stop enrolling if the ci for a3 doesn't include 
# a_exp. This would force us to choose an expected value of the interaction coefficent, which isn't easy
# to interpret. If however, we base our monitoring on the ci for Theta, that it is larger than some value
# (say 0.01 for example), this may provide a more natural rule for stopping enrollment. 

# this script performs the interim monitoring using theta as a stopping rule instead of a3, as in the other
# script. 



source("SimulationCode.R")

# null marker

bootstraps = 500

# weak marker theta = 
#sample size needed for criteria 1 
# 80% power 800, 90% power 1050

mybeta <- c(-1.23, -0.09, .6, -0.5)  
mymean <-  0; 
mysd <- 1
square <- FALSE

# strong marker
#sample size needed for criteria 1 
# 80% power 100, 90% power 150
mybeta <- c(-1.2402598, -0.6910426, 0.6, -2.25)
mybeta <-  c( -1.24, -0.29,  0.6, -1.5)
mymean <- 0; mysd <- 1
square <- FALSE
calcTruth(a=mybeta, f = dnorm)
#onc-dx marker 
#sample size needed for criteria 1 
# 80% power 1475, 90% power 1975

mybeta <-  c(-2.236,.4, .037,-0.0192)
mymean <- 4.84; mysd <- 1.8
square <- TRUE
calcTruth(a=mybeta, f = f.y.onc, lower.bound = 0, upper.bound = Inf)


#null marker 
mybeta<-c(-1.23, -0.09, .6, 0)  
mymean <- 0; mysd <- 1
square <- FALSE





calcTruth(a=mybeta, f=dnorm)$value


#Simulate Data

tstdata <- SimulateData(n = 1000, a = mybeta, mu = mymean, sd = mysd, square = square)
myBetaHat <- EstCoefs(tstdata)

bootEstTheta(tstdata)

Theta <- estTheta(tstdata$marker, estEta(tstdata))


EstimateTheta(tstdata, bootstraps = 100)
myBetaHat
mybeta



# run a small simulation where the marker has null performance, 
#but we expect it to have the performance of the weak sim marker
M = 1000
altVal <- 0.01 #we expect the performance of the weak sim marker
startN <- 100
endN <- 1050

output.IM   <- data.frame(matrix(nrow = M, ncol=4))
output.ALL  <- data.frame(matrix(nrow = M, ncol=4))                      
names(output.IM) <- c("est", "low", "high", "N")
names(output.ALL) <- c("est", "low", "high", "N")


for( m in 1:M){
  
  #run a single simulation
  mydata <-  SimulateData(n = endN, a = mybeta, mu = mymean, sd = mysd, square = square)
  
  N = startN -25
  keepGoing <- TRUE
  
  while(keepGoing){
    N <- N + 25
    tmpdata <- mydata[1:N, ]
    
    
    tmpest <- EstimateTheta(tmpdata, bootstraps = 500)
    
    #if the ci covers the alt value we keep going, otherwise we stop
    
    if(!cover(low = tmpest$ci[1], high = tmpest$ci[2], val = altVal )) keepGoing = FALSE
    if(N>=endN) keepGoing = FALSE
    # print(paste("simulation", m, "| sample size", N))
  }
  
  output.IM[m, ] <- c(tmpest$thetaEst, tmpest$ci,  N)
  
  if(N != endN) tmpest <- EstimateTheta(mydata, bootstraps = 500)
  output.ALL[m, ] <- c(tmpest$thetaEst, tmpest$ci, endN )
  
  
  print(m)
}


save(output.IM, output.ALL, file = "nullTrue_weakAlt_Theta.Rdata")

#now the truth will be the weak marker and we will see how much interim monitoring reduces power

M = 1000
altVal <- -0.5
startN <- 100
endN <- 1050

output.IM   <- data.frame(matrix(nrow = M, ncol=5))
output.ALL  <- data.frame(matrix(nrow = M, ncol=5))                      
names(output.IM) <- c("est", "low", "high", "p.value", "N")
names(output.ALL) <- c("est", "low", "high", "p.value", "N")


for( m in 1:M){
  
  #run a single simulation
  mydata <-  SimulateData(n = endN, a = mybeta, mu = mymean, sd = mysd, square = square)
  
  N = startN -25
  keepGoing <- TRUE
  
  while(keepGoing){
    N <- N + 25
    tmpdata <- mydata[1:N, ]
    
    
    tmpest <- EstCoefs(tmpdata)
    
    #if the ci covers the alt value we keep going, otherwise we stop
    
    if(!cover(low = tmpest$ci[4,1], high = tmpest$ci[4,2], val = altVal )) keepGoing = FALSE
    if(N>=endN) keepGoing = FALSE
    # print(paste("simulation", m, "| sample size", N))
  }
  
  output.IM[m, ] <- c(tmpest$coefs[4], tmpest$ci[4,], tmpest$p.value[4], N)
  
  if(N != endN) tmpest <- EstCoefs(mydata)
  
  output.ALL[m, ] <- c(tmpest$coefs[4], tmpest$ci[4,], tmpest$p.value[4], endN )
  
  
  print(m)
}


save(output.IM, output.ALL, file = "weakTrue_nullAlt.Rdata")
