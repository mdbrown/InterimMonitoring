#script to test code as I write it

# null marker

expit = function(x) exp(x)/(1+exp(x))

bootstraps = 500

# weak marker
marker.type <- "weak"
#alpha.onc <- c(-2.236,.4, .037, -0.0192)
a<-c(-1.23, -0.09, .6, -0.5)  
mymean = 0; mysd = 1
square = FALSE

# strong marker
marker.type <- "strong"
#alpha.strong <- c( -1.2402598, -0.6910426,  0.6, -2.25) 
a <-  c( -1.24, -0.29,  0.6, -1.5)
mymean <- 0; mysd <- 1
square <- FALSE




#SimulateData

