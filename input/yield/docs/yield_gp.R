## This is a script supporting the yield project for the RSM & Compare
## Class by Robert B. Gramacy, Statistics Dept/Virginia Tech
##
## It was run during the GP module to illustrate a potential role for 
## the GP in optimiziation.  This is what is sometimes called surrograte
## based optimization.

## read in the data and convert to the full-replicate/record format
data <- read.table("rbg.txt", header=TRUE)
nna <- sum(!is.na(data[,9:18]))
D <- matrix(NA, nrow=nna, ncol=7)
y <- rep(NA, nna)
k <- 1
for(i in 1:nrow(D)) {
	for(j in 1:10) {
		if(is.na(data[i,8+j])) { break; }
		D[k,] <- as.numeric(data[i,2:8])
		y[k] <- data[i,8+j]
		k <- k+1
	}
}
colnames(D) <- names(data)[2:8]

## convert to coded inputs
r <- apply(D, 2, range)
X <- as.data.frame(D)
for(j in 1:ncol(X)) X[,j] <- 2*(X[,j] - r[1,j])/(r[2,j] - r[1,j])-1
names(X) <- paste("x", 1:ncol(r), sep="")

## use GP subroutines from the laGP library
library(laGP)

## fit the data and obtain MLE for hyperparameters
eps <- sqrt(.Machine$double.eps)
gpi <- newGPsep(X, y, d=0.1, g=0.1*var(y), dK=TRUE)
mle <- jmleGPsep(gpi, drange=c(eps, 100))

## predictive mean objective
obj <- function(par)
  {
  	p <- predGPsep(gpi, matrix(par, ncol=ncol(X)), lite=TRUE)
  	return(-p$mean)
  }

## starting locations from current best value
start <- X[which.max(y),]

## This defines the bounding box of optim search. I started with explore 
## set to 1.1, and then decreased it in several steps to 0.9
explore <- 0.9
lower <- rep(-explore, ncol(X))
if(explore > 1) lower[r[1,] == 0] <- -1
upper <- rep(explore, ncol(X))

## search via optim
out <- optim(start, obj, method="L-BFGS-B", lower=lower, upper=upper)

## transform back to natural inputs for running in the yield shiny app
print((out$par + 1) * (r[2,] - r[1,])/2 + r[1,])

## I sourced this file several times, each time after performing the
## printed run in the shiny app, at first with several replicates and
## ultimately with just one.  I stopped when the process stopped
## providing actual improvements.