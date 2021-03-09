## This is a script supporting the yield project for the RSM & Compare
## Class by Robert B. Gramacy, Statistics Dept/Virginia Tech
##
## It was run during the EI module to illustrate a potential role for 
## the GP with EI in optimiziation.  

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

## fit the data and obtain MLE for hyperparameters
library(laGP)
eps <- sqrt(.Machine$double.eps)
gpi <- newGPsep(X, y, d=0.1, g=0.1*var(y), dK=TRUE)
da <- darg(list(mle=TRUE, max=100), X)
ga <- garg(list(mle=TRUE), y)
mle <- jmleGPsep(gpi, drange=c(da$min, da$max), grange=c(ga$min, ga$max), dab=da$ab, gab=da$ab)

## predict at the input locations to de-noise
ym <- predGPsep(gpi, X, lite=TRUE)$mean

yresid <- rep(0, nrow(data))
k <- 1
for(i in 1:nrow(data)) {
    for(j in 1:10) {
        if(is.na(data[i,8+j])) { break; }
        yresid[i] <- yresid[i] + (data[i,8+j] - ym[k])^2
        k <- k+1
    }
    yresid[i] <- yresid[i]/sum(!is.na(data[i,9:(8+10)]))
}
