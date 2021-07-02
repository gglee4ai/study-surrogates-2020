## This is a script supporting the yield project for the RSM & Compare
## Class by Robert B. Gramacy, Statistics Dept/Virginia Tech
##
## It was run during the IECI module to illustrate a potential role for 
## the GP with IECI in optimiziation.  

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

## negate the y-values because EI wants to minimize
y <- -y

## fit the data and obtain MLE for hyperparameters
library(laGP)
eps <- sqrt(.Machine$double.eps)
gpi <- newGPsep(X, y, d=0.1, g=0.1*var(y), dK=TRUE)
da <- darg(list(mle=TRUE, max=100), X)
ga <- garg(list(mle=TRUE), y)
mle <- jmleGPsep(gpi, drange=c(da$min, da$max), grange=c(ga$min, ga$max), dab=da$ab, gab=da$ab)

## predict at the input locations to de-noise
ym <- predGPsep(gpi, X, lite=TRUE)$mean

obj.IECI <- function(x, fmin, gpi, Xref) ieciGPsep(gpi, matrix(x, nrow=1), fmin, Xref)

library(tgp) ## for lhs with bounding box
IECI.search <- function(X, y, gpi, nref=1000, lower=0, upper=1, multi.start=floor(10*sqrt(ncol(X))))
 {
    m <- which.min(y)
    fmin <- y[m]
    Xref <- lhs(nref, cbind(lower,  upper))
    start <- matrix(X[m,], nrow=1)
    if(multi.start > 1) 
        start <- rbind(start, lhs(multi.start-1, cbind(lower, upper)))
    xnew <- matrix(NA, nrow=nrow(start), ncol=ncol(X)+1)
    for(i in 1:nrow(start)) {
        sv <- ieciGPsep(gpi, start[i,,drop=FALSE], fmin, Xref)
    	if(abs(sv) <= sqrt(.Machine$double.eps))
    		{ out <- list(value=-Inf); next }
        out <- optim(start[i,], obj.IECI, method="L-BFGS-B", 
            lower=lower, upper=upper, gpi=gpi, fmin=fmin, Xref=Xref)
        xnew[i,] <- c(out$par, -out$value)
    }
    solns <- data.frame(cbind(start, xnew))
    names(solns) <- c(paste("s", 1:ncol(X), sep=""), paste("x", 1:ncol(X), sep=""), "val")
    solns <- solns[!is.na(solns$val) & (abs(as.numeric(solns$val)) > sqrt(.Machine$double.eps)),]
    return(solns)
}

## Keeiping explore set to widen the space by 10%
explore <- 1.1
lower <- rep(-explore, ncol(X))
if(explore > 1) lower[r[1,] == 0] <- -1
upper <- rep(explore, ncol(X))

## search via optim
## solns <- EI.search(X, y, gpi, lower=lower, upper=upper)
solns <- IECI.search(X, ym, gpi, lower=lower, upper=upper)
m <- which.max(solns$val)
x <- as.numeric(as.matrix(solns[m,ncol(X)+(1:ncol(X))]))

## transform back to natural inputs for running in the yield shiny app
print(round((x + 1) * (r[2,] - r[1,])/2 + r[1,], 6))

## in case I decide to run it off-line
## (probably not a good idea to pass this to students)
# xm <- matrix((x + 1) * (r[2,] - r[1,])/2 + r[1,], nrow=1)
# colnames(xm) <- c("N", "P", "K", "Na", "Ca", "Mg", "Nx")
# write.table(xm, file="newruns.txt", row.names=FALSE, quote=FALSE)

## I sourced this file several times, each time after performing the
## printed run in the shiny app, at first with several replicates and
## ultimately with just one.  I stopped when the process stopped
## providing actual improvements.