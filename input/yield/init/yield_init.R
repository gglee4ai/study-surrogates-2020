source("../yield/yield.R")

## design <- read.table("yield_init_design.txt", header=TRUE)
design <- read.table("newruns.txt", header=TRUE)

start.wk <- 38
## zero.wk <- 37 ## 20
zero.wk <- 46 ## put the current week here
## reps <- rep(5, nrow(design)) ## for the actual zero week
reps <- 5 ## c(2,2,2,2) ## custom version for current week

y <- yield.fn(design[,-7])

Y <- matrix(NA, nrow=nrow(design), 10)
var <- 0.1+0.05*(cos(2*pi*(zero.wk-start.wk)/10)+1)
for(i in 1:nrow(design)) {
	Y[i,1:reps[i]] <- rnorm(reps[i], y[i], sd=sqrt(var))
}

df <- cbind(zero.wk, design, Y)
names(df) <- c("week", formalArgs(yield), "Nx", paste("y", 1:10, sep=""))

write.table(file="newruns_out.txt", df, quote=FALSE, row.names=FALSE)

## After init, the files should be either moved into the 
## ../yield directory for local execution, or to the dropbox
## yield folder for shinyapps.io execution
