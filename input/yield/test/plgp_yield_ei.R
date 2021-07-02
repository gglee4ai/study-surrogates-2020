## Active Learning for for regression by expected 
## improvement using Particle Learning on a simple 2-d
## exponential function

## load the plgp library
library(plgp)
library(tgp)
library(akima)
source("../yield/yield.R")

## close down old graphics windows and clear session
graphics.off()

## set up 2-d data; generation of Ys and Xs
fn <- function(X){ 0.0 - yield.fn(X) + rnorm(nrow(X), sd=0.1) }
formals(data.GP.improv)$f <- fn

## set bounding rectangle for araptive sampling
rect <-  rbind(c(0,100),c(0,100),c(0,100),c(0,100),c(0,100),c(0,100))
formals(data.GP.improv)$rect <- rect

## set a default prior
prior <- prior.GP(6, cov="separable")
formals(data.GP.improv)$prior <- prior
formals(data.GP.improv)$oracle < FALSE

## use a small LHS candidate set
formals(data.GP.improv)$cands <- 1000

## use akima for interp
library(akima)
formals(data.GP.improv)$interp <- interp

## set up start and end times
start <- 25
end <- 100

## do the particle learning
out <- PL(dstream=data.GP.improv, ## adaptive design PL via EI
          start=start, end=end,
          init=draw.GP,  ## init with Metropolis-Hastings
          lpredprob=lpredprob.GP, propagate=propagate.GP,
          prior=prior, addpall=addpall.GP, params=params.GP)

## design a grid of predictive locations
XX <- dopt.gp(200, Xcand=lhs(200*10, rect))$XX
XXs <- rectscale(XX, rect)

## sample from the particle posterior predictive distribution
outp <- papply(XX=XXs, fun=pred.GP, quants=TRUE, prior=prior)

## extract the mean of the particles predictive
m <- rep(0, nrow(as.matrix(XX)))
for(i in 1:length(outp)) m <- m + outp[[i]]$m
m <- m / length(outp)

## extract the quantiles of the particles predictive
q2 <- q1 <- rep(0, nrow(as.matrix(XX)))
for(i in 1:length(outp)) {
  q1 <- q1 + outp[[i]]$q1
  q2 <- q2 + outp[[i]]$q2
}
q1 <- q1 / length(outp)
q2 <- q2 / length(outp)

## unscale the data locations
X <- rectunscale(PL.env$pall$X, rect)

## plot the summary stats of the predictive distribution
par(mfrow=c(1,2))
image(interp.loess(XX[,1], XX[,2], m))
points(X)
image(interp.loess(XX[,1], XX[,2], q2-q1))
points(X)

## look at a historgram of the parameters
params <- params.GP()
dev.new()
par(mfrow=c(1,2)) ## two plots
## hist(params$d)
hist(params$g)

## plot EI progress
dev.new()
par(mfrow=c(1,2)) ## two plots
## plot the sampled points over time
plot(X[,1], main="sampled points",
     xlab="t", ylab="x1 & x2")
abline(v=start, col=3, lty=3)
lines((start+1):end, PL.env$psave$xstar[,1])
points(X[,2], col=2, pch=18)
lines((start+1):end, PL.env$psave$xstar[,2], col=2, lty=2)
legend("topright", c("x1", "x2"), col=1:2, pch=c(21,18))
## plot the max log ei over time
plot((start+1):end, PL.env$psave$max.as, type="l", xlab="t",
     ylab="max log EI", main="progress meter")
