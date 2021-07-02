source("yield.R")


## optimizing the non-noisy version
out <- optim(runif(6)*100, yield.fn, lower=rep(0,6), upper=rep(100,6), method="L-BFGS-B",
	control=list(fnscale=-1))
print(out)

## check optimzlity of Na Ca relationship
c(4*out$par[5], 16 + out$par[4])

## study response near optimum

po <- out$par

par(mfrow=c(2,4))

## varying over N from the opt
Ns <- seq(0,20,length=100)
XNs <- data.frame(N=Ns, P=po[2], K=po[3], Na=po[4], Ca=po[5], Mg=po[6])
YNs <- yield.fn(XNs)
plot(Ns, YNs, type="l")
points(Ns, YNs + rnorm(length(YNs), sd=sqrt(0.1)), pch=18)

## varying over K from the opt
Ps <- seq(0,20,length=100)
XPs <- data.frame(N=po[1], P=Ps, K=po[3], Na=po[4], Ca=po[5], Mg=po[6])
YPs <- yield.fn(XPs)
plot(Ps, YPs, type="l")
points(Ps, YPs + rnorm(length(YPs), sd=sqrt(0.1)), pch=18)

## Joint pver N and P
g <- expand.grid(Ns, Ps)
XNPs <-data.frame(N=g[,1], P=g[,2], K=po[3], Na=po[4], Ca=po[5], Mg=po[6])
YNPs <- yield.fn(XNPs)
image(Ns, Ps, matrix(YNPs, nrow=length(Ns)), col=heat.colors(128))

## varying over K from the opt
Ks <- seq(0,20,length=100)
XKs <- data.frame(N=po[1], P=po[2], K=Ks, Na=po[4], Ca=po[5], Mg=po[6])
YKs <- yield.fn(XKs)
plot(Ks, YKs, type="l")
points(Ks, YKs + rnorm(length(YKs), sd=sqrt(0.1)), pch=18)

## Joint over P and K
g <- expand.grid(Ps, Ks)
XPKs <-data.frame(N=po[1], P=g[,1], K=g[,2], Na=po[4], Ca=po[5], Mg=po[6])
YPKs <- yield.fn(XPKs)
image(Ps, Ks, matrix(YPKs, nrow=length(Ps)), col=heat.colors(128))

## varying Na from the opt
Nas <- seq(0,150,length=100)
XNas <- data.frame(N=po[1], P=po[2], K=po[3], Na=Nas, Ca=po[5], Mg=po[6])
YNas <- yield.fn(XNas)
plot(Nas, YNas, type="l")
points(Nas, YNas + rnorm(length(YNas), sd=sqrt(0.1)), pch=18)

# Joint over Na and Ca
Cas <- seq(0,150,length=100)
g <- expand.grid(Nas, Cas)
XNaCas <-data.frame(N=po[1], P=po[2], K=po[3], Na=g[,1], Ca=g[,2], Mg=po[6])
YNaCas <- yield.fn(XNaCas)
image(Nas, Cas, matrix(YNaCas, nrow=length(Cas)), col=heat.colors(128))

## varying Mg from the opt
Mgs <- seq(0,100,length=100)
XMgs <- data.frame(N=po[1], P=po[2], K=po[3], Na=po[4], Ca=po[5], Mg=Mgs)
YMgs <- yield.fn(XMgs)
plot(Mgs, YMgs, type="l")
points(Mgs, YMgs + rnorm(length(YNas), sd=sqrt(0.1)), pch=18)