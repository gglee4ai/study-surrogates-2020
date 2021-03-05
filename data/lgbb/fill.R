library(tgp)

as <- read.table("lgbb_as.txt", header=TRUE)
rest <- read.table("lgbb_as_rest.txt", header=TRUE)
plan <- read.table("lgbb_as_planned.txt", header=TRUE)
XX <- rest[,-1]

responses <- names(as)[-(1:4)]
fill <- matrix(NA, nrow=nrow(plan), ncol=length(responses))
fill <- as.data.frame(fill)
names(fill) <- responses

for(r in responses) {
	X <- as[,2:4]
	Z <- as[r]
	out <- btgpllm(X=X, Z=Z, XX=XX, bprior="b0", BTE=c(10000,20000,100), 
		linburn=TRUE, R=100)

	fill[as[,1],r] <- out$Zp.mean
	fill[rest[,1],r] <- out$ZZ.mean

	save(fill, file="fill.RData")
}
