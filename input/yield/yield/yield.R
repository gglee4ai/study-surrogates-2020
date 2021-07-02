yield <- function(N, P, K, Na, Ca, Mg)
	{
		l1 <- 0.015 + 0.0005*N + 0.001*P + 1/((N+5)*(P+2)) + 0.001*K + 0.1/(K+2)
		l2 <- 0.001*((2 + K + 0.5*Na)/(Ca + 1)) + 0.004*((Ca + 1)/(2 + K + 0.5*Na))
		l3 <- 0.02/(Mg + 1)

		return(1/(l1 + l2 + l3))
	}

yield.fn <- function(X)
{
	if(is.null(ncol(X))) {
		if(length(X) != 6) stop("X should have length 6")
		X <- matrix(X, nrow=1)
	}

	if(is.matrix(X)) {
		if(ncol(X) != 6) stop("X should have 6 cols")
		if(is.null(colnames(X))) colnames(X) <- formalArgs(yield)
		X <- as.data.frame(X)
	}

	## data frame
	if(is.data.frame(X)) {
		nms <- formalArgs(yield)
		if(all(names(X) != nms)) {
			print(nms)
			stop("must provide data frame or matrix with colums matching above")
		}
	}

	yield(X[,1], X[,2], X[,3], X[,4], X[,5], X[,6])
}