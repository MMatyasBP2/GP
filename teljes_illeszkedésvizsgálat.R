k <- c(83, 91, 122, 107, 74, 123)
p <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)

N <- 600

Fit <- function(k, p, N) {
	khi <- 0
	n <- length(k)

	for (i in 1 : n) {
		khi <- khi + (((k[i] - N * p[i])^2)/(N * p[i]))
	}
	
	return (khi)
}

Fit(k, p, N)