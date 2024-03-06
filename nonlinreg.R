x <- c(1, 2, 3, 4)
y <- c(1, log(2)* exp(20*3), 54, 78)
n <- length(x)


NonLinReg <- function(X, Y) {
  Y <- log(Y)
  a <- CalcA(X, Y)
  b <- CalcB(X, Y)
  plot(X, Y)
  curve(exp(a* x) * exp(b))
}

CalcA <- function(X, Y) {
  sumX <- Summary(X)
  sumY <- Summary(Y)
  sumXY <- Summary(X, Y)
  sumXSquared <- Summary(X, X)
  
  upper <- (sumXY * n) - (sumY * (-sumX))
  lower <- (sumXSquared * n) - (sumX * (-sumX))
  return(upper / lower)
}

CalcB <- function(X, Y) {
  sumX <- Summary(X)
  sumY <- Summary(Y)
  sumXY <- Summary(X, Y)
  sumXSquared <- Summary(X, X)
  
  upper <- (sumX * sumY) - (sumX * sumXY)
  lower <- (sumXSquared * n) - (sumX * (-sumX))
  return(upper / lower)
}

Summary <- function(X, Y) {
  sum <- 0
  
  if (missing(Y)) {
    for (i in seq_along(X)) {
      sum <- sum + X[i]
    }
    return(sum)
  }
  
  for (i in seq_along(X)) {
    sum <- sum + X[i] * Y[i]
  }
  return(sum)
}
NonLinReg(x, y)