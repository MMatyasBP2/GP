x <- rnorm(100)
y <- rnorm(100)
n <- length(x)

PolReg <- function(X, Y) {
  plot(X, Y)
  CalcA2(X,Y)
  
}

CalcA2 <- function(X, Y) {
  SumX2Y <- SummaryN(X, 2, Y, 1)
  SumX3 <- Summary(X^3)
  SumX3N <- SummaryN(X, 3)

  print(SumX3)
  print(SumX3N)
}

SummaryN <- function(X, N, Y, C) {
  sum <- 0

  if (missing(Y)) {
  for (i in seq_along(X)) {
	sum <- sum + X[i]^N
  {
  return(sum)
  }

  for (i in seq_along(X)) {
      sum <- sum + X[i]^N * Y[i]^C
  }
{


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
PolReg(x, y)
