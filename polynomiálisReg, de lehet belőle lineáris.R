X <- c(1, 2, 3, 4)
Y <- 5 * X^2 + 3 * X + 7
n <- length(X)

summa <- function(x) {
  summa <- 0
  for (item in x) {
    summa <- summa + item
  }
  return(summa)
}

calcMatrix <- function(values, nrow, byrow = TRUE) {
  ncol <- length(values) / nrow
  if (length(values) %% nrow != 0) {
    stop("A megadott értékek száma nem osztható egyenlően a sorok számával.")
  }
  
  mat <- matrix(nrow = nrow, ncol = ncol)
  
  if (byrow) {
    for (i in 1:nrow) {
      for (j in 1:ncol) {
        mat[i, j] <- values[(i - 1) * ncol + j]
      }
    }
  } else {
    for (j in 1:ncol) {
      for (i in 1:nrow) {
        mat[i, j] <- values[(j - 1) * nrow + i]
      }
    }
  }
  
  return(mat)
}

solveMatrix <- function(A, b) {
  Ab <- cbind(A, b)
  n <- ncol(A)
  for (i in 1:n) {
    Ab[i,] <- Ab[i,] / Ab[i,i]
    for (j in 1:n) {
      if (i != j) {
        Ab[j,] <- Ab[j,] - Ab[i,] * Ab[j,i]
      }
    }
  }
  return(Ab[,ncol(Ab)])
}

X_matrix <- calcMatrix(c(
  n, summa(X), summa(X^2),
  summa(X), summa(X^2), summa(X^3),
  summa(X^2), summa(X^3), summa(X^4)
), nrow = 3, byrow = TRUE)

Y_matrix <- c(summa(Y), summa(X*Y), summa(X^2*Y))

beta <- solveMatrix(X_matrix, Y_matrix)

beta
