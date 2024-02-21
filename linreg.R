linReg <- function(X,Y) {
  plot(X,Y);
  SumA1 = 0;
  n = length(X);
  for (i in 1:n){
    SumA1 = SumA1 + (X[i] - atlag(X)) * (Y[i] - atlag(Y));
  }
  
  SumA2 = 0;
  n = length(X);
  for (i in 1:n){
    SumA2 = SumA2 + (X[i] - atlag(X))^2;
  }
  a = SumA1 / SumA2;
  b = atlag(Y) - (a * atlag(X));
  
  lines(X, a*X+b, type = "l", lty = 1, col="blue")

}

atlag <- function(x) {
  Sum = 0;
  for (i in 1:length(x)){
    Sum = Sum + x[i];
  }
  return (Sum / length(x));
}