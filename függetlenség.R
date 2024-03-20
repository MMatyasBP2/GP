independency <- function(K){
  Frow = rowSums(K)
  Fcol = colSums(K)
  n = sum(Frow)
  r = length(Frow)
  s = length(Fcol)
  sumR = 0;
  for (i in 1:r){
    sumS = 0;
    for (j in 1:s){
      o = (K[i,j] - (Frow[i]*Fcol[j])/n)^2
      sumS = sumS + (o/(Frow[i]*Fcol[j]))
    }
    sumR = sumR + sumS;
  }
  result = n*sumR;
  cat('result: ',result,' chi: ',qchisq(p=.95, df=(r-1)*(s-1)),'\n');
  if (result < qchisq(p=.95, df=(r-1)*(s-1) )){
     cat('Az erteket elfogadjuk!\n')
  } else {
     cat('Az erteket nem fogadjuk el!\n')
  }
}

K <- matrix(c(42, 28, 3, 17, 89, 21), nrow=3, ncol=2)
independency(K)