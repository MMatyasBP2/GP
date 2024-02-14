x = c(1,2,3,4,5)
emp = function(x)
{ s=0
  avg=0
  for (i in 1:length(x))
  {
    s=s+x[i]
  }
  avg=s/length(x)
  return(avg)
}
print(emp(x))
