kesz=function(x)
{
	s=0
 	for (i in 1:length(x))
 	{
		s=s+(x[i]-emp(x))^2
	}
	return(s/(length(x)-1))
}
print(kesz(x))