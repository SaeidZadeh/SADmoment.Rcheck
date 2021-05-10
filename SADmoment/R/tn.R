tn <-
function(n,N)
{
	if(n==0)
		return(1/bet.f(n,N))
	return(-1*sqrt((N-n)/(N+n))*sqrt((2*n+1)/(2*n-1))*tn(n-1,N))
}
