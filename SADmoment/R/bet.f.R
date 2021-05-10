bet.f <-
function(n,N)
{
	tmp<-N
	if(n!=0)
		for(i in 1:n)
			tmp<-tmp*(N^2-n^2)
	tmp<-tmp/(2*n+1)
	return(sqrt(tmp))
}
