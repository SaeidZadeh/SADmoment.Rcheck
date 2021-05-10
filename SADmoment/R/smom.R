smom <-
function(mu,sig,k)
{
	s<-0;
	if(k>=2)
		for(i in seq(2,k,2))
			s<-s+choose(k,i)*(mu^(k-i))*(sig^i)*dbfact(i-1);
	return(s)
}
