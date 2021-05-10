PLNmom <-
function(mu,sig,k)
{
	s<-0;
	for(i in 1:k)
		s<-s+st2k(k,i)*cmoment(mu,sig,i);
	return(s);
}
