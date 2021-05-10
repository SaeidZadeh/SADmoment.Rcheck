PGmom <-
function(al,bet,k)
{
	s<-0;
	for(i in 1:k)
		s<-s+st2k(k,i)*mmoment(al,bet,i);
	s<-s*((log(2))^k);
	return(s);
}
