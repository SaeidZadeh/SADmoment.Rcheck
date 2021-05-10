dbfact <-
function(i)
{
	s<-1;
	if(i%%2 == 1)
		for(j in 1:((i+1)/2))
			s<-s*(2*j-1);
	if(i%%2==0)
		for(j in 1:((i)/2))
			s<-s*(2*j);
	return(s);
}
