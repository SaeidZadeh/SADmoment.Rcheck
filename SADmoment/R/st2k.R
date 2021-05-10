st2k <-
function(n,k)
{
	s<-factorial(k);
	z<-0;
	for(j in 0:k)
		z<-z+(-1)^(k-j)*choose(k,j)*j^n;
	return(z/s);
}
