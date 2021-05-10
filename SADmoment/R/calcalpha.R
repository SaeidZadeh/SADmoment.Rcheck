calcalpha <-
function(n.orig, s.orig)
{
	bads <- (n.orig<=0 | s.orig<=0 | n.orig<=s.orig)
	n<-n.orig
	s<-s.orig
	n[bads]<-100
	s[bads]<-50
	a <- rep(20,length(n))
	poorest <- rep(T,length(n))
	a<-3;
	while(length(a[poorest])>0)
	{
		a <- a-fo(a,n,s)/fprime(a,n,s)
		poorest <- abs(fo(a,n,s))>1e-10
		a[a<=0|is.nan(a)] <- 1
	}
	a[bads]<-(-1)
	return(a)
}
