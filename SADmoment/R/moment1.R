moment1 <-
function(x,n) # moments calculator for Xi
{
	li<-polylog(x,1-n);
	return(-li/log(1-x));
}
