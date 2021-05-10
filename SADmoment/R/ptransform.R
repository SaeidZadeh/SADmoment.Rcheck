ptransform <-
function(x,k)
{
	return(polylog(x,1,"sum",n.sum =2^(k+1)-1)-polylog(x,1,"sum",n.sum = 2^(k)-1));
}
