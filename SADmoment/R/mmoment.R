mmoment <-
function(al,bet,k)
{
	x<-digamma(al)-log(bet);
	for(i in 2:k)
		x<-c(x,psigamma(al,i-1));
	s<-0;
	for(i in 1:k)
	{
		s<-s+BellB(k,i,x[1:(k-i+1)]);
	}
	return(s);
}
