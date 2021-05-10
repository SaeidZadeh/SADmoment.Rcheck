LSmom <-
function(x,n) # moments calculator for log2(Xi)
{
	tmp<-0;
	y<-7;
	s<-numsum(n,x,y);
	while(-tmp/log(1-x)!=-s/log(1-x)&y<=23) # recursively caculate moments if two consecutive values are equal we will consider it as corresponding moment
	{
		tmp<-s;
		y<-y+1;
		s<-s+numsum(n,x,y);
	}
	return(-s/log(1-x));
}
