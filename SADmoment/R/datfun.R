datfun <-
function(df,x,spp,n,ratio)
{
	xc<-runif(3*n,0,round(ratio*max(df[,3]))-ratio*x);
	yc<-runif(3*n,0,round(max(df[,3]))-x);
	S<-0
	N<-0
	for(i in 1:(3*n))
	{
		g<-f(df,xc[i],yc[i],x,ratio);
		W<-fg(g,spp);
		S<-S+length(intersect(g,g))
		N<-N+sum(W)
	}
	S<-S/n/3
	N<-N/n/3
	return(c(S,N));
}
