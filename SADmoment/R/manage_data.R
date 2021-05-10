manage_data <-
function(df,spp,n,x,ratio)
{
	xc<-runif(n,0,round(ratio*max(df[,3]))-ratio*x);
	yc<-runif(n,0,round(max(df[,3]))-x);
	W<-rep(0,length(spp))
	for(i in 1:n)
	{
		g<-f(df,xc[i],yc[i],x,ratio);
		W<-W+fg(g,spp);
	}
	W<-W/n;
	return(W);
}
