momfun <-
function(df,x,spp,n,no.m,ratio)
{
	xc<-runif(n,0,round(ratio*max(df[,3]))-ratio*x);
	yc<-runif(n,0,round(max(df[,3]))-x);
	mom<-rep(0,no.m)
	mom1<-mom
	mom2<-mom
	for(i in 1:n)
	{
		g<-f(df,xc[i],yc[i],x,ratio);
		W<-fg(g,spp);
		mom<-mom+r.mom(log2(W[][W[]!=0]),no.m)
		mom1<-mom1+r.mom(round(log2(W[][W[]!=0])),no.m)
		mom2[1:3]<-mom2[1:3]+r.mom(round((W[][W[]!=0])),3)
	}
	mom<-mom/n;
	mom1<-mom1/n;
	mom2<-mom2/n
	return(rbind(mom,mom1,mom2));
}
