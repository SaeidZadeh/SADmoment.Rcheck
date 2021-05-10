Tcheb.pol.2nd <-
function(N,no.m)
{
	t<-matrix(data=0,nrow=no.m,ncol=N);
	t[1,1]<-tn(0,N)
	for(i in 2:(no.m))
		t[i,1]<-tn(i-1,N)
	for(i in 1:no.m)
		t[i,2]<-(1+((i-1)*i)/(1-N))*t[i,1]
	for(x in 2:floor(N/2))
		for(n in 1:no.m)
		{
			g1<-(-1*(n-1)*n-(2*x-1)*(x-N-1)-x)/(x*(N-x))
			g2<-((x-1)*(x-N-1))/(x*(N-x))
			t[n,x+1]<-g1*t[n,x]+g2*t[n,x-1]
		}
	for(x in floor(N/2):(N-1))
		for(n in 1:no.m)
			t[n,x+1]<-(-1)^(n-1)*t[n,N-x]
	return(t)
}
