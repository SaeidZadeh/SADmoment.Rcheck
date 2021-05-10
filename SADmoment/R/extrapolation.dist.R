extrapolation.dist <-
function(momraw,momdist,ext.rate,momr)
{
	p<-1:length(momraw[1,])*0
	for(k in 1:length(momraw[1,]))
	{
		c<-lm(log(momdist[,k])~log(momraw[,k])+0)
		p[k]<-as.double(c$coefficients)
	}
	momd<-momr^extpfun(p,ext.rate*length(momraw[1,]))[ext.rate*length(momraw[1,])]
	return(momd)
}
