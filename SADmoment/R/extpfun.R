extpfun <-
function(dat,m)
{
	if(length(dat)>=m)
		return(dat[1:m])
	bba<-sqrt(1:length(dat)*10000)
	tmp<-length(bba)
	c<-lm(dat~bba)
	Cp<-as.double(residuals(c))
	kp<-as.double(c$coefficients)
	k1<-kp[1]+kp[2]*(sqrt(1+length(dat)))
	deg<-atan(Cp)
	dat<-c(dat,k1+(deg[tmp]))
	extpfun(dat,m)
}
