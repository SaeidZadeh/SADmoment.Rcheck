extrapolation.raw <-
function(momraw,ratio,ba,bba)
{
	A<-max(ba);
	t<-1:length(bba)
	momr<-1:20*0
	l<-1:20
	momr[1]<-1
	for(k in 2:20)
	{
		TMP<-log(momraw[k,t])
		X<-matrix(data=0,nrow=length(t),ncol=3)
		j<-1
		X[,1]<-log(bba[t])
		kk<-(log(bba)-min(log(bba)))/(max(log(bba))-min(log(bba)))*pi
		X[,2]<-sin(kk)
		X[,3]<-cos(kk)
		Y<-data.frame(X[,1:(3)])
		reslm1<-lm(TMP[t]~.,data=Y)
		l[k]<-summary(reslm1)$r.squared
		momr[k]<-exp(relpwave(reslm1$coefficients,A,bba))
	}
	return(momr)
}
