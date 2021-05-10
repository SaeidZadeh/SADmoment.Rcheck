LLfun <-
function(Y,spp)
{
	sa<-data_read();
	n<-ceiling(max(Y[,2])*max(Y[,3])/sa*5)
	ratio<-max(Y[,2])/max(Y[,3])
	y<-round(manage_data(Y,spp,n,sqrt(sa/ratio),ratio))
	y<-y[log(y)>=0];
	yp<-y;
	y<-log(y,2);
	L<-matrix(data=0,nrow=5,ncol=4)
	options(warn = -1)
	g<-function(x) -length(y)*digamma(x)-length(y)*log((x+mean(yp))/x)+sum(digamma(yp+x))
	t1<-try(uniroot(g,c(0.0001,10),tol = .Machine$double.eps))
	if(!is.error(t1))
	{
		t1<-uniroot(g,c(0.0001,10),tol = .Machine$double.eps)
		bet<-t1$root
		al<-bet/mean(yp,2);
		pg<-function(al,bet) -sum(lgamma(yp+bet)+bet*log(al)-lgamma(bet)-(bet+yp)*log(1+al)-lfactorial(yp))
		L[1,1]<-2*pg(al,bet)+4;
		L[1,2]<-al;
		L[1,3]<-bet;
	}
	mu<-mean(log(yp,2));
	sig<-sqrt(sum((log(yp,2)-mean(log(yp,2)))^2)/length(log(yp,2)));
	AICln<-function(mu,sig)
	{
		-2*(-length(log(yp,2))/2*log(2*pi*sig^2)-sum(log(yp,2))-sum((log(yp,2)-mu)^2/(2*sig^2)))+4
	}
	L[2,1]<-AICln(mu,sig)
	L[2,2]<-mu;
	L[2,3]<-sig;
	pmfls<-function(x,y)
	{
		-x^y/(y*log(1-x))
	}
	ls<-function(x) -sum(-log(-log(1-x))+yp*log(x)-log(yp,2))
	fit0 <- try(mle2(ls, start = list(x=0.9),method="Nelder-Mead",skip.hessian = TRUE))
	if(!is.error(fit0))
	{
		fit0 <- mle2(ls, start = list(x=0.9),method="Nelder-Mead",skip.hessian = TRUE)
		L[3,1]<-AIC(fit0);
		L[3,2]<-as.numeric(coef(fit0))
	}
	U<-poilogMLE(round(yp));
	L[4,1]<--2*U$logLval+4;
	L[4,2:3]<-as.numeric(U$par)
	L[4,4]<-as.numeric(U$p)
	L[5,4]<-sa
	options(warn = 1)
	provide_info(L)
}
