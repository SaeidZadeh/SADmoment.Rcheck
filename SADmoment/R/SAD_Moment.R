SAD_Moment <-
function(xpos=NULL,ypos=NULL,splist=NULL,ext.rate=1,dplot=T)
{
	prm<-proc.time()
	Y<-data.frame(splist,xpos,ypos);
	spp<-sort(intersect(splist,splist));
	W<-fg(splist,spp);
	TW<-W*ext.rate
	W<-log2(W[][W[]!=0])
	TW<-log2(TW[][TW[]!=0])
	larg.i2 <- round(max(TW))
	x<-round(max(xpos))
	y<-round(max(ypos))
	Area<-x*y
	Areap<-x*y*ext.rate
	ratio<-x/y;
	Ratio<-ratio
	a<-1:(ext.rate*x*y*1e-4);
	a<-a*1e4
	a<-sqrt(a/ratio)
	aa<-a[1:round(length(a)/ext.rate)];
	momdist<-matrix(data=0,nrow=20,ncol=length(aa)) 
	# nrow=20 corresponds to the case where maximum number of individuals of all species do not reach 741455. 
	momraw<-matrix(data=0,nrow=20,ncol=length(aa))
	momlin<-matrix(data=0,nrow=3,ncol=length(aa))
	if(ext.rate>1)
	{
		for(i in 1:length(aa))
		{
			n<-ceiling(2*y/aa[i]);
			tmp<-momfun(Y,aa[i],spp,n,20,ratio);
			momraw[,i]<-tmp[1,]
			momdist[,i]<-tmp[2,]
			momlin[,i]<-tmp[3,1:3]
		}
		Mom<-momdist[,length(aa)]
		ba<-ratio*a^2
		bba<-ratio*aa^2;
		momr<-extrapolation.raw(momraw,ratio,ba,bba)
		momd<-extrapolation.dist(momraw,momdist,ext.rate,momr)
		Momp<-momd
		S<-length(spp)
		larg.i <- round(max(W))
		Nbins<-larg.i+1
		Nbinsp<-larg.i2+1
		br <- seq(-0.5, larg.i2+0.5, 1)
		no.bins<-larg.i2+1
		no.m<-no.bins/2+1
		mom<-momd[1:no.m]
		no.bin<-no.bins+2
		C<-Coef.Tcheb.pol.2nd(no.bin,no.m)
		TchCo<-C
		Tp4<-C%*%mom
		TchMo<-Tp4
		tp4<-Tcheb.pol.2nd(no.bin,no.m)
		TchPo<-tp4
		fx4 <- rep(0,no.bins)
		for(i in 1:no.bins)
			fx4[i] <- sum(Tp4*tp4[,i])
		fx4 <- fx4*S
		no.binsp<-larg.i2+1
		fx4<-fx4[1:no.binsp]
		fx4[which(fx4<0)]<-0
	}
	if(ext.rate==1)
	{
		tmp<-momfun(Y,max(aa),spp,1,20,ratio)
		momd<-tmp[2,]
		S<-length(spp)
		larg.i <- round(max(W))
		br <- seq(-0.5, larg.i+0.5, 1)
		Mom<-momd
		Momp<-momd
		no.bins<-larg.i+1
		Nbins<-larg.i+1
		Nbinsp<-larg.i2+1
		no.m<-no.bins
		mom<-momd[1:no.m]
		no.bin<-no.bins
		C<-Coef.Tcheb.pol.2nd(no.bin,no.m)
		TchCo<-C
		Tp4<-C%*%mom
		TchMo<-Tp4
		tp4<-Tcheb.pol.2nd(no.bin,no.m)
		TchPo<-tp4
		fx4 <- rep(0,no.bins)
		for(i in 1:no.bins)
			fx4[i] <- sum(Tp4*tp4[,i])
		fx4 <- fx4*S
		fx4<-fx4[1:no.bins]
	}
	res<-hist(W, breaks=br,plot = F)
	lp<-res$counts
	if(dplot==T){
	  hist(W, breaks=br,main = paste0("Extrapolation to ",ext.rate," times of area"),ylab="Number of Species",ylim=c(0,max(c(fx4,lp))),xlab=substitute(paste(log[2],"(Number of Individuals)")))
	  lines(0:(larg.i2),fx4,col=4,lty=1)
	}
	INIT<-list(Area,Mom,Ratio,Nbins)
	names(INIT)<-c('Area','Moments','XY.Ratio','Number.of.bins');
	PRED<-list(Areap,Momp,Nbinsp,TchCo,TchPo,TchMo,fx4)
	names(PRED)<-c('Area','Moments','Number.of.bins','Coefficients.of.Tchebychev.Polynomials','Tchebychev.Polynomials.Values','Tchebychev.Moments.Values','Extrapolate.SAD')
	pttime<-proc.time()-prm
	dat<-list(spp,xpos,ypos,splist,length(spp),lp)
	names(dat)<-c('Species.Name','X.Position','Y.position','Species.In.Position','Number.Species','SAD')
	resu<-list(dat,INIT,PRED,pttime)
	names(resu)<-c('Data','Initial','Extrapolate','Time')
	return(invisible(resu))
}
