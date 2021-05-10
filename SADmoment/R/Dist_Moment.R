Dist_Moment <-
function(xpos,ypos,splist,DIST="LS")
{
	Y<-data.frame(splist,xpos,ypos);
	spp<-sort(intersect(splist,splist));
	W<-fg(splist,spp);
	W<-log2(W[][W[]!=0])
	x<-round(max(xpos))
	y<-round(max(ypos))
	ratio<-x/y;
	aa<-1:(x*y*1e-4);
	aa<-aa*1e4
	aa<-sqrt(aa/ratio)
	momdist<-matrix(data=0,nrow=3,ncol=length(aa))
	momraw<-matrix(data=0,nrow=3,ncol=length(aa))
	momlin<-matrix(data=0,nrow=3,ncol=length(aa))
	No.Id<-rep(0,length(aa))
	No.Sp<-No.Id
	for(i in 1:length(aa))
	{
		n<-ceiling(2*y/aa[i]);
		tmp<-momfun(Y,aa[i],spp,n,3,ratio);
		momraw[,i]<-tmp[1,]
		momdist[,i]<-tmp[2,]
		momlin[,i]<-tmp[3,]
		temp<-datfun(Y,aa[i],spp,n,ratio)
		No.Sp[i]<-as.numeric(temp[1])
		No.Id[i]<-as.numeric(temp[2])
	}
	bba<-ratio*aa^2;
	tmp<-ro.c.z(No.Sp,No.Id,bba)
	ro<-tmp[1]
	c<-tmp[2]
	z<-tmp[3]
	x1<-aa*0
  	s1<-c*bba^z;
	c1<-lm(log(momlin[3,])~log(bba))
  	p2<-as.double(c1$coefficients)
  	c1<-lm(log(momdist[2,])~log(bba))
  	pl1<-as.double(c1$coefficients)
  	c1<-lm(log(momdist[3,])~log(bba))
  	pl2<-as.double(c1$coefficients)
  	mom<-matrix(data=0,nrow=20,ncol=length(bba));
  	if(DIST=="LS")
  	{
  		alpha1<-aa*0;
  		t<-1:length(aa)
  		for(i in t)
    		alpha1[i]<-calcalpha(ro*bba[i],c*bba[i]^z);
  		x1<-1-exp(-s1/alpha1)
  		for(i in t)
    		for(j in 1:20)
      			mom[j,i]<-LSmom(x1[i],j);
      	return(mom)
  	}
  	if(DIST=="PG")
  	{
  		alf<-1:length(bba)*0;
  		bet<-1:length(bba)*0;
  		for(i in 1:length(bba))
  		{
    		alf[i]<-alffinder(p2[1],p2[2],ro,c,z,bba[i])
    		bet[i]<-betfinder(p2[1],p2[2],ro,c,z,bba[i])
 	 	}
  		for(i in 1:length(bba))
   			for(j in 1:20)
      			mom[j,i]<-PGmom(alf[i]+1,bet[i],j);
      	return(mom)
	}
    if(DIST=="LN")
  	{
  		sigLN<-sigfinderLN(exp(pl2[1]),pl2[2],ro,c,z,bba);
  		muLN<-mufinderLN(exp(pl2[1]),pl2[2],bba)
  		for(i in 1:length(bba))
    		for(j in 1:20)
      			mom[j,i]<-LNmom(muLN[i],sigLN[i],j);
      	return(mom)
    }
    if(DIST=="PLN")
    {
    	sigPLN<-sigfinderPLN(exp(pl1[1]),pl1[2],exp(pl2[1]),pl2[2],bba);
  		muPLN<-mufinderPLN(exp(pl1[1]),pl1[2],bba)
  		for(i in 1:length(bba))
    		for(j in 1:20)
     			mom[j,i]<-PLNmom(muPLN[i],sigPLN[i],j);
		return(mom)
    }
}
