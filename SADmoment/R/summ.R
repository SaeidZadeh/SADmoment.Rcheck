summ <-
function(u1,u2)
{
	m<-max(max(u1[2,]),max(u2[2,]))
	up1<-expnd(u1,m);
	up2<-expnd(u2,m)
	up<-up1+up2;
	up[2,]<-up1[2,]
	up<-shrt(up);
	return(up)
}
