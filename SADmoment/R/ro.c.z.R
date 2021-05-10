ro.c.z <-
function(No.Sp,No.Id,bba)
{
	p<-lm(No.Id~bba+0)
	ro<-as.numeric(p$coefficients)
	p<-lm(log(No.Sp)~log(bba))
	tmp<-as.numeric(p$coefficients)
	return(c(ro,exp(tmp[1]),tmp[2]))
}
