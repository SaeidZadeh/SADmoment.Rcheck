shrt <-
function(u)
{
	a<-which(u[1,]!=0)
	s<-u[,a];
	return(s);
}
