relpwave <-
function(l,t,ba)
{
	tmp<-(log(t)-min(log(ba)))/(max(log(ba))-min(log(ba)))*pi
	for(i in 1:length(l))
		if(is.na(l[i])) 
			l[i]<-0
	k<-0
	X<-1:length(l)
	j<-1
	X[2]<-log(t)
	X[3]<-sin(tmp)
	X[4]<-cos(tmp)
	for(i in 1:length(l))
		k<-k+X[i]*l[i];
	return(k)
}
