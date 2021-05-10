f <-
function(df,xcoor,ycoor,x,ratio) # list of species in an specific area
{
	h<-df[,1][df[,2]>=xcoor & df[,2]<=xcoor+ratio*x & df[,3]>=ycoor & df[,3]<=ycoor+x];
	return(h);
}
