fg <-
function(g,sppnames)
{
	noin<-1:length(sppnames)*0;
	for(i in 1:length(sppnames))
		noin[i]<-length(g[][g[]==sppnames[i]]);
	return(noin);
}
