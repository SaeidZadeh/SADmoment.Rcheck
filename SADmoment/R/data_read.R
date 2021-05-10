data_read <-
function()
{
	if(interactive()) sa<-readline("Enter sub-area size: ")
	return(as.numeric(unlist(strsplit(sa,","))))
}
