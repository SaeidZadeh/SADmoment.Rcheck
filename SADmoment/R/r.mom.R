r.mom <-
function(dat,no.m)
{
	S <- length(dat)
	mom <- rep(0,no.m)
	mom[1] <- 1
	for(i in 1:(no.m-1))
		mom[i+1] <- sum(dat^i)/S
	return(mom)
}
