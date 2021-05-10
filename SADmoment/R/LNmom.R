LNmom <-
function(mu,sig,k)
{
	return(cmoment(mu,sig,k)/(log(2)^k))
}
