sigfinderPLN <-
function(res1,slope1,res2,slope2,A)
{
	return(mo2nd(res2,slope2,A)-(mufinderPLN(res1,slope1,A))^2-mufinderPLN(res1,slope1,A))
}
