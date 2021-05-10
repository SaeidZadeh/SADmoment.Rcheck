sigfinderLN <-
function(res,slope,ro,c,z,A)
{
	return(sqrt(abs(2*(log(M1(ro,c,z,A))-mufinderLN(res,slope,A)))))
}
