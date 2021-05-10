alffinder <-
function(res,slope,ro,c,z,A)
{
	return(M1(ro,c,z,A)*betfinder(res,slope,ro,c,z,A));
}
