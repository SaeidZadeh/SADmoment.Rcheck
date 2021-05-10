betfinder <-
function(res,slope,ro,c,z,A)
{
	q<-(mo2nd(res,slope,A)/M1(ro,c,z,A))-1-M1(ro,c,z,A);
	return(1/q);
}
