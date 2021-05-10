M1 <-
function(ro,c,z,A)
{
	return(ro*A/(c*A^z));
#  return(ro*A/(c+z*log(A)))
}
