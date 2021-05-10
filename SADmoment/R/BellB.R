BellB <-
function(n, k, x)
{
	if(n == 0) return(rep(1, ifelse(missing(k), 1, length(k))))
	if(missing(k))
	{ # complete
		k <- 1:n
		v <- sapply(k, BellB, n=n, x=x)
		sum(v)
	}
	else
	{ # partial
		#' recursion function
		rf <- function(m, l)
		{
			if(l < 1)
			{
				x[m]
			} 
			else
      		{
				i <- l:(m-1)
				sum( choose(m, i) * x[m-i] * sapply(i, rf, l-1))
			}
		}
		K <- k-1
		v <- rf(n, K)
	    v/factorial(k)
	}
}
