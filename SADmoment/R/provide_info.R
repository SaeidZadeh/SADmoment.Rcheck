provide_info <-
function(L)
{
	i<-which.max(L[,1]);
	Llist<-c("Poisson Gamma","Log-Normal","Log-Series","Poisson Log-Normal")
	x<-(paste0("For the sub-area ( ",L[5,4]," ) the best fitting distribution is : ", Llist[i]))
	if(i == 1)
		y<-(paste0("alpha= ",L[1,2]," and beta= ",L[1,3]))
	if(i == 2)
		y<-(paste0("mu= ",L[2,2]," and sigma= ",L[2,3]))
	if(i == 3)
		y<-(paste0("x= ",L[3,2]))
	if(i == 4)
		y<-(paste0("mu= ",L[4,2]," , sigma= ",L[4,3]," and p= ",L[4,4]))
	return(rbind(unlist(x),unlist(y)))
}
