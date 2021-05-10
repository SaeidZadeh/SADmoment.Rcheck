\name{SADmoment-package}
\alias{SADmoment-package}
\alias{SADmoment}
\docType{package}
\title{
\packageTitle{SADmoment}
}
\description{
Extrapolate SAD and parameters of best fitting distribution for subareas.
}
\details{
It is implemented by the techniques developed in Alirezazadeh S. (2019) and Alirezazadeh et. al. (2018). The most important functions of the package LLfun and SAD_Moment.
The function SAD_Moment(X,Y,Splist,ext.rate,dplot) asks for positions (X,Y) of individuals of species Splist in the area of existing sample, an extrapolation rate that is the proportion of area we are interested to extrapolate to by the sample area, and dplot that identify whether the user want to visualise the SAD of the extrapolated area and compare it with the sample data or not. If the dplot=T then after running the function provides the histogram for species abundances of the sample and a line which is the estimation of species abundances of the extrapolated area. 
Note that the function SAD_Moment(X,Y,Splist,ext.rate,dplot) returns valuable information regarding the running time, information on the sample and also information regarding the extrapolated area. So, we suggest to run it as: name<-SAD_Moment(X,Y,Splist,ext.rate,dplot). Also, note that the length of X, Y and Splist must be the same.
The function LLfun(X,splist), where X is a data.frame consists of species names and their (X,Y) positions. And splist is the name of existing species in the sample. And by pressing return it requests the user to enter a value for sub-area size and then with respect to the Log-Likelihood, finds the best fitting distribution of data and returns its parameters, see Alirezazadeh et.al. (2018)
}
\author{
Saeid Alirezazadeh
}


\references{
Alirezazadeh, S., 2019, Improvement on Extrapolation of Species Abundance Distribution Across Scales from Moments Across Scales (Unpublished)

Saeid Alirezazadeh and Luís Borda-de-Água and Paulo Borges and Rosalina Gabriel and F. Miguel Dionisio and Henrique Miguel Pereira and Pedro Cardoso, 2018, Theoretical Approach for how Species Abundance Distributions Change Across Scales, 2018 13th APCA International Conference on Control and Soft Computing (CONTROLO), pp. 131-136.
}

\examples{
# Data from Barro Colorado Island (BCI) is used here for the example, 
# within total 50 ha area all the individuals 
# with at least 1cm (Diameter at Breast Height) (dbh) are considered.

SAD_Moment(dbci[,3],dbci[,4],dbci[,1],ext.rate=1.1,dplot=TRUE)

# In the following we used all the individuals 
# with at least 10cm (Diameter at Breast Height) (dbh) from the BCI data.
#R> name<-SAD_Moment(X,Y,Spnames,ext.rate=1.2,dplot=FALSE)
#R> summary(name)
#            Length Class     Mode   
#Data        6      -none-    list   
#Initial     4      -none-    list   
#Extrapolate 7      -none-    list   
#Time        5      proc_time numeric
#R> summary(name$Initial)
#               Length Class  Mode   
#Area            1     -none- numeric
#Moments        20     -none- numeric
#XY.Ratio        1     -none- numeric
#Number.of.bins  1     -none- numeric
#R> summary(name$Extrapolate)
#                                       Length Class  Mode   
#Area                                    1     -none- numeric
#Moments                                20     -none- numeric
#Number.of.bins                          1     -none- numeric
#Coefficients.of.Tchebychev.Polynomials 49     -none- numeric
#Tchebychev.Polynomials.Values          98     -none- numeric
#Tchebychev.Moments.Values               7     -none- numeric
#Extrapolate.SAD                        12     -none- numeric
#R> summary(name$Time)
#   user  system elapsed 
#  4.632   2.009   6.690 
#R>print(name$Data$SAD)
# [1] 27 11 20 30 29 38 30 21 20  5  4  1
#R> print(name$Extrapolate$Extrapolate.SAD)
# [1] 32 13 15 24 31 33 30 24 17 10  5  2
# 
#R> LLfun(X,Splist)
#Enter sub-area size: 9800
#     [,1]                                                                        
#[1,] "For the sub-area ( 9800 ) the best fitting distribution is : Poisson Gamma"
#[2,] "alpha= 1.2089269105249 and beta= 1.2089269105249"
#R> LLfun(X,Y,Splist)
#Enter sub-area size: 200000
#     [,1]                                                                      
#[1,] "For the sub-area ( 2e+05 ) the best fitting distribution is : Log-Normal"
#[2,] "mu= 3.51478121939837 and sigma= 2.2582385148143"
#
}

