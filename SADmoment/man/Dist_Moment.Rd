\name{Dist_Moment}
\alias{Dist_Moment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moments of the most common fitting distributions
}
\description{
Returns the moments up to degree 20 of the fitting distribution of data across scales.
}
\usage{
Dist_Moment(xpos, ypos, splist, DIST = "LS") 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xpos}{
  position of individuals of species by X-axis
}
  \item{ypos}{
position of individuals of species by Y-axis
}
  \item{splist}{
Respective name of the species in the position (X,Y)
}
  \item{DIST}{
The fitting distribution under consideration ("LS","LN","PG","PLN")
}
}