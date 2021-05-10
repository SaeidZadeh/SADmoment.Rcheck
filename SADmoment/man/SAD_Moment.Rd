\name{SAD_Moment}
\alias{SAD_Moment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extrapolate Species Abundance Distribution
}
\description{
Extrapolates SAD by extrapolating of moments.
}
\usage{
SAD_Moment(xpos = NULL, ypos = NULL, splist = NULL, ext.rate = 1, dplot = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xpos}{
List of position of individuals over X-axis
}
  \item{ypos}{
List of position of individuals over Y-axis
}
  \item{splist}{
Name of species where its individual is in place (X,Y)
}
  \item{ext.rate}{
extrapolation rate(>=1)
}
  \item{dplot}{
Values (T or F) respectively to inform whether the plot of the extrapolation should appears or not.
}
}
