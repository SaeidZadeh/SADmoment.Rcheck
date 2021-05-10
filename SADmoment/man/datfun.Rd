\name{datfun}
\alias{datfun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
average number of species and individuals
}
\description{
Returns average number of species and individuals in a given sub-area, by randomly sampling that sub-area size from the total area.
}
\usage{
datfun(df, x, spp, n, ratio)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
data frame, name of species, position over X and Y axis
}
  \item{x}{
sub-section size over Y axis
}
  \item{spp}{
list of all species
}
  \item{n}{
number of random sampling
}
  \item{ratio}{
ratio of X by Y
}
}