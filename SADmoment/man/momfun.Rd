\name{momfun}
\alias{momfun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
raw, distribution, and linear moments
}
\description{
all moments are calculated here. linear moments is when we do not log transform the number of individuals.
}

\usage{
momfun(df, x, spp, n, no.m, ratio)
}
\arguments{
  \item{df}{
data frame consists of name of species together with positions over X and Y axis
}
  \item{x}{
subsection size over Y axis
}
  \item{spp}{
list of all species
}
  \item{n}{
number of randomize sampling in the total area
}
  \item{no.m}{
maximum order of moments
}
  \item{ratio}{
ratio of X by Y
}
}
