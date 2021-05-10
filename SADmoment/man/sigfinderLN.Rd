\name{sigfinderLN}
\alias{sigfinderLN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
standard deviation finder for Log Normal across scales
}
\description{
returns the parameter sigma in the Log-normal Distribion.
}
\usage{
sigfinderLN(res, slope, ro, c, z, A)
}
\arguments{
  \item{res}{
residual for the change of the first raw moments across scales
}
  \item{slope}{
slope of the change of the first raw moments across scales
}
  \item{ro}{
the parameter ro in the formula N=ro A, number of individuals across scales
}
  \item{c}{
the parameter c in S=cA^z
}
  \item{z}{
the parameter z in S=cA^z
}
  \item{A}{
area or scale
}
}
