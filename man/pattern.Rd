%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
%%%  $Id: pattern.Rd 4251 2009-01-13 18:18:10Z leisch $
%%%
\name{pattern}
\alias{pattern}
\title{Expression pattern}
\description{
Expression patterns that can be used to generate artificial gene expression data.
}
\usage{
pattern(time = 8, v = 5)
}
\arguments{
  \item{time}{number of time points}
  \item{v}{absolute value of maximum gene expression}
}
\value{
a data matrix}
\author{Theresa Scharl}
\seealso{\code{\link{gcSim}}}
\examples{
cent <- pattern(time=15)
data <- gcSim(sim="pattern", cent=cent)
matplot(t(data),type="l")
}
\keyword{cluster}


