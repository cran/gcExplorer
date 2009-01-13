%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
%%%  $Id: clnorm.Rd 4249 2009-01-13 14:06:28Z scharl $
%%%
\name{gcSim}
\alias{gcSim}
\alias{gcData}
\title{Create artificial cluster data}
\description{
Functionality to create artificial time course gene cluster data.
}
\usage{
gcSim(sim=c("arima","norm","pattern","noise","outlier"), time=10,
      sd=0.1, sd.ri=0, size=50, n=10, ar=NULL, o=NULL, cent)

gcData(...)
}
\arguments{
  \item{sim}{simulation method used}
  \item{time}{number of time points}
  \item{sd}{standard deviation of the expression profiles}
  \item{sd.ri}{standard deviation of the random intercept or gene specific shift}
  \item{size}{cluster size, either one value for all clusters or a vector of cluster sizes of length n}
  \item{n}{number of clusters}
  \item{ar}{any value between -1 and 1}
  \item{o}{the degree of differencing}
  \item{cent}{a data matrix giving expression profiles in rows, only used in \code{cl.pattern} and \code{cl.outlier}}
  \item{...}{Several \code{gcSim} object can be combined.}
}
\details{
\code{gcSim} is a unifying function to call different data simulators.

\code{"arima"} generates expression patterns that come from an integrated AR-process with AR order 1 that can be controlled via \code{ar} and the degree of differencing \code{o}. \code{"norm"} and \code{"noise"} generate normally distributed expression patterns where \code{"noise"} is used to form a noise set of genes.

\code{"pattern"} and \code{"outlier"} can be used to generate clusters based on a set of cluster centers which are passed to the functions using the argument \code{cent}. \code{"outlier"} can be used to test Jackknife distance measures.

\code{gcData} can be used to combine different artificial data generators.
}
\value{
a data matrix
}
\author{Theresa Scharl}
\seealso{\code{\link{pattern}}}
\examples{
## generate 10 clusters with normally distributed expression patterns:
data <- gcSim(sim="norm", time=16, sd=0.1, sd.ri=0.5, 
              size=50, n=10)
matplot(t(data),type="l",pch=1)

## combine expression patterns that follow an ARIMA process and a null cluster:

data <- gcData(gcSim(sim="arima", time=16, sd=0.1, sd.ri=0.5, 
                     size=c(20,50,100,100), n=4),
               gcSim(sim="noise",time=16, size=100, sd=0))
matplot(t(data),type="l")
}
\keyword{cluster}
