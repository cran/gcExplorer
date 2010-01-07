%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
%%%  $Id: clusterPlot.Rd 4379 2009-07-24 09:22:58Z scharl $
%%%
\name{clusterPlot}
\docType{methods}
\alias{clusterPlot}
\alias{clusterPlot,kccasimple-method}
\title{Cluster solution plot}
\description{
Plot the expression profiles of the smallest clusters of an object of class \code{"kccasimple"}.
}
\usage{
\S4method{clusterPlot}{kccasimple}(object, method = c("size", "tight"), layout = c(3, 4),
   xlabels = NULL, xlab = "time", ...)
}
\arguments{
 \item{object}{An object of class \code{"kccasimple"}.}
 \item{method}{Which clusters should be plotted: either small clusters or tight clusters.}
 \item{layout}{A vector of the form c(nr, nc). Only a subset of nr x nc clusters will be drawn. The arrangement of nr rows and nc columns is passed to the \code{layout} argument of \code{lattice} function \code{xyplot}.}
 \item{xlabels}{Either a numeric vector of time points giving the positions on the x-axis or a character vector with names of the positions on the x-axis.}
 \item{xlab}{Character string or expression giving label for the x-axis}
 \item{...}{Further arguments can be passed to function \code{xyplot}.}
}
\author{Theresa Scharl}
\examples{
data("hsod")
cl1 <- qtclust(hsod, radius = 2, save.data = TRUE)

clusterPlot(cl1, method = "tight",layout = c(3,2))

}
\keyword{methods}
\keyword{hplot}
