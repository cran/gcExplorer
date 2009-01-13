%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
%%%  $Id: gcProfile.Rd 4249 2009-01-13 14:06:28Z scharl $
%%%
\name{gcProfile}
\docType{methods}
\alias{gcProfile}
\alias{gcProfile,kccasimple-method}
\title{ Plot for cluster results }
\description{
Plot a single cluster of a 'kccasimple' object.
}
\usage{
\S4method{gcProfile}{kccasimple}(object, which, data = NULL, cexl = 0.8, xlab = "", 
   ylab = "M", ylim=c(-6,6), cex.axis=1, xlabels=NULL,
   opar = par(las=1, mar=c(5, 4, 2, 0.5) + 0.1), 
   data.type=c("time", "other"), legend=TRUE, ...)
}
\arguments{
  \item{object}{an object of class \code{"kccasimple"}}
  \item{data}{Plot either the data stored in \code{object} or external data. }
  \item{which}{Number of the cluster.}
  \item{cexl}{Point size of the legend.}
  \item{xlab}{Label for the x-axis.}
  \item{ylab}{Label for the y-axis.}
  \item{ylim}{Range of the y-axis.}
  \item{cex.axis}{Point size of x-axis.}
  \item{xlabels}{Positions on the x-axis. Default is \code{1:ncol(data)}.}
  \item{opar}{Graphical parameters.}
  \item{data.type}{If the data come from arbitrary source (default) colnames of the data are used as xlabels if not stated otherwise using \code{xlabels}. If the data comes from a time course experiment x-values start at 0 and different time intervals are supported.}
  \item{legend}{Logical. Should a legend be drawn?.}
  \item{\dots}{Further arguments can be passed to \code{matplot}.}
}
\author{Theresa Scharl}
\examples{
data("hsod")
cl1 <- qtclust(hsod, radius=2, save.data=TRUE)

gcProfile(cl1, which=5)
gcProfile(cl1, which=5, xlabels=c(0,8,15,22,45,68,90,150,180),
          xlab="time after induction [min]",data.type="time")
}
\keyword{methods}
\keyword{hplot}

