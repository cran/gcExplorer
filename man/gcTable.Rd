%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
%%%  $Id: gcTable.Rd 4249 2009-01-13 14:06:28Z scharl $
%%%
\name{gcTable}
\docType{methods}
\alias{gcTable}
\alias{gcTable,kccasimple-method}
\title{ HTML table for cluster results }
\description{
Create HTML table for a single cluster of a 'kccasimple' object.
}
\usage{
\S4method{gcTable}{kccasimple}(object, which, links, file="gcTable", ...)
}
\arguments{
  \item{object}{an object of class \code{"kccasimple"}}
  \item{which}{Number of the cluster.}
  \item{links}{Vector of the same length as rows in the data with links to a database.}
  \item{file}{File name prefix used for HTML tables. Of the form 'file-which.html'.}
  \item{\dots}{Further arguments can be passed to \code{write.htmltable}.}
}
\author{Theresa Scharl}
\seealso{\code{\link{write.htmltable}}}
\examples{
data("hsod")
cl1 <- qtclust(hsod, radius=2, save.data=TRUE)

gcTable(cl1, which=5, links = links_hsod, file = "hsod")
\dontrun{
gcExplorer(cl1, theme = "blue", panel.function = gcTable, 
           panel.args = list(links = links_hsod, file="hsod"),
           node.function = node.size) 
}

}
\keyword{methods}

