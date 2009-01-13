%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
%%%  $Id: node.tight.Rd 4249 2009-01-13 14:06:28Z scharl $
%%%
\name{node.tight}
\alias{node.tight,kccasimple-method}
\alias{node.tight}
\alias{node.size,kccasimple-method}
\alias{node.size}
\alias{node.go,kccasimple-method}
\alias{node.go}
\alias{node.group,kccasimple-method}
\alias{node.group}
\alias{legend.size,kccasimple-method}
\alias{legend.size}
\alias{legend.tight,kccasimple-method}
\alias{legend.tight}
\title{Node Methods for Neighborhood Graphs}
\description{
Several methods how to color nodes of a neighborhood graph.}
\usage{
\S4method{node.tight}{kccasimple}(object, theme, colscale)
\S4method{node.size}{kccasimple}(object, theme, colscale)
\S4method{node.go}{kccasimple}(object, theme, colscale, gonr, source.group, source.id, id)
\S4method{node.group}{kccasimple}(object, theme, colscale, group)
\S4method{legend.size}{kccasimple}(object, theme, colscale=NULL, pos="bottomleft")
\S4method{legend.tight}{kccasimple}(object, theme, colscale=NULL, pos="bottomleft")
}
\arguments{
  \item{object}{An object of class \code{"kccasimple"}}
  \item{theme}{A color theme, eg. "blue".}
  \item{colscale}{Range of luminescence lum of hcl colors, default is min to max.}
  \item{gonr}{Unique identifier from \code{source.group} giving the group of genes to be extracted.}
  \item{source.id}{Vector of gene IDs assigned to functional groups given in \code{"source.group"}.}
  \item{source.group}{Vector of the same length as \code{source.id}.}
  \item{id}{Vector of identifiers of the same length as rows in the clustered data of the same type as given in \code{source.id}.}
  \item{group}{Vector of integers giving the cluster membership of grouped genes.}
  \item{pos}{Position where the legend should be placed.}
}
\details{
Function \code{node.size} is used to highlight large clusters where the largest cluster will be assigned the darkest color.

Function \code{node.tight} is used to highlight tight clusters where the tightest cluster will be assigned the darkest color.

Function \code{node.go} is used to highlight clusters with accumulation of the functional group given by \code{gonr} where the highest proportion will be assigned the darkest color.

Function \code{node.group} is used to highlight clusters with accumulation of a functional group where the class membership are passed by argument \code{group}. Again the highest proportion will be assigned the darkest color.
}
\author{Theresa Scharl and Ingo Voglhuber}
\seealso{\code{\link{gcExplorer}}}
\examples{
data("hsod")
set.seed(1111)
cl1 <- qtclust(hsod, radius = 2, save.data = TRUE)

gcExplorer(cl1, theme = "blue", node.function = node.size, 
           legend.pos= "topleft")

gcExplorer(cl1, theme = "red", node.function = node.tight, 
           legend.pos= "topleft")

data("gobp")
gcExplorer(cl1, theme = "green", node.function = node.go, 
           node.args = list(gonr = "transport", source.group = gobp[,3],
                            source.id = gobp[,1], id = bn_hsod), 
           legend.pos= "topleft")

}
\keyword{methods}
