%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl, Ingo Voglhuber
%%%  $Id: gcModify.Rd 4498 2010-01-07 13:35:42Z scharl $
%%%
\name{gcModify}
\docType{methods}
\alias{gcModify}
\alias{gcModify,graphdata-method}
\title{ Modify Ragraph Objects and Replot them }
\description{
  \code{gcModify} is a function to modify and plot an object
  of class \code{"graphdata"}:
  
  - remove edges/nodes
  
  - zoom  
  
  - draw custom node plots
}
\usage{
\S4method{gcModify}{graphdata}(graphdata, clsim = NULL, rmNodes = NULL,
         kpNodes = NULL, edgeDep = TRUE, nodeDep = FALSE,
         zoom = c("none", "manual", "auto"),
         keepAspectRatio = TRUE, node.function = NULL,
         doViewPort = TRUE, bgdata = NULL)
}
\arguments{
  \item{graphdata}{list, containing object of class \code{"Ragraph"}, 
    object of class \code{"kcca"} and other parameters of graph created by
    \code{gcExplorer}). }
  \item{clsim}{matrix, new clsim to define removal or modification of edges. }
  \item{rmNodes}{character vector, names of nodes to remove.
    (can not be used in combination with \code{kpNodes}) }
  \item{kpNodes}{character vector, names of nodes to keep.
    (can not be used in combination with \code{rmNodes}) }
  \item{edgeDep}{logical. If \code{TRUE} edges are removed,
    if they do not connect two nodes. }
  \item{nodeDep}{logical. If \code{TRUE} nodes are removed,
    if they are not connected to other nodes. }
  \item{zoom}{one of:
  
    \code{"none"} - no zoom.
    
    \code{"manual"} - activate manual zoom, user interaction needed to 
    specify area to be enlarged.
    
    \code{"auto"} - auto zoom, automatically enlarges graph to size of
    graphic device.}
  \item{keepAspectRatio}{logical. If \code{TRUE} aspect ratio is preserved. }
  \item{node.function}{Grid based function for node plotting.
    To work correctly, the function will take three arguments:
    
    \code{object} is an object of class \code{"kcca"}.
    
    \code{cluster} is an integer giving the node (i.e., cluster) number.
    
    \code{bgdata} is a data.frame of external data. }
  \item{doViewPort}{logical. If \code{TRUE} \code{node.function} is called
    to draw nodes. }
  \item{bgdata}{data.frame. external data for node drawing.
    (passed to \code{node.function}). }
}
\details{
  \code{gcModify} is a tool to modify and plot graphs, created by
  \code{gcExplorer}, zoom certain areas of the plot and use grid 
  based functions to draw custom node plots.
}
\value{
Object of class \code{"graphdata"} with the following slots: an object of 
class \code{"Ragraph"} (see package \code{Rgraphviz}), \code{object}, 
\code{bgdata}, \code{node.function}, \code{edge.method}, \code{theme} and 
\code{colscale}.
}
\author{ Ingo Voglhuber }
\seealso{ \code{\link{gcExplorer}} }
\examples{
data("hsod")
library(flexclust)
set.seed(1111)
cl1 <- qtclust(hsod, radius = 2,
               family = kccaFamily(dist = distEuclidean,
	            cent = colMeans), save.data = TRUE)

## create Ragraph object from kcca object with gcExplorer
graph <- gcExplorer(cl1, theme = "blue", node.function = node.size)

## extract and modify clsim
clsim <- clusterSim(cl1)
clsim[clsim < 0.5] <- 0

## use modified clsim on Ragraph object to remove edges (<0.5)
gcModify(graph, clsim)

## use nodeDep=TRUE to delete nodes without edges
gcModify(graph, clsim, nodeDep = TRUE, zoom = "none")

## use zoom="auto" to center and maximize subgraph
gcModify(graph, clsim, nodeDep = TRUE, zoom = "auto")


\dontrun{
## R package symbols is available from Rforge: 
## http://r-forge.r-project.org/projects/symbols/

require("symbols")

## create a grid based plotting function: plot cluster data and centers in matplot.
gmatplot <- function (object, cluster, bgdata) {
        grid.rect()
        data <- object@data@get("designMatrix")
        ylimits <- c(min(data, na.rm = TRUE), max(data, na.rm = TRUE))
        index <- (object@cluster == cluster)
        nodedata <- data[index,]
        symb.matplot(1:ncol(nodedata), t(nodedata), type = "l", 
                col = "gray", ylim = ylimits, pch = 1)
        center <- object@centers[cluster,]
        symb.matplot(1:ncol(object@centers), center, type = "l",
                col = "red", ylim = ylimits, pch = 1)
}
## use grid based node function to draw nodes
gcModify(graph, clsim, nodeDep = TRUE, zoom = "auto", 
         node.function = gmatplot)
}
}
\keyword{hplot}
\keyword{methods}
