%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl, Ingo Voglhuber
%%%  $Id: gcExplorer.Rd 4249 2009-01-13 14:06:28Z scharl $
%%%
\name{gcExplorer}
\docType{methods}
\alias{gcExplorer}
\alias{gcExplorer,kccasimple-method}
\alias{graphdata-class}
\alias{calcHCL}
\title{Graphical Cluster Explorer}
\description{
Plot a neighborhood graph for 'kccasimple' cluster solutions.
}
\usage{
\S4method{gcExplorer}{kccasimple}(object, layout = c("dot", "neato", "twopi","circo","fdp"), 
   theme = "grey", edge.method=c("orig","mean","min","max"),
   node.function = NULL, node.args = NULL, doViewPort = FALSE,
   filt = 0.1, interactive = !is.null(panel.function), dev=c("one","many"),
   panel.function = NULL, panel.args = NULL, bgdata = NULL,
   colscale = NULL, mfrow = c(1,1), legend.pos = "none")
}
\arguments{
  \item{object}{Object of class \code{kccasimple}.}
  \item{layout}{Layout method used: One of 'dot', 'neato', 'twopi', 'circo', and 'fdp'.}
  \item{theme}{Color theme used.}
  \item{edge.method}{Several methods are available to draw edges: 'orig', 'mean',
     'min', and 'max', see details below.}
  \item{node.function}{Optional. Additional information about the clusters can be included in the representation of nodes. Either a function calculating node colors or a grid--based function (see \code{doViewPort}).}
  \item{node.args}{List of arguments which should be passed to \code{node.function}.}
  \item{doViewPort}{Currently not used in release version of the package. Call a grid--based function specified 
     by argument \code{node.function} and use it for node representation?}
  \item{filt}{Cutoff value for similarities between clusters, edges above the threshold will be displayed.}
  \item{interactive}{Should the plot be interactive?}
  \item{dev}{Only used if \code{interactive=TRUE}. Display each cluster plot (specified by \code{panel.function}) in one device or open new devices for each cluster when clicking on a node.}
  \item{panel.function}{Only used if \code{interactive=TRUE}. The panel function
     which should be used to display the corresponding cluster}
  \item{panel.args}{List of arguments which should be passed to \code{panel.function}.}
  \item{bgdata}{Background data to be plotted by \code{panel.function} or \code{node.function}.}
  \item{colscale}{A vector of length 2 specifying the color range for edges and 
     nodes, e.g. c(0,0.5).}
  \item{mfrow}{Only used if \code{interactive=TRUE}. The panel layout in which the panel plots should be displayed.}
  \item{legend.pos}{Position of the legend.}
}
\details{
A neighborhood graph is the default plot method for cluster objects of 
package \code{flexclust}. For large and highdimensional data sets like 
microarray data linear projection of the data into two dimensions may 
not scale well in the number of clusters. In this case non-linear arrangement
of the nodes using layout algorithms from Graphviz can be helpful. An 
interface to Graphviz is provided in Bioconductor package \code{Rgraphviz}. 
One of the implemented layout algorithms can be selected using \code{layout}.

In a neighborhood graph each node corresponds to a cluster centroid. 
Two nodes are connected by an edge if there exist data points that have
these two centroids as closest and second closest. The edge weights are 
taken from \code{clusterSim(object)}. The similarity between two clusters is
bounded between 0 and 1 where well-separated clusters have values close to 0.
The larger the similarity between clusters the stronger the edge will be 
drawn in the graph. The cutoff value for drawing the edge between two 
centroids can be chosen by argument \code{filt}. The larger the filt value
the fewer edges will be drawn.

Originally the neighborhood graph is a directed graph. An edge will be 
drawn from centroid 1 to centroid 2 if there exists at least one data 
point that has centroid 1 as closest and centroid 2 as second closest. 
But there need not necessarily be a data point that has centroid 2 as 
closest and centroid 1 as second closest centroid. For this reason there
are several methods for plotting the edges between nodes. The default 
\code{edge.method} is 'orig' where each edge is drawn separately with 
its corresponding weight. This method will result in a directed graph. All
other edge methods yield undirected graphs where the mean, minimum or maximum
of the similarities between two clusters is used.

Additional information about the clusters can be included in the graph using 
\code{node.function} and \code{panel.function}. \code{node.function} is used 
for the node representation. If no \code{node.function} is given all nodes 
will be drawn in one color. The \code{node.function} can be used to calculate
different colors for the nodes like cluster size or cluster tightness. 
Additionally \code{node.function} can be a grid--based function displaying 
the data in the underlying cluster, e.g. a scatterplot or a boxplot.

\code{gcExplorer} is implemented interactively. If \code{interactive=TRUE}
\code{panel.function} is used to plot a cluster when clicking on the 
corresponding node. An example of a \code{panel.function} is given by function
\code{gcProfile}.

Function \code{calcHCL} is used to calculate a HCL--based color.
}
\value{
Object of class \code{graphdata} with the following slots: an object of class 
\code{Ragraph} (see package \code{Rgraphviz}), \code{object}, \code{bgdata}, 
\code{node.function}, \code{edge.method}, \code{theme} and \code{colscale}.
}
\seealso{\code{\link{node.tight}}}
\examples{
data("hsod")
cl1 <- qtclust(hsod, radius = 2, save.data = TRUE)

gcExplorer(cl1, theme = "blue", node.function = node.size)

}
\author{Theresa Scharl and Ingo Voglhuber}
\keyword{methods}
\keyword{hplot}
