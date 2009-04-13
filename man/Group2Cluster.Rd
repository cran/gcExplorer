\name{Group2Cluster}
\alias{Group2Cluster}
\alias{Random2Cluster}
\alias{DefinedCluster}
\title{Find clusters to a group}
\description{
Find the cluster memberships for a group and create the vector of all cluster memberships where the grouped elements are assigned to.
}
\usage{
Group2Cluster(object, gonr, source.group, source.id, id)
Random2Cluster(object, perc)
DefinedCluster(object, filt=0, numEdges=6, perc=1, noise=0)
}
\arguments{
  \item{object}{An object of class \code{kccasimple}}
  \item{gonr}{Unique identifier from \code{source.group} giving the group of
          genes to be extracted}
  \item{source.group}{Vector of functional groups where \code{source.id} are assigned to}
  \item{source.id}{Corresponding vector of identifiers to source.group}
  \item{id}{Vector of identifiers of the same length as rows in the
          clustered data of the same type as given in \code{source.id}}
  \item{perc}{For artificial assignment: the percentage of elements in a cluster that should be assigned to the group}
  \item{filt}{Edges above this threshold are taken into account}
  \item{numEdges}{Number of edges chosen where clusters are assigned similar amount of affected elements}
  \item{noise}{The percentage of noise that should be added (i.e., further assigned elements in different clusters)}
}
\value{
A vector of cluster memberships.
}
\author{Theresa Scharl}
\seealso{\code{edge.test}}
\examples{
data("hsod")
data("gobp")
set.seed(1111)
cl1 <- qtclust(hsod, radius = 2, save.data = TRUE)

g1 <- Group2Cluster(cl1, gonr = "GO:0009061", 
        source.group = gobp[,3], source.id=gobp[,1],
        id = bn_hsod)
table(g1)
}
\keyword{methods}

