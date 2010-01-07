\name{edgeTest}
\alias{edgeTest}
\alias{comp_diff}
\alias{newclsim}
\title{Functional Relevance Test}
\description{
Perform a functional relevance test on the edges of a neighborhood graph
}
\usage{
edgeTest(object, min.size = 1, group, N = 500, filt = 0.1, 
    useNH = TRUE, quant = 0.95)
}
\arguments{
  \item{object}{An object of class \code{"kccasimple"}.}
  \item{min.size}{Minimum number of grouped genes in a cluster to be considered for testing}
  \item{group}{Vector of cluster memberships of functionally grouped genes (from function \code{Group2Cluster}).}
  \item{N}{Number of permutations.}
  \item{filt}{Threshold for edges in the neighborhood graph to be considered for testing.}
  \item{useNH}{Use the neighborhood structure or test all combination of nodes?}
  \item{quant}{The defined quantile of the maxima of the permuted average distances.}
}
\value{
A list consisting of the matrix \code{res} and the defined quantile \code{quant} of the maxima of the permuted average distances. The matrix \code{res} gives the cluster sizes, the difference in proportions and the corresponding p-value for each edge considered.
}
\author{Theresa Scharl}
\seealso{\code{Group2Cluster}}
\examples{
data("hsod")
data("gobp")
set.seed(1111)
cl1 <- qtclust(hsod, radius = 2, save.data = TRUE)

g1 <- Group2Cluster(cl1, gonr = "GO:0009061", 
        source.group = gobp[,3], source.id=gobp[,1], 
        id = bn_hsod)
test1 = edgeTest(cl1, group=g1, min.size=2, useNH=TRUE, filt=0.1, N=1000)
}
\keyword{htest}
