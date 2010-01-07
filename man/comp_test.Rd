\name{comp_test}
\alias{comp_test}
\alias{pvalFromPermMat}
\alias{comp_perm}
\alias{comp_dist}
\title{Compare Cluster Results}
\description{
Cluster validation by testing the validity of a cluster solution under different experimental conditions.
}
\usage{
comp_test(data, cll, N = 500, quant=0.05, ...)
}
\arguments{
  \item{data}{Dataset with the same number of rows as the clustered dataset.}
  \item{cll}{Vector of cluster memberships of the clustered dataset.}
  \item{N}{Number of permutations.}
  \item{quant}{The defined quantile for the permuted average distances.}
  \item{\dots}{Further arguments can be passed to the subfunctions.}
}
\value{
A matrix giving for each cluster the size of the cluster, the observed average within cluster distance to the computed cluster center in the new dataset, the defined quantile for the permuted average distances and the p-values, i.e., the proportion of permutations where the observed within cluster distance is lower than the permuted.
}
\author{Theresa Scharl}
\examples{
data(comp19)
set.seed(1111)
cl3 <- qtclust(comp19,radius=1.5,family=kccaFamily(dist=distEuclidean,
   cent=colMeans),save.data=TRUE,control=list(min.size=5))
cl3

ct1 <- comp_test(comp17, clusters(cl3), N=1000)
ct1
}
\keyword{htest}
