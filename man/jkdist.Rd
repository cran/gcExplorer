%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
%%%  $Id: jkdist.Rd 4249 2009-01-13 14:06:28Z scharl $
%%%
\name{jkdist}
\alias{distJackCor}
\alias{distJackEuc}
\alias{distJackMan}
\alias{distJackMax}
\alias{centSpline}
\title{Further Distance and Centroid Computations}
\description{
Helper functions to create 'kccaFamily' objects.
}
\usage{
distJackCor(x, centers)
distJackEuc(x, centers)
distJackMan(x, centers)
distJackMax(x, centers)

centSpline(d)
}
\arguments{
  \item{x}{A data matrix}
  \item{d}{A data matrix}
  \item{centers}{A matrix of centroids}
}
\details{
A possible problem using classical distance measures for clustering time--course gene expression data is that single outlier variables can completely change the expression pattern of certain genes. Outliers at special time points are very common in microarray experiments as technical problems like dust or a scratch on the slide can easily distort the data. In such a case these outlier variables can lead to unwanted correlations between genes and to incorrect assignment to clusters. There is a need for distance measures which are robust against outlier variables. The idea of Jackknife (Efron, 1982) distance measures is not to exclude the whole observation for such a gene but rather one or several variables. We want to introduce so--called "Jackknife" distance measures which can handle one outlier time point.  The so-called Jackknife correlation was first used by Heyer et al. (1999) to cluster gene expression data. It is defined as  
\deqn{d_{xy} = 1 - \min(\rho_{xy}^{(1)}, \rho_{xy}^{(2)}, \ldots, \rho_{xy}^{(T)})}{d_xy = 1 - min(rho_xy^(1), rho_xy^(2), \ldots, rho_xy^(T))}
where  \eqn{\rho_{xy}^{(t)}}{rho_xy^(t)} is the correlation of pair x,y computed with the t-th time point deleted. 

This concept can be extended for the three geometric distance measures Euclidean, Manhattan and Maximum distance. Jackknife Euclidean distance is defined as
\deqn{d_{xy} = \min(d_{xy}^{(1)}, d_{xy}^{(2)}, \ldots, d_{xy}^{(T)})}{d_xy = min(d_xy^(1), d_xy^(2), \ldots, d_xy^(T))}
where  \eqn{d_{xy}^{(t)}}{d_xy^(t)} is the Euclidean distance of pair x,y computed with the t-th time point deleted. Jackknife Manhattan distance and Jackknife Maximum distance can be defined in the same way.
}
\references{Theresa Scharl and Friedrich Leisch: Jackknife distances for clustering time--course gene expression data, in JSM Proceedings 2006}

\author{Theresa Scharl}
\keyword{cluster}
