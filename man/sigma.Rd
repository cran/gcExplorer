\name{sigma}
\alias{sigma}
\alias{reg}
\docType{data}
\title{E. coli Sigma Factors and Global Regulators}
\description{
The E. coli sigma factors and the genes they regulate.
}
\usage{data(sigma)}
\format{
  A data frame with 1851 observations on the following 6 variables.
  \describe{
    \item{\code{SigmaFactor}}{a factor with levels \code{Sigma19} \code{Sigma24} \code{Sigma28} \code{Sigma32} \code{Sigma38} \code{Sigma54} \code{Sigma70}}
    \item{\code{SigmaGene}}{a factor with levels \code{fecI} \code{fliA} \code{rpoD} \code{rpoDS} \code{rpoE} \code{rpoH} \code{rpoN} \code{rpoS}}
    \item{\code{RegulatedGeneName}}{The genename of the regulated genes.}
    \item{\code{RegulatedGenebnumber}}{The Blattner numbers of the regulated genes.}
    \item{\code{function}}{a factor with levels \code{+}}
    \item{\code{GeneType}}{a factor with levels \code{} \code{Phantom Gene} \code{Pseudo Gene}}
  }
}
\source{
http://regulondb.ccg.unam.mx/LicenseRegulonDBd.jsp
}
\references{
Salgado H, Gama-Castro S, Peralta-Gil M, Diaz-Peredo E, Sanchez-Solano F,
Santos-Zavaleta A, Martinez-Flores I, Jimenez-Jacinto V,
Bonavides-Martinez C, Segura-Salazar J, Martinez-Antonio A, Collado-Vides J.
RegulonDB (version 5.0): Escherichia coli K-12 transcriptional
regulatory network, operon organization, and growth conditions
 Nucleic Acids Res. 2006 Jan 1;34(Database issue):D394-7
}
\examples{
data(sigma)
data(reg)
}
\keyword{datasets}
