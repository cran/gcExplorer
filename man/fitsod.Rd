%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
%%%  $Id: fitsod.Rd 4249 2009-01-13 14:06:28Z scharl $
%%%
\name{fitsod}
\alias{fitsod}
\alias{hsod}
\alias{bn_hsod}
\alias{links_hsod}
\alias{gobp}
\docType{data}
\title{E. coli Fermentation Data}
\description{
E. coli Fermentation Fit Data Object containing M-values, P-values, GeneNames and Links zu NCBI. Output of limma function \code{write.fit} with links to NCBI added for each gene.
}
\usage{data(fitsod)
data(hsod)
data(gobp)
}
\format{
  A data frame with 4368 observations on the following variables.
  \describe{
    \item{\code{A}}{a numeric vector giving the mean A-values}
    \item{\code{Coef.stress3A}}{a numeric vector for each coefficient giving the estimated coefficient for a particular gene for the contrast to the reference}
     \item{\code{Coef.stress3B}}{a numeric vector for each coefficient giving the estimated coefficient for a particular gene for the contrast to the reference}
      \item{\code{Coef.stress3C}}{a numeric vector for each coefficient giving the estimated coefficient for a particular gene for the contrast to the reference}
    \item{\code{Coef.stress3F}}{a numeric vector for each coefficient giving the estimated coefficient for a particular gene for the contrast to the reference}
    \item{\code{Coef.stress4}}{a numeric vector for each coefficient giving the estimated coefficient for a particular gene for the contrast to the reference}
     \item{\code{Coef.stress4A}}{a numeric vector for each coefficient giving the estimated coefficient for a particular gene for the contrast to the reference}
    \item{\code{Coef.stress5A}}{a numeric vector for each coefficient giving the estimated coefficient for a particular gene for the contrast to the reference}
     \item{\code{Coef.stress6}}{a numeric vector for each coefficient giving the estimated coefficient for a particular gene for the contrast to the reference}
    \item{\code{t.stress3A}}{a numeric vector giving the t-statistic to the coefficient estimate}
    \item{\code{t.stress3B}}{a numeric vector giving the t-statistic to the coefficient estimate}
    \item{\code{t.stress3C}}{a numeric vector giving the t-statistic to the coefficient estimate}
    \item{\code{t.stress3F}}{a numeric vector giving the t-statistic to the coefficient estimate}
    \item{\code{t.stress4}}{a numeric vector giving the t-statistic to the coefficient estimate}
    \item{\code{t.stress4A}}{a numeric vector giving the t-statistic to the coefficient estimate}
    \item{\code{t.stress5A}}{a numeric vector giving the t-statistic to the coefficient estimate}
    \item{\code{t.stress6}}{a numeric vector giving the t-statistic to the coefficient estimate}
    \item{\code{p.value.stress3A}}{a numeric vector giving the corresponding p-value}
    \item{\code{p.value.stress3B}}{a numeric vector giving the corresponding p-value}
    \item{\code{p.value.stress3C}}{a numeric vector giving the corresponding p-value}
    \item{\code{p.value.stress3F}}{a numeric vector giving the corresponding p-value}
    \item{\code{p.value.stress4}}{a numeric vector giving the corresponding p-value}
    \item{\code{p.value.stress4A}}{a numeric vector giving the corresponding p-value}
   \item{\code{p.value.stress5A}}{a numeric vector giving the corresponding p-value}
    \item{\code{p.value.stress6}}{a numeric vector giving the corresponding p-value}
    \item{\code{F}}{a numeric vector giving the overall F-statistik}
    \item{\code{F.p.value}}{a numeric vector giving the corresponding F-p-value}
    \item{\code{Genes.Block}}{first block position of the gene}    
    \item{\code{Genes.Row}}{first row position of the gene}
    \item{\code{Genes.Column}}{first column position of the gene}
    \item{\code{Genes.GeneName}}{Short genename}
    \item{\code{Genes.ID}}{Gene ID}
    \item{\code{Genes.AccessionReference}}{Blattner numbers}
    \item{\code{Genes.Status}}{status of the gene: always gene}
    \item{\code{links}}{link to NCBI}
}}
\details{
The data set \code{hsod} is a filtered subset of the original data. It contains 527 differentially expressed genes at the 8 time points. The vector \code{bn_hsod} contains the corresponding identifiers which can be used to search for functional groups in the data set \code{gobp}. \code{links_hsod} contains the corresponding links to the NCBI database.

\code{gobp} ist a data set listing functional groups to gene identifiers. The data set consists of 8726 observations, the first column gives the gene identifier, the second column gives the gene name and the third column gives the functional group.
}
\references{
  Duerrschmid, K., Reischer, H., Schmidt-Heck, W., Hrebicek, T., Guthke, R., Rizzi, A., Bayer, K. (2008). Monitoring of transcriptome and proteome profiles to investigate the cellular response of E. coli towards recombinant protein expression under defined chemostat conditions. Journal of Biotechnology 135, 34--44.
}
\examples{
data(fitsod)
}
\keyword{datasets}
