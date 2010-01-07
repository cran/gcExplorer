\name{ps19}
\alias{ps19}
\alias{bn_ps19}
\alias{links_ps19}
\alias{f}
\alias{comp19}
\alias{comp17}
\docType{data}
\title{E. coli Fermentation Data}
\description{
E. coli Fermentation Data - Transcription profiling of E. coli HMS174(DE3)(pET30aNproGFPmut3.1) -
cellular response to limited induction with IPTG
}
\usage{data(ps19)}
\format{
  A data frame with 918 observations on the following 10 variables.
  \describe{
    \item{\code{10}}{Estimated coefficient for a particular gene for the contrast of the sample 10 hours past induction to the sample before induction}
    \item{\code{12}}{Estimated coefficient for a particular gene for the contrast of the sample 12 hours past induction to the sample before induction}
    \item{\code{14}}{Estimated coefficient for a particular gene for the contrast of the sample 14 hours past induction to the sample before induction}
    \item{\code{16}}{Estimated coefficient for a particular gene for the contrast of the sample 16 hours past induction to the sample before induction}
    \item{\code{18}}{Estimated coefficient for a particular gene for the contrast of the sample 18 hours past induction to the sample before induction}
    \item{\code{20}}{Estimated coefficient for a particular gene for the contrast of the sample 20 hours past induction to the sample before induction}
    \item{\code{22}}{Estimated coefficient for a particular gene for the contrast of the sample 22 hours past induction to the sample before induction}
    \item{\code{24}}{Estimated coefficient for a particular gene for the contrast of the sample 24 hours past induction to the sample before induction}
    \item{\code{26}}{Estimated coefficient for a particular gene for the contrast of the sample 26 hours past induction to the sample before induction}
    \item{\code{28}}{Estimated coefficient for a particular gene for the contrast of the sample 28 hours past induction to the sample before induction}
  }
}
\source{
Two experiments (including all processing protocols) have been loaded into ArrayExpress (http://www.ebi.ac.uk/microarray-as/ae/). The ArrayExpress accession number of the array design is A-MARS-10. The experiment with fully induced E. coli expression system (ps19) has accession number E-MARS-16 and the experiment with partially induced system (ps17) has accession number E-MARS-17.
}
\references{
T. Scharl, G. Striedner, F. Poetschacher, F. Leisch and K. Bayer: Interactive visualization of clusters in microarray data: an efficient tool for improved metabolic analysis of E. coli. \emph{Microbial Cell Factories}, 8:37, 2009.
}
\examples{
data(ps19)
}
\keyword{datasets}
