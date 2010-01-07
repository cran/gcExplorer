\name{oxygen}
\docType{data}
\alias{oxygen}
\alias{bn_oxy}
\title{Preprocessed microarray oxygen deprivation data}
\description{Normalized gene
expression microarray data from Escherichia coli (E. coli).
}
\usage{
data(oxygen)
}
\format{
  \code{oxygen} is a data matrix containing n=43 experiments
  of various mutants under oxygen deprivation (Covert et al., 2004). The
  mutants were designed to monitor the response from E. coli during an
  oxygen shift in order to target the a priori most relevant part of the
  transcriptional network by using six strains with knockouts of five
  key transcriptional regulators in the oxygen response (\emph{arcA},
  \emph{appY}, \emph{fnr}, \emph{oxyR} and \emph{soxS}). The data was
  obtained by downloading the corresponding CEL files from the Gene
  Expression Omnibus (\url{http://www.ncbi.nlm.nih.gov/geo}) under
  accession \code{GDS680} and then normalized using the \code{rma()}
  function from the \code{affy} package. Following the steps described in
  (Castelo and Roverato, 2008) probesets were mapped to Entrez Gene
  Identifiers and filtered such that the \code{ExpressionSet} in the 
  \code{qpgraph} package names \code{EcoliOxygen} contains a total 
  of p=4205 genes.
  Here a subset of the EcoliOxygen data was used containing all genes
  where Blattner numbers were available.
}
\source{
Covert, M.W., Knight, E.M., Reed, J.L., Herrgard, M.J., and Palsson, B.O.
Integrating high-throughput and computational data elucidates bacterial networks.
\emph{Nature}, 429(6987):92-96, 2004.

Gama-Castro, S., Jimenez-Jacinto, V., Peralta-Gil, M., Santos-Zavaleta, A.,
Penaloza-Spinola, M.I., Contreras-Moreira, B., Segura-Salazar, J., Muniz-Rascado,
L., Martinez-Flores, I., Salgado, H., Bonavides-Martinez, C., Abreu-Goodger, C.,
Rodriguez-Penagos, C., Miranda-Rios, J., Morett, E., Merino, E., Huerta, A.M.,
Trevino-Quintanilla, L., and Collado-Vides, J. RegulonDB (version 6.0): gene
regulation model of Escherichia coli K-12 beyond transcription, active
(experimental) annotated promoters and Textpresso navigation.
\emph{Nucleic Acids Res.}, 36(Database issue):D120-124, 2008.

Castelo, R. and Roverato, A. Reverse engineering molecular regulatory
networks from microarray data with qp-graphs.
\emph{J. Comp. Biol.}, 16(2):213-227, 2009.

}
\note{This dataset was taken from Bioconductor package \code{qpgraph} and modified}
\examples{
data(oxygen)
}
\keyword{datasets}
