%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
%%%  $Id: go.details.Rd 4379 2009-07-24 09:22:58Z scharl $
%%%
\name{go.details}
\docType{methods}
\alias{go.details}
\alias{go.details,data.frame-method}
\title{Functional Group Methods}
\description{
Plot or extract size, members or data of a functional group}
\usage{
\S4method{go.details}{data.frame}(object, mvalues, gn, id, stats, links, gonr,
    source.id, source.group, details = c("size", "names", "id", "data"), 
    table = TRUE, file = "go.details", plot = TRUE, cexl = 0.8, 
    xlab = "", xlabels = NULL, ylab = "M", ylim = c(-6,6), cex.axis = 1, 
    main = NULL, data.type = c("time", "other"), legend = TRUE, ...)
}
\arguments{
  \item{object}{An object of class \code{"data.frame"}.}
  \item{mvalues}{Vector giving the columns in \code{object} which correspond to the gene expression values.}
  \item{gn}{Column of \code{object} which corresponds to the gene names used for representation.}
  \item{id}{Column of \code{object} which corresponds to the unique IDs of the same type as given in \code{source.id}.}
  \item{links}{Column of \code{object} which corresponds to links to database.}
  \item{stats}{Column(s) of \code{object} which correspond to statistics.}
  \item{gonr}{Unique identifier from \code{source.group} giving the group of genes to be extracted.}
  \item{source.id}{Vector of gene IDs assigned to functional groups given in \code{source.group}.}
  \item{source.group}{Vector of the same length as \code{source.id}.}
  \item{details}{The type of details to be extracted.}
  \item{table}{Logical. Should an html table be created.}
  \item{file}{The file where the output of 'gotable' will be written.}
  \item{plot}{Logical. Should the genes be plotted.}
  \item{cexl}{Point size of the legend.}
  \item{xlab}{Label for the x-axis.}
 \item{xlabels}{Either a numeric vector of time points giving the positions on the x-axis or a character vector with names of the positions on the x-axis.}
  \item{ylab}{Label for the y-axis.}
  \item{ylim}{Range of the y-axis.}
  \item{cex.axis}{Point size of the axis.}
  \item{main}{The main title of the plot or html table. If null the name of the functional group is used.}
  \item{data.type}{The data type is either on a time scale (default) or something else ("other").}
  \item{legend}{Draw a legend?}
  \item{\dots}{Further arguments can be passed to \code{matplot} or \code{write.htmltable}.}
}
\author{Theresa Scharl}
\seealso{\code{\link{fitsod}}}
\examples{
data(fitsod)
data(gobp)

## Plot the functional group
go.details(fitsod, mvalues = 2:9, gn = 31, id = 33, links = 35, stats = 26,
        gonr = "flagellar", source.group = gobp[,3], source.id = gobp[,1],
        plot = TRUE)

## A file named "go.details.html" will be created in the current 
## working directory.
go.details(fitsod, mvalues = 2:9, gn = 31, id = 33, links = 35, stats = 26,
        gonr = "flagellar", source.group = gobp[,3], source.id = gobp[,1],
        table = TRUE)

## Names of the genes in functional group "flagellar"
go.details(fitsod, mvalues = 2:9, gn = 31, id = 33, links = 35, stats = 26,
        gonr = "flagellar", source.group = gobp[,3], source.id = gobp[,1],
        details = "names")

## Gene expression values of the functional group
d1 <- go.details(fitsod, mvalues = 2:9, gn = 31, id = 33, links = 35, stats = 26,
        gonr = "flagellar", source.group = gobp[,3], source.id = gobp[,1],
        details = "data")
dim(d1)

}
\keyword{methods}
\keyword{hplot}
