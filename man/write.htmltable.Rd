%%%
%%%  Copyright (C) 2003 Andreas Buness and Wolfgang Huber
%%%  $Id: write.htmltable.Rd 4299 2009-03-05 10:00:32Z scharl $
%%%
\name{write.htmltable}
\alias{write.htmltable}
\title{Write a data frame into an html table within a html page}
\description{Write a \code{"data.frame"} into an html table within a html page}
\usage{
write.htmltable(x, filename, title="", sortby=NULL, decreasing=TRUE,
    open="wt", formatNumeric=function(x) paste(signif(x, 3)))}
\arguments{
  \item{x}{\code{data.frame}.}
  \item{filename}{character. File name.}
  \item{title}{character. Title of html page.}
  \item{sortby}{character. Name of column by which to sort the table rows.}
  \item{decreasing}{logical. Should the sort order be increasing or decreasing?}
  \item{open}{character. This argument is passed on to \code{\link{file}}.}
  \item{formatNumeric}{function that takes a numeric and returns a
    character. This function is called for all numeric values in the table.}
}

\details{
This function is taken from package \code{arrayMagic}.}

\value{The function is called for its side effect: writing a file.}

\author{Wolfgang Huber \url{http://www.dkfz.de/mga/whuber}}

\examples{
out = tempfile()

n  = 10
ex = data.frame(genename=paste("Gene", 1:n, sep=""), score=
       signif(16*runif(n)), database=paste("http://super.data.base/?id",
       round(1e9*runif(n)), sep=""))

write.htmltable(ex, out, "Hi there", sortby="score")

cat("Now have a look at ", out, ".html\n", sep="")
}
\keyword{IO}
