%%%
%%%  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
%%%  $Id: gcOffline.Rd 4299 2009-03-05 10:00:32Z scharl $
%%%
\name{gcOffline}
\docType{methods}
\alias{gcOffline}
\alias{gcOffline,kccasimple-method}
\title{Offline gcExplorer}
\description{
Save \code{gcExplorer} plots or tables to a file.
}
\usage{
\S4method{gcOffline}{kccasimple}(object, panel.function, panel.args=NULL,
    type=pdf, file="gcOffline", which=NULL, html=FALSE, ...)
}
\arguments{
  \item{object}{Object of class \code{"kccasimple"}.}
  \item{panel.function}{Only used if \code{interactive=TRUE}. The panel function
     which should be used to display the corresponding cluster}
  \item{panel.args}{List of arguments which should be passed to \code{panel.function}.}
  \item{type}{Create graphics of type \code{type}, e.g., pdf, postscript, jpeg, png.}
  \item{file}{File name prefix used for graphics files. Of the form file-which.type and file-graph.type.}
  \item{which}{A vector specifying if all cluster plots (default) or only a subset should be created.}
  \item{html}{Logical. Does the \code{panel.function} produce HTML tables.}
  \item{...}{Further arguments can be passed to \code{gcExplorer}.}
}
\seealso{\code{\link{gcTable}}, \code{\link{gcProfile}}}
\examples{
data("hsod")
set.seed(1111)
cl1 <- qtclust(hsod, radius=2, save.data=TRUE)

# create three files: hsod-003.pdf, hsod-005.pdf, hsod-graph.pdf
gcOffline(cl1,panel.function=gcProfile, file="hsod", which=c(3,5))

# create two files: hsod-003.html, hsod-005.html
gcOffline(cl1, panel.function = gcTable, html = TRUE, 
          panel.args = list(links = links_hsod),
          file = "hsod", which=c(3,5))

 # tidy up
unlink(list.files()[grep("hsod-",list.files())])
}
\author{Theresa Scharl}
\keyword{methods}
\keyword{hplot}
