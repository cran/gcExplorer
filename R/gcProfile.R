#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: gcProfile.R 4249 2009-01-13 14:06:28Z scharl $
#

setGeneric("gcProfile", function(object, ...)
    standardGeneric("gcProfile"))

setMethod("gcProfile", signature(object="kccasimple"),
function(object, which, data = NULL, cexl=0.8, xlab="", ylab="M", ylim=c(-6,6), cex.axis=1, xlabels=NULL, opar = par(las=1, mar=c(5, 4, 2, 0.5) + 0.1), data.type=c("time", "other"), legend=TRUE, ...)
{
     if (is.null(data))
         data <- flexclust:::getData(object)

     data.type <- match.arg(data.type)
     if(data.type=="time")
     {
         if (is.null(xlabels))
             xlabels <- 0:ncol(data)

         else if (length(xlabels) == ncol(data)+1)
             xlabels <- xlabels

         else if (length(xlabels) == ncol(data))
             xlabels <- c(0,xlabels)

         else
             stop(paste("Length of Vector of xlables (", length(xlabels), ") does not fit to dimensions of cluster result (", ncol(data),").",sep=""))

         if (is.null(names(xlabels))) {

             if (is.null(colnames(data)))
                 names(xlabels) <- as.character(xlabels)

             else names(xlabels) <- c("",colnames(data))

         }

     }

     else
     {
         if (is.null(xlabels))
             xlabels <- 1:ncol(data)

         if (is.null(colnames(data)))
             names(xlabels) <- as.character(xlabels)
 
         else names(xlabels) <- colnames(data)
     }

     if (is.null(rownames(data))) rownames(data) <- as.character(1:nrow(data))

     o <- which(object@cluster==which)
     opar
     spalten <- 1:ncol(data)

     plotdata <- data[o,spalten]
     rangedata <- range(plotdata)
     legpos <- which.max(abs(rangedata-ylim))
     if(legpos==1) loc <- "bottomleft"
     else loc <- "topleft"

     if (data.type=="time")
     {
         matplot(xlabels, t(cbind(0,data[o,spalten])),
             type="l", col=1:6, pch=1,
             lty=rep(1:5, length.out=length(o)),
             main=paste("Cluster", which, sep=" "), ylim=ylim, 
             ylab=ylab, xlab=xlab, cex.axis=cex.axis, ...)

         if (legend)
             legend(loc, as.character(rownames(data)[o]),
                 ncol=5, col=1:6,
                 lty=rep(1:5,length.out=length(o)),
                 cex=cexl, inset=0.05)
     }

     else
     {
         matplot(xlabels, t(data[o,spalten]), type="l",
             col=1:6, lty=rep(1:5, length.out=length(o)),
             main=paste("Cluster", which, sep=" "), ylim=ylim, 
             ylab=ylab, xlab=xlab, xaxt="n", pch=1, ...)
         axis(1, xlabels, names(xlabels), cex.axis=cex.axis)

         if (legend)
             legend(loc, as.character(rownames(data)[o]),
                 ncol=5, col=1:6,
                 lty=rep(1:5,length.out=length(o)),
                 cex=cexl, inset=0.05)
     }

})

