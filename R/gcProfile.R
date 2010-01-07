#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: gcProfile.R 4379 2009-07-24 09:22:58Z scharl $
#

setGeneric("gcProfile", function(object, ...)
    standardGeneric("gcProfile"))

setMethod("gcProfile", signature(object="kccasimple"),
function(object, which, data = NULL, cexl=0.8, xlab="", ylab="M", ylim=c(-6,6), cex.axis=1, xlabels=NULL, opar = par(las=1, mar=c(5, 4, 2, 0.5) + 0.1), data.type=c("time", "other"), legend=TRUE, main = NULL, ...)
{
     if (is.null(data))
         data <- flexclust:::getData(object)

     data.type <- match.arg(data.type)
     if(data.type=="time")
     {
         if (is.null(xlabels))
             xlabels <- 0:ncol(data)

         else if (((class(xlabels) == "numeric") | (class(xlabels) == "integer")) && 
                 length(xlabels) %in% c(ncol(data):(ncol(data)+2)))

             xlabels <- xlabels

         else
             stop(paste("Length of Vector of xlabels (", length(xlabels), ") does not fit to dimensions of cluster result (", ncol(data),") or data type (",data.type,") does not fit to class of xlabels (", class(xlabels),").",sep=""))

         names(xlabels) <- as.character(xlabels)

     }

     else
     {
         if (is.null(xlabels))
         {
             xlabels <- 1:ncol(data)
             names(xlabels) <- colnames(data)
         }

         if(class(xlabels) == "character")
         {
             if(length(xlabels) != ncol(data))
                 stop(paste("Length of Vector of xlabels (", length(xlabels), ") 
                 does not fit to dimensions of cluster result (",
                 ncol(data),").",sep=""))

             xla <- 1:length(xlabels)
             names(xla) <- xlabels
             xlabels <- xla
         }

     }

     if (is.null(rownames(data))) rownames(data) <- as.character(1:nrow(data))

     o <- which(object@cluster==which)
     opar

     plotdata <- data[o,]
     rangedata <- range(plotdata)
     legpos <- which.max(abs(rangedata-ylim))
     if(legpos==1) loc <- "bottomleft"
     else loc <- "topleft"

     if(is.null(main)) main <- paste("Cluster", which, sep=" ")

     if (data.type=="time")
     {

         if (length(xlabels) == ncol(data)) pdata <- data[o,]
         else if (length(xlabels) == ncol(data)+1) pdata <- cbind(0,data[o,])
         else if (length(xlabels) == ncol(data)+2) pdata <- cbind(0,0,data[o,])

         matplot(xlabels, t(pdata),
             type="l", col=1:6, pch=1,
             lty=rep(1:5, length.out=length(o)),
             main=main, ylim=ylim, 
             ylab=ylab, xlab=xlab, cex.axis=cex.axis, ...)

         if (legend)
             legend(loc, as.character(rownames(data)[o]),
                 ncol=5, col=1:6,
                 lty=rep(1:5,length.out=length(o)),
                 cex=cexl, inset=0.05)
     }

     else
     {
         matplot(xlabels, t(data[o,]), type="l",
             col=1:6, lty=rep(1:5, length.out=length(o)),
             main=main, ylim=ylim, 
             ylab=ylab, xlab=xlab, xaxt="n", pch=1, ...)
         axis(1, xlabels, names(xlabels), cex.axis=cex.axis)

         if (legend)
             legend(loc, as.character(rownames(data)[o]),
                 ncol=5, col=1:6,
                 lty=rep(1:5,length.out=length(o)),
                 cex=cexl, inset=0.05)
     }

})

