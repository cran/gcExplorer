#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: gcTable.R 4249 2009-01-13 14:06:28Z scharl $
#

setGeneric("gcTable", function(object, ...)
    standardGeneric("gcTable"))

setMethod("gcTable", signature(object="kccasimple"),
function(object, which, links, file="gcTable", ...) {

    mvalues <- object@data@get("designMatrix")
    o <- which(object@cluster==which)

    if(length(object@cluster) != length(links))
        stop("Vector of 'links' does not fit to cluster result")

    out <- cbind(rownames(mvalues),links,mvalues)[o,]

    if (which %in% 1:9) number <- paste("00",which,sep="")
    else if (which %in% 10:99) number <- paste("0",which,sep="")
    else number <- which

    write.htmltable(out, file=paste(file,number,sep="-"),  
                    title=paste("Cluster",number,sep=" "), ...)

    file <- paste(paste(file,number,sep="-"),"html",sep=".")
    wd <- getwd()
    if(interactive()) 
        browseURL(paste(wd,file,sep="/"))

})
