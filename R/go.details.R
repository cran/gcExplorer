#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: go.details.R 4299 2009-03-05 10:00:32Z scharl $
#

setGeneric("go.details",
           function(object, ...) standardGeneric("go.details"))

setMethod("go.details", signature(object="data.frame"),
function(object, mvalues, gn, id, stats, links, gonr, source.id, source.group, 
    details = c("size", "names", "id", "data"), 
    table = TRUE, file = "go.details", plot = TRUE, cexl = 0.8, xlab = "", xlabels=NULL, 
    ylab = "M", ylim = c(-6,6), cex.axis = 1, ...)
{
    mvalues <- object[,mvalues]

    if (is.null(xlabels))
        xval <- 1:ncol(mvalues)
    else xval <- xlabels

    names(xval) <- as.character(xval)

    gn <- object[,gn]
    id <- object[,id]
    links <- object[,links]
    stats <- object[,stats]

    go <- source.group
    bnumber <- source.id

    gmain <- gonr

    values <- grep(as.character(gonr),as.character(go), ignore.case=TRUE)

    if(length(values)<1)
        return("NA")

    else 
    {
        genes <- bnumber[values]
        genes <- unique(genes)
        genes <- genes[complete.cases(genes)]
        result <- NULL
        for (i in 1:length(genes)) 
           result <- c(result, grep(as.character(genes)[i],
                       as.character(id), ignore.case=TRUE))

        if(length(result)<1)
            stop("No genes found for this group.")

        details <- match.arg(details)
        if(details=="size")
            print(length(result))
        else if(details=="names")
            print(as.character(gn[result]))
        else if(details=="id")
            print(as.character(id[result]))
        else {
            gn1 <- paste(1:length(result),gn[result],sep=".")
            gdata <- mvalues[result,]
            rownames(gdata) <- gn1
            return(gdata)}

        if(plot)
        {
            matplot(xval, t(mvalues[result,]),type="l", col=1:6,
                    lty=rep(1:5,length.out=length(result)), xaxt="n", 
                    main=paste(gmain), ylim=ylim, ylab=ylab, xlab=xlab,
                    ...)
            axis(1, xval, labels(xval), cex.axis=cex.axis)
            legend(1, ylim[2], as.character(gn[result]), ncol=5, 
                   col=1:6, lty=rep(1:5,length.out=length(result)), cex=cexl)
        }

        if(table)
        {
           out <- cbind(gn,id,links,mvalues,stats)
           write.htmltable(out[result,], file=file, title=paste(gmain))
           file <- paste(file,"html",sep=".")
           wd <- getwd()
           if(interactive()) 
              browseURL(paste(wd,file,sep="/"))
         }

    }
})

