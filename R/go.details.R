#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: go.details.R 4498 2010-01-07 13:35:42Z scharl $
#

setGeneric("go.details",
           function(object, ...) standardGeneric("go.details"))

setMethod("go.details", signature(object="data.frame"),
function(object, mvalues, gn, id1, stats, links, gonr, source.id, source.group, 
    details = c("size", "names", "id", "data"), ptype = c("matplot", "barplot"), 
    table = TRUE, file = "go.details", plot = TRUE, cexl = 0.8, xlab = "", xlabels=NULL, 
    ylab = "M", ylim = c(-6,6), cex.axis = 1, main = NULL, data.type = c("time", "other"), legend = TRUE, ...)
{
     mvalues <- object[,mvalues]

     data.type <- match.arg(data.type)
     if(data.type=="time")
     {
         if (is.null(xlabels)) {
             xval <- 0:ncol(mvalues)
             mvalues <- cbind(0,mvalues) }

         else if (((class(xlabels) == "numeric") | (class(xlabels) == "integer")) && 
                 length(xlabels) %in% c(ncol(mvalues):(ncol(mvalues)+2))) {

             xval <- xlabels
             if(length(xlabels) == ncol(mvalues)+1) mvalues <- cbind(0,mvalues)
             else if(length(xlabels) == ncol(mvalues)+2) mvalues <- cbind(0,0,mvalues)

         }

         else
             stop(paste("Length of Vector of xlabels (", length(xlabels), ") does not fit to dimensions of cluster result (", ncol(mvalues),") or data type (",data.type,") does not fit to class of xlabels (", class(xlabels),").",sep=""))

         names(xval) <- as.character(xval)

     }

     else
     {
         if (is.null(xlabels))
         {
             xval <- 1:ncol(mvalues)
             names(xval) <- colnames(mvalues)
         }

         if(class(xlabels) == "character")
         {
             if(length(xlabels) != ncol(mvalues))
                 stop(paste("Length of Vector of xlabels (", length(xlabels), ") 
                 does not fit to dimensions of cluster result (",
                 ncol(mvalues),").",sep=""))

             xla <- 1:length(xlabels)
             names(xla) <- xlabels
             xval <- xla
         }

     }

    gn <- object[,gn]
    id1 <- object[,id1]
    links <- object[,links]
    stats <- object[,stats]

    go <- source.group
    bnumber <- source.id

    if(is.null(main)) main <- paste(gonr)

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
                       as.character(id1), ignore.case=TRUE))

        if(length(result)<1)
            stop("No genes found for this group.")

        details <- match.arg(details)
        if(details=="size")
            res <- length(result)
        else if(details=="names")
            res <- as.character(gn[result])
        else if(details=="id")
            res <- as.character(id1[result])
        else {
            gn1 <- paste(1:length(result),gn[result],sep=".")
            gdata <- mvalues[result,]
            rownames(gdata) <- gn1
            res <- gdata}

        if(plot)
        {
        type <- match.arg(ptype)
        if(ptype=="matplot") {
            if (data.type=="time")
                matplot(xval, t(mvalues[result,]),type="l", col=1:6,
                lty=rep(1:5,length.out=length(result)), 
                main=main, ylim=ylim, ylab=ylab, xlab=xlab, pch=1,
                ...)
            else {
                matplot(xval, t(mvalues[result,]),type="l", col=1:6,
                lty=rep(1:5,length.out=length(result)), xaxt="n",
                main=main, ylim=ylim, ylab=ylab, xlab=xlab, pch=1,
                ...)
                axis(1, xval, names(xval), cex.axis=cex.axis)}


            if(legend) {
                xpos <- ifelse(class(xval)=="character",1,xval[1])
                legend(xpos, ylim[2], as.character(gn[result]), ncol=5, 
                col=1:6, lty=rep(1:5,length.out=length(result)), cex=cexl)
            }
        }
        else {
            d <- mvalues[result,]
            d <- data.frame(gn[result], d)
            colnames(d) <- c("id","strain1","strain2","strain3")
            d <- reshape(d, direction="long", varying=2:4, sep="")
            d$time[d$time==1] <- "BL21"
            d$time[d$time==2] <- "RV308"
            d$time[d$time==3] <- "HMS174"
            colnames(d) <- c("id", "strain", "expression")
            #d <- data.frame(d)
            require(lattice)
            lattice.options(default.theme = canonical.theme(color = FALSE))
            print(barchart(id~expression|strain,data=d,layout=c(3,1),col="grey",main=main, origin=0, xlim=ylim))
        }
        }

        if(table)
        {
           out <- cbind(gn,id1,links,mvalues,stats)
           write.htmltable(out[result,], file=file, title=main)
           file <- paste(file,"html",sep=".")
           wd <- getwd()
           if(interactive()) 
              browseURL(paste(wd,file,sep="/"))
         }

    }
    if(plot==TRUE) print(res)
    invisible(res)
})

