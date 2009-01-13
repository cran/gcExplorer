#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: legend.size.R 4249 2009-01-13 14:06:28Z scharl $
#

setGeneric("legend.size", function(object, ...)
    standardGeneric("legend.size"))

setMethod("legend.size", signature(object="kccasimple"),
function(object, theme, colscale=NULL, pos="bottomleft")
{
    node.col <- table(object@cluster)/length(object@cluster)
    node.val <- node.col
    if (is.null(colscale))
    {
       min_val <- as.numeric(min(node.col))
       max_val <- as.numeric(max(node.col))
    }
    else
    {
       min_val <- as.numeric(colscale[1])
       max_val <- as.numeric(colscale[2])
    }

    quant <- c(0,0.25,0.5,0.75,1)
    perc <- rep(0,5)
    quantcol <- rep(0,5)
    for (i in 1:5)
    {
        perc[i] <- as.numeric(quantile(node.val,quant[i]))
        quantcol[i] <- calcHCL(theme, perc[i], c(min_val,max_val))
    }

    perc <- round(perc,2)
    legend(pos, legend = as.character(perc), fill= quantcol, inset=0.05)
})

setGeneric("legend.tight", function(object, ...)
    standardGeneric("legend.tight"))

setMethod("legend.tight", signature(object="kccasimple"),
function(object, theme, colscale=NULL, pos="bottomleft")
{
    data <- object@data@get("designMatrix")

    dist.to.center <- rep(0,nrow(data))
    for(i in 1:length(dist.to.center)) 
        dist.to.center[i] <- object@family@dist(data[i,, drop = F],
                             object@centers[object@cluster[i],,drop = F])

    av.dist <- rep(0,nrow(object@centers))
    for(i in 1:length(av.dist)) 
        av.dist <- as.vector(tapply(dist.to.center, object@cluster, sum))/
                   table(object@cluster)
    node.col <- av.dist

    if (is.null(colscale))
    {
       min_val <- as.numeric(min(node.col))
       max_val <- as.numeric(max(node.col))
    }
    else
    {
       min_val <- as.numeric(colscale[1])
       max_val <- as.numeric(colscale[2])
    }

    quant <- c(0,0.25,0.5,0.75,1)
    perc <- rep(0,5)
    quantcol <- rep(0,5)
    for (i in 1:5)
    {
        perc[i] <- as.numeric(quantile(av.dist,quant[i]))
        quantcol[i] <- calcHCL(theme, perc[i], c(min_val,max_val))
    }
    perc <- round(perc,2)
    legend(pos, legend = as.character(perc), fill= quantcol, inset=0.05)
})
