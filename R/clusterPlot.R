#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: clusterPlot.R 4249 2009-01-13 14:06:28Z scharl $
#

setGeneric("clusterPlot", function(object, ...)
        standardGeneric("clusterPlot"))

setMethod("clusterPlot", signature(object="kccasimple"),
function(object,method= c("size", "tight"), layout=c(3,4))
{
    require(flexclust)
    require(lattice)

    palette(flxColors())

    ## plot only a subset of clusters given by layout, 

    data <- object@data@get("designMatrix")

    k <- prod(layout)
    method <- match.arg(method)

    if(method=="size") {

    ## start with the smallest clusters
    o <- order(table(object@cluster))
    }

    else {

    ## start with tight clusters

    dist.to.center <- rep(0,nrow(data))
    for(i in 1:length(dist.to.center)) 
        dist.to.center[i] <- object@family@dist(data[i,, drop = F],
                             object@centers[object@cluster[i],,drop = F])

    av.dist <- rep(0,nrow(object@centers))
    for(i in 1:length(av.dist)) 
        av.dist <- as.vector(tapply(dist.to.center, object@cluster, sum))/
                   table(object@cluster)

    o <- order(av.dist)
    }

    oo <- o[1:k]
    ok <- object@cluster %in% oo
    data1 <- data[ok,]
    cl <- object@cluster[ok]
    for(i in 1:9) cl[cl==i] <- paste("0",i,sep="")
    cll <- paste("cluster",cl,sep=" ")

    ## tranform the data for lattice plots
    xaxis <- 1:ncol(data1)
    d <- as.data.frame(data1)
    r <- reshape(d, varying = list(Expr = colnames(d)),
                 times=xaxis, direction = "long")
    colnames(r)[2] <- "E"

    xyplot(E ~ time | cll, groups = id, data = r, type = "l", layout=layout, col=cl)

}

)