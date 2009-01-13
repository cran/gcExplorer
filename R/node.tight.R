#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: node.tight.R 4249 2009-01-13 14:06:28Z scharl $
#

setGeneric("node.tight", function(object, ...)
    standardGeneric("node.tight"))

setMethod("node.tight", signature(object="kccasimple"),
function(object, theme, colscale)
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
    for (i in 1:length(node.col))
    {
        if ( (node.col[i] > max_val) | (node.col[i] < min_val) )
        {
           warning("Node 'k",i,"' out of scale definded by 'colscale'.",
                   call. = FALSE)
        }
        node.col[i] <- calcHCL(theme, as.numeric(node.col[i]),
                                c(min_val,max_val))
    }
    l <- list(node.col=node.col,val=av.dist)
    l
})


setGeneric("node.size", function(object, ...)
    standardGeneric("node.size"))

setMethod("node.size", signature(object="kccasimple"),
function(object, theme, colscale)
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
    for (i in 1:length(node.col))
    {
        if ( (node.col[i] > max_val) | (node.col[i] < min_val) )
        {
           warning("Node 'k",i,"' out of scale definded by 'colscale'.",
                   call. = FALSE)
        }
        node.col[i] <- calcHCL(theme, as.numeric(node.col[i]),
                                c(min_val,max_val))
    }
    l <- list(node.col=node.col, val=node.val)
    l
})

setGeneric("node.go", function(object, ...)
    standardGeneric("node.go"))

setMethod("node.go", signature(object="kccasimple"),
function(object, theme, colscale, gonr, source.group, source.id, id) {

    if(length(id) != nrow(object@data@get("designMatrix")))
        stop("Vector of identifiers does not fit to cluster object.")

    if(length(source.group) != length(source.id))
        stop("Lengths of source identifiers and source groups differ.")

    go <- source.group
    bnumber <- source.id

    v <- grep(as.character(gonr),as.character(go),ignore.case=TRUE)

    if(length(v)<1)
        stop("Group not in source groups.")

    else
    {
        genes <- unique(bnumber[v])
        result  <- NULL
        for(i in 1:length(genes))
        result <- c(result, grep(as.character(genes)[i],
                    as.character(id), ignore.case=TRUE)[1])

        if(length(result)<1) 
            stop("No genes found for this group.")

        else 
        {
        ff <- result
        c1 <- object@cluster[ff]
        c1 <- c1[complete.cases(c1)]
        t1 <- NULL

        for(i in 1:object@k) 
        {
            if(length(c1[c1==i])==0) t1 <- c(t1,0)
            else t1 <- c(t1, length(c1[c1==i])/table(object@cluster)[i])
        }
         node.col <- t1

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
        for (i in 1:length(node.col))
        {
            if ( (node.col[i] > max_val) | (node.col[i] < min_val) )
        {
            warning("Node 'k",i,"' out of scale definded by 'colscale'.",
                   call. = FALSE)
        }
        node.col[i] <- calcHCL(theme, as.numeric(node.col[i]),
                                c(min_val,max_val))
        }
        l <- list(node.col=node.col,val=t1)
        l

        }

     }
})

setGeneric("node.group", function(object, ...)
    standardGeneric("node.group"))

setMethod("node.group", signature(object="kccasimple"),
function(object, theme, colscale, group) {

        tab <- as.vector(table(clusters(object)))

        t1 <- NULL
        for(i in 1:object@k) 
        {
            if(length(group[group==i])==0) t1 <- c(t1,0)
            else t1 <- c(t1, length(group[group==i])/tab[i])
        }
        node.col <- t1

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
        for (i in 1:length(node.col))
        {
            if ( (node.col[i] > max_val) | (node.col[i] < min_val) )
        {
            warning("Node 'k",i,"' out of scale definded by 'colscale'.",
                   call. = FALSE)
        }
        node.col[i] <- calcHCL(theme, as.numeric(node.col[i]),
                                c(min_val,max_val))
        }
        l <- list(node.col=node.col,val=t1)
        l
})
