#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: gcExplorer.R 4339 2009-05-06 15:23:04Z scharl $
#

setGeneric("gcExplorer", function(object, ...)
        standardGeneric("gcExplorer"))

setMethod("gcExplorer", signature(object="kccasimple"),
function(object, layout = c("dot", "neato", "twopi", "circo", "fdp"), 
         theme = "grey",
         edge.method = c("orig", "mean", "min", "max"), 
         node.function = NULL, node.args = NULL, doViewPort = FALSE,
         filt = 0.1,  interactive = !is.null(panel.function), dev=c("one","many"),
         panel.function = NULL, panel.args = NULL, bgdata = NULL,
         colscale = NULL, mfrow = c(1,1), legend.pos = "none") 
{

    require(Rgraphviz)

    #data1 <- flexclust:::getData(object, error=TRUE)
    d <- clusterSim(object)

    ## define nodes 
    mynodes <- paste("k",1:ncol(d),sep="")


    ## how should the edges be drawn

    edge.method <- match.arg(edge.method)

    if(edge.method=="mean") {    
        d <- (d+t(d))/2
        for(i in 1:nrow(d)) {
            for(j in 1:ncol(d))
                if(i>j) d[i,j] <- 0}
    }

    if(edge.method=="min") {    
        for(i in 1:nrow(d)) {
            for(j in 1:ncol(d))
                d[i,j] <- min(d[i,j],d[j,i])}
        for(i in 1:nrow(d)) {
            for(j in 1:ncol(d))
                if(i>j) d[i,j] <- 0}
    }

    if(edge.method=="max") {    
        for(i in 1:nrow(d)) {
            for(j in 1:ncol(d))
                d[i,j] <- max(d[i,j],d[j,i])}
        for(i in 1:nrow(d)) {
            for(j in 1:ncol(d))
                if(i>j) d[i,j] <- 0}
    }

    ## filter small similarities
    d[d<filt] <- 0

    ## define edges
    myedges <- list()
    for(i in 1:length(mynodes)) {
        myedges[[mynodes[i]]] <- list(edges = mynodes[intersect(which(d[i,] != 1), which(d[i,] != 0))],
			              weights=d[i,intersect(which(d[i,] != 1), which(d[i,] != 0))])
    }

    ## create a graph object
    g <- new("graphNEL",nodes=mynodes, edgeL = myedges, edgemode="directed")

    ## names and weights for drawing edges in different color and width
    names.E <- rep("NA",numEdges(g))
    weights.E <- rep(0,numEdges(g))
    k <- 0
    for(i in 1:numNodes(g)) {
                len <- length(myedges[[i]]$edges)
                    if(len == 0) {
                        k=k}
                    else {
                        for(j in 1:len) {
                            k <- k+1
                            weights.E[k] <- round(myedges[[i]]$weights[j],1)
                            names.E[k] <- paste(names(myedges)[i], myedges[[i]]$edges[j], sep="~")
                        }
                    }
    }

    ## color theme

    new.col <- weights.E
    if (is.null(colscale))
    {
       min_val <- as.numeric(min(new.col))
       max_val <- as.numeric(max(new.col))
    }
    else
    {
       min_val <- as.numeric(colscale[1])
       max_val <- as.numeric(colscale[2])
    }
    for (i in 1:length(new.col))
    {
       new.col[i] <- calcHCL(theme, as.numeric(new.col[i]), c(min_val,max_val))
    }
    fillcolor <- calcHCL(theme, 20, c(1,100))

    names(new.col) <- names.E
    new.lwd <- weights.E *5
    names(new.lwd) <- names.E

    ## additional attributes
    edgeA <- list(color = "black", lwd=1)

    nodeA <- list(fillcolor = fillcolor, fontsize = "15", height = "0.7", width = "0.7",shape="ellipse")

    attrs <- getDefaultAttrs(list(node=nodeA,edge=edgeA))
    layout <- match.arg(layout)
    defAttrs <- getDefaultAttrs()


    ## color nodes    
    if(is.null(node.function) || doViewPort ) {
        f1 <- rep(fillcolor, numNodes(g))
    }
    else
    {
        f2 <- do.call(node.function, c(list(object, theme, colscale), node.args))
        f1 <- f2$node.col
    }

    names(f1) <- nodes(g)

    ## plot the graph

    if (doViewPort)
	 {
	   require(symbols)
	 	if(edge.method=="orig") {
        	p <- plot(g, y = layout, edgeAttrs = list(color=new.col, lwd = new.lwd), 
                   nodeAttrs = list(fillcolor = f1), 
                   attrs = attrs, recipEdges = "distinct",
                   drawNode = function(x, i, object){})
    	}
    	else {
        	ug <-ugraph(g)
        	p <- plot(ug, y=layout, edgeAttrs = list(color = new.col, lwd = new.lwd),
                   nodeAttrs = list(fillcolor = f1), attrs = attrs,
                   drawNode = function(x, i, object){})
    	}
    	gridnodes(node.function = node.function, graph = p, object = object, bgdata = bgdata)
    }
    else
    {
    	if(edge.method=="orig") {
        	p <- plot(g, y = layout, edgeAttrs = list(color = new.col, lwd = new.lwd), 
                   nodeAttrs = list(fillcolor = f1), 
                   attrs = attrs, recipEdges = "distinct")
    	}
    	else {
        	ug <-ugraph(g)
        	p <- plot(ug, y = layout, edgeAttrs = list(color = new.col, lwd = new.lwd),
                   nodeAttrs = list(fillcolor = f1), attrs = attrs)
    	}    
    }

    if(length(unique(f1)) != 1)
    {
        if(legend.pos != "none") 
        {
        positions <- c("bottomright", "bottom", "bottomleft", "left", 
                    "topleft", "top", "topright", "right", "center")
        pos1 <- match.arg(legend.pos, positions)
        #else if (legend.pos == "manual") pos1 <- locator(1)

        min_val <- as.numeric(min(f2$val))
        max_val <- as.numeric(max(f2$val))
        quant <- c(0,0.75,1)
        perc <- rep(0,3)
        quantcol <- rep(0,3)
        for (i in 1:3)
        {
            perc[i] <- as.numeric(quantile(f2$val, quant[i]))
            quantcol[i] <- calcHCL(theme, perc[i], c(min_val, max_val))
        }
        perc <- round(perc, 2)
        perc <- sapply(perc, function(x) ifelse(x < 0.0001, "< 10e-4", as.character(x)))
        legend(pos1, legend = perc, fill = quantcol, inset = 0.02) 
        }

    }

    ## interactive plot
    if(interactive)
    {

        dev <- match.arg(dev)

        dev1 <- dev.cur()

        if(dev.cur() == dev.next()) {
            x11()
            par(mfrow=mfrow)
            dev2 <- dev.cur()}

        else dev2 <- dev.next()
        dev.set(dev2)
        par(mfrow = mfrow)

        dev.set(dev1)
        centers.x <- getNodeXY(p)$x
        centers.y <- getNodeXY(p)$y
#    x11()
        dev.set(dev1)

        while(length(z <- identify(x = centers.x, y = centers.y, n = 1, plot = FALSE)) > 0) {
            if(dev.cur() == dev.next()) x11()
	    if(dev == "many") x11()
            else dev.set(dev2)
            do.call(panel.function, c(list(object), z, panel.args))
            dev.set(dev1)
        }
        dev.set(dev2)
        op <- par(no.readonly = TRUE)
        par(op)
        dev.set(dev1)


    }

    ## return object of class graphdata 
    foo <- new("graphdata")
    foo@Ragraph <- p
    foo@kcca <- object
    if (!is.null(bgdata)) foo@bgdata <- bgdata
    if (doViewPort) foo@node.function <- node.function
    foo@edge.method <- edge.method
    foo@theme <- theme
    if (is.null(colscale)) colscale <- c(min_val, max_val)
    foo@colscale <- colscale
    invisible(foo)

}
)

