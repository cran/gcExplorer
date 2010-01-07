#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl, Ingo Voglhuber
#  $Id: gcModify.R 4498 2010-01-07 13:35:42Z scharl $
#

setGeneric("gcModify", function(graphdata, ...)
        standardGeneric("gcModify"))

setMethod("gcModify", signature(graphdata="graphdata"),
function (graphdata, clsim = NULL, rmNodes = NULL,
          kpNodes = NULL, edgeDep = TRUE, nodeDep = FALSE,
          zoom = c("none", "manual", "auto"),
          keepAspectRatio = TRUE, node.function = NULL,
          doViewPort = TRUE, bgdata = NULL) {

    require(symbols)
    require(Rgraphviz)

    ## check argument graph
    if (class(graphdata) != "graphdata")
       stop("'graphdata' has to be of class 'graphdata'")
    else graph <- graphdata@Ragraph
    ## remove or modify edges
    object <- graphdata@kcca
    if (!is.null(clsim))
    {
      if (dim(clsim)[1] != dim(clsim)[2])
         stop("Wrong dimension of clsim.")
      if (!is.null(object))
      {
#         if (class(object) != "kccasimple")
#            stop("'object' has to be of class 'kccasimple'")
          if (object@k != dim(clsim)[1])
             stop("Wrong dimension of clsim.")
      }
      if (length(graph@AgEdge) > 0)
      {
         colscale <- graphdata@colscale
         min_val <- as.numeric(colscale[1])
         max_val <- as.numeric(colscale[2])
         for (k in length(graph@AgEdge):1)
	 		{
 	  			i <- graph@AgEdge[[k]]@tail
 	  			j <- graph@AgEdge[[k]]@head
 	  			i <- as.numeric(gsub("k","",i))
 	  			j <- as.numeric(gsub("k","",j))
 	  			if (graphdata@edge.method == "mean")
 	  			  clsim_val <- mean(c(clsim[i,j],clsim[j,i]))
 	  			else if (graphdata@edge.method == "min")
 	  			  clsim_val <-  min(clsim[i,j],clsim[j,i])
 	  			else if (graphdata@edge.method == "max")
 	  			  clsim_val <- max(clsim[i,j],clsim[j,i])
 	  			else clsim_val <- clsim[i,j]
 	  			if (clsim_val == 0)
 	  			{
 	  			  graph@AgEdge <- graph@AgEdge[-k]
 	  			}
 	  			else
 	  			{
               newvalue <- clsim_val * 5
 	  			   names(newvalue) <- names(graph@AgEdge[[k]]@lwd)
 	  			   graph@AgEdge[[k]]@lwd <- newvalue
               theme <- graphdata@theme
 	  			   if (!is.null(theme))
 	  			   {
 	  			      graph@AgEdge[[k]]@color <- calcHCL(theme,
 	  			                                          as.numeric(clsim_val),
 	  			                                          c(min_val,max_val))
 	  			   }
 	  			}
         		}
	 	}
	 	else stop("Graph has no edges to remove.")
	 }
	 ## check argument rmNodes
	 if ( !is.null(rmNodes) & !is.null(kpNodes) )
	 {
	 	stop("Only use one of arguments: rmNodes, kpNodes.")
	 }
	 ## remove all nodes listet in rmNodes
	 if (!is.null(rmNodes))
	 {
	   ## check argument rmNodes
	   if (class(rmNodes) != "character")
	      stop("'rmNodes' has to be of class 'character'")
	 	for (k in length(graph@AgNode):1)
	 	{
	 		pos <- match(graph@AgNode[[k]]@name, rmNodes)
	 		if (!is.na(pos))
	 		{
	 			if (length(graph@AgNode) == 1)
	 			{
	 				stop("No nodes left to draw!")
	 			}
	 			else
	 			{
	 				graph@AgNode <- graph@AgNode[-k]
	 				rmNodes <- rmNodes[-pos]
	 			}
	 		}
	 	}
	 	if (length(rmNodes) != 0)
	 	{
	 		warning("unknown node(s) in rmNodes: ", rmNodes)
	 	}
	 }
	 ## keep all nodes listet in kpNodes
	 if (!is.null(kpNodes))
	 {
	   ## check argument kpNodes
	   if (class(kpNodes) != "character")
	      stop("'kpNodes' has to be of class 'character'")
	 	for (k in length(graph@AgNode):1)
	 	{
	 	   pos <- match(graph@AgNode[[k]]@name, kpNodes)
	 		if (is.na(pos))
	 		{
	 			if (length(graph@AgNode) < 1)
	 			{
	 				stop("No nodes left to draw!")
	 			}
	 			else
	 			{
	 				graph@AgNode <- graph@AgNode[-k]
	 			}
	 		}
	 		else
	 		{
	 			kpNodes <- kpNodes[-pos]
	 		}
	 	}
	 	if (length(kpNodes) != 0)
	 	{
	 		warning("unknown node(s) in kpNodes: ")
	 	}
	 }
	 ## remove edges depending on removed
	 if (edgeDep)
	 {
	 	NodeNames <- extrNodeNames(graph)
	 	for (k in length(graph@AgEdge):1)
	 	{
	 		HeadOfEgde <- graph@AgEdge[[k]]@head
	 		TailOfEdge <- graph@AgEdge[[k]]@tail
	 		if (!all(c(HeadOfEgde, TailOfEdge) %in% NodeNames))
	 		{
	 			graph@AgEdge <- graph@AgEdge[-k]
	 		}
	 	}
	 }
	 ## remove nodes depending on removed edges
	 if (nodeDep)
	 {
	 	EdgeHeadTails <- getEdgeHeadTails(graph)
	 	for (k in length(graph@AgNode):1)
	 	{
	 		if (!any(graph@AgNode[[k]]@name %in% EdgeHeadTails))
	 		{
	 			graph@AgNode <- graph@AgNode[-k]
	 		}
	 	}
	 }
	 ## find and set new boundBox values for auto-zoom
	 zoom <- match.arg(zoom)
	 if (zoom == "auto")
	 {
	 	max_x <- graph@boundBox@botLeft@x
	 	max_y <- graph@boundBox@botLeft@y
	 	min_x <- graph@boundBox@upRight@x
	 	min_y <- graph@boundBox@upRight@y
	 	if (length(graph@AgEdge) >= 1)
	 	{
 			for (k in length(graph@AgEdge):1)
 			{
				for (l in 1:length(graph@AgEdge[[k]]@splines))
				{
					for (m in 1:4)
					{
						x <- graph@AgEdge[[k]]@splines[[l]]@cPoints[[m]]@x
						y <- graph@AgEdge[[k]]@splines[[l]]@cPoints[[m]]@y
						if (max_x < x) max_x <- x
 						if (max_y < y)	max_y <- y
 						if (min_x > x) min_x <- x
 						if (min_y > y)	min_y <- y
					}
				}
			}
		}
 		for (k in length(graph@AgNode):1)
 		{
 			x <- graph@AgNode[[k]]@center@x + graph@AgNode[[k]]@rWidth
 			y <- graph@AgNode[[k]]@center@y + round(graph@AgNode[[k]]@height/2)
 			if (max_x < x) max_x <- x
 			if (max_y < y)	max_y <- y
 			x <- graph@AgNode[[k]]@center@x - graph@AgNode[[k]]@rWidth
 			y <- graph@AgNode[[k]]@center@y - round(graph@AgNode[[k]]@height/2)
 			if (min_x > x) min_x <- x
 			if (min_y > y)	min_y <- y
		}
		if (keepAspectRatio)
		{
			dif_x <- max_x - min_x
			dif_y <- max_y - min_y
			if (dif_x > dif_y)
			{
				max_y <- max_y + round((dif_x - dif_y)/2)
				min_y <- min_y - round((dif_x - dif_y)/2)
			}
			else
			{
				max_x <- max_x + round((dif_y - dif_x)/2)
				min_x <- min_x - round((dif_y - dif_x)/2)
			}
		}
	 	graph@boundBox@botLeft@x <- min_x
	 	graph@boundBox@botLeft@y <- min_y
	 	graph@boundBox@upRight@x <- max_x
	 	graph@boundBox@upRight@y <- max_y
	 }
	 
	 if ((is.null(node.function)) & (!(is.null(body(graphdata@node.function)))))
	    node.function <- graphdata@node.function
	 if ((is.null(node.function)) | (!doViewPort)) p <- plot(graph)
	 else p <- plot(graph, drawNode = function(x,i,object){})
	 
	 ## set new boundBox values for manual-zoom
	 if (zoom == "manual")
	 {
	 	max_x <- graph@boundBox@botLeft@x
	 	max_y <- graph@boundBox@botLeft@y
	 	min_x <- graph@boundBox@upRight@x
	 	min_y <- graph@boundBox@upRight@y
	 	print("define bottom left corner by clicking on active graphic device")
	 	location <- locator(1)
	 	min_x <- location$x
	 	min_y <- location$y
	 	print("define upper right corner by clicking on active graphic device")
	 	location <- locator(1)
	 	max_x <- location$x
	 	max_y <- location$y	 	
		if (keepAspectRatio)
		{
			dif_x <- max_x - min_x
			dif_y <- max_y - min_y
			if (dif_x > dif_y)
			{
				max_y <- max_y + round(dif_x/2)
				min_y <- min_x - round(dif_y/2)
			}
			else
			{
				max_x <- max_x + round(dif_y/2)
				min_x <- min_x - round(dif_y/2)
			}
		}
	 	graph@boundBox@botLeft@x <- min_x
	 	graph@boundBox@botLeft@y <- min_y
	 	graph@boundBox@upRight@x <- max_x
	 	graph@boundBox@upRight@y <- max_y
	 	if (is.null(node.function)) p <- plot(graph)
	 	else p <- plot(graph, drawNode = function(object,i,bgdata){})
 	 }
 	 
 	 if (is.null(bgdata))
 	 {
 	    if (!is.null(names(graphdata@bgdata))) bgdata <- graphdata@bgdata
 	 }
 	 ## draw grid-based nodes
 	 if ((!is.null(node.function)) & (doViewPort))
 	 {
 	 	gridnodes(node.function = node.function, graph = p,
 	 				 object = object, bgdata = bgdata)
 	 }
        
    ## LEGEND TODO


    ## return object of class graphdata 
    foo <- graphdata
    foo@Ragraph <- p
    if (!is.null(bgdata)) foo@bgdata <- bgdata
    if ( (doViewPort) & (!(is.null(node.function))) )
    {
       foo@node.function <- node.function
    }
    invisible(foo)
})

extrNodePos <- function(graph) {
   if (length(graph@AgNode) > 0)
   {
      for (k in 1:length(graph@AgNode))
      {
         x <- graph@AgNode[[k]]@center@x
         y <- graph@AgNode[[k]]@center@y
         nodepos <- cbind(nodepos, c(x,y))
      }
      return(nodepos)
   }
   else warning("no elements in list 'graph@AgNode'.")
}

extrEdgePos <- function(graph) {
 	if (length(graph@AgEdge) > 0)
 	{
		for (k in length(graph@AgEdge):1)
		{
			for (l in 1:length(graph@AgEdge[[k]]@splines))
			{
				for (m in 1:4)
				{
					x <- graph@AgEdge[[k]]@splines[[l]]@cPoints[[m]]@x
					y <- graph@AgEdge[[k]]@splines[[l]]@cPoints[[m]]@y
					edgepos <- cbind(edgepos, c(x,y))
				}
			}
		}
		return(edgepos)
	}
	else warning("no elements in list 'graph@AgEdge'.")
}

extrNodeNames <- function (graph) {
   NodeVector <- c()
   for (k in 1:length(graph@AgNode))
   {
      NodeVector <- cbind(NodeVector, graph@AgNode[[k]]@name)
   }
   NodeVector
}

getEdgeHeadTails <- function (graph) {
   EdgeHeadTails <- c()
   for (k in 1:length(graph@AgEdge))
   {
      EdgeHeadTails <- cbind(EdgeHeadTails, 
                             cbind(c(graph@AgEdge[[k]]@head),
                             c(graph@AgEdge[[k]]@tail)))
   }
   EdgeHeadTails
}