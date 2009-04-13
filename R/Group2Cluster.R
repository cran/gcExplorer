Group2Cluster <- function(object, gonr, source.group, source.id, id) {

    if (length(id) != length(clusters(object)))
        stop("Vector of Blattner numbers does not fit to cluster Object")

    go <- source.group
    bnumber <- source.id

    #all genes with function gonr in annotation file
    v <- grep(as.character(gonr),as.character(go),ignore.case=TRUE)

    if(length(v)<1)
        stop("No genes found for this group.")

    else
    {
        #bnumber of gonr genes in annotation file
        genes <- unique(bnumber[v])

        #positions of gonr genes in object
        result  <- NULL
        for(i in 1:length(genes))
        result <- c(result, grep(as.character(genes)[i],
                    as.character(id), ignore.case=TRUE)[1])
        result = result[complete.cases(result)]

        if(length(result)<1) 
            stop("No genes found for this group.")

        else 
        {
           ff <- result

           #cluster membership of annotated genes
           c1 <- object@cluster[ff]
         }
    }
    c1 <- c1[complete.cases(c1)]
    c1
}

Random2Cluster <- function(object, perc)
{
   if (perc > 1)
       stop("The percentage must be a value between 0 and 1")

   l <- length(object@cluster)
   f1 <- sample(1:l,l*perc)
   c1 <- object@cluster[f1]
   c1 <- c1[complete.cases(c1)]
   c1
}

DefinedCluster <- function(object,filt=0,numEdges=6,perc=1,noise=0)
{
   if (perc > 1)
       stop("The percentage must be a value between 0 and 1")

   if (noise > 1)
       stop("The noise percentage must be a value between 0 and 1")


    d <- clusterSim(object)
    d <- (d+t(d))/2
    for(i in 1:nrow(d)) {
       for(j in 1:ncol(d)) {
          if(i>=j) d[i,j] <- 0
       }
    }
    d[d<filt] <- 0
    
    l <- list()
    k <- 0
    for(i in 1:nrow(d)) {
       for(j in 1:ncol(d)) {
          if(d[i,j] != 0) {
             k <- k+1
             l[[k]] <- c(i,j)
          }
       }
    }
    num.edges <- length(l)

    if (numEdges > num.edges)
        stop(paste("Number must be an integer between 1 and ",num.edges))

    
    m <- sample(1:num.edges, numEdges)
    ll <- unique(unlist(l[m]))
    
    clus <- object@cluster[complete.cases(object@cluster)]
    
    c1 = NULL
    for(i in seq(along=ll)) c1 = c(c1, rep(ll[i], (length(clus[clus==ll[i]])*perc)))

    if (noise > 0) {
    "%w/o%" <- function(x,y) x[!x %in% y]
    clus2 <- clus %w/o% ll
    l <- length(clus2)
    f1 <- sample(1:l,l*noise)
    c2 <- clus2[f1]
    c2 <- c2[complete.cases(c2)]

    c(c1,c2)
    }

    else c1
}
