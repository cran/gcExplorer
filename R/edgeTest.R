edgeTest <- function(object, min.size=1, group, N = 500, filt = 0.1, useNH = TRUE)
{
    clus <- clusters(object)
    cclus <- clus[!is.na(clus)]
    sclus <- as.vector(table(clus))
    grouptab <- NULL

    for(i in 1:object@k) 
    {
       if(length(group[group==i][complete.cases(group[group==i])])==0) grouptab[i] <- 0
       else grouptab[i] <- length(group[group==i][complete.cases(group[group==i])])
    }

    p <- prop.test(grouptab,sclus)

    if(p$p.value > 0.05)
    {
#        MESSAGE <- "Proportions are the same and independent of cluster structure."
#        print(list(MESSAGE = MESSAGE, p=p))
         p
    }

    else {

    d <- clusterSim(object)
    nc <- ncol(d)
    d <- (d+t(d))/2
    for(i in 1:nrow(d)) {
       for(j in 1:nc) {
          if(i>=j) d[i,j] <- 0
       }
    }
    d[d<filt] <- 0

    r <- comp_diff(nc, group=group, sclus = sclus, d = d, useNH = useNH, min.size = min.size, grouptab = grouptab)

    if (length(r) == 0)
        stop("No clusters with more than ", min.size, " genes of this group.")

    perms <- rep(0,nrow=N)
    for (i in 1:N)
    {
        newgroup <- sample(cclus, length(group), replace=FALSE)
        perms[i] <- max(comp_diff(nc, group = newgroup, sclus = sclus, d = d, useNH = useNH, min.size = min.size, grouptab = grouptab))
    }

    #perms
    res <- matrix(ncol=4, nrow=length(r))
    rownames(res) <- names(r)
    colnames(res) <- c("Clsize1", "Clsize2", "Diff.in.Prop.", "P-value")
    res[,3] <- r

    x1 <- strsplit(names(r),"~") 
    x2 <- matrix(ncol=2,nrow=length(x1))
    for(i in 1:length(x1)) x2[i,] <- as.numeric(x1[[i]])
    x3 <- matrix(sapply(x2, function(x) as.vector(table(clusters(object)))[x]), ncol=2)

    res[,1] <- x3[,1]
    res[,2] <- x3[,2]

    res[,4] <- sapply(r,function(x) sum(perms >= x)/N)

    res

    }

}

comp_diff <- function(nc, group, sclus, useNH=FALSE, d, min.size=0, grouptab)
{
    t1 <- rep(0,nc)

    # absolute amount of genes in a certain cluster belonging to 
    # a certain group

    for(i in 1:nc) 
    {
       if(length(group[group==i][complete.cases(group[group==i])])==0) t1[i] <- 0
       else t1[i] <- length(group[group==i][complete.cases(group[group==i])])
    }

    if ((length(grouptab) != length(t1)) | (sum(grouptab) != sum(t1)))
        stop(paste("Table of initial group membership has sum",sum(grouptab),"and new group was sum",sum(t1),sep=" "))

    if(useNH == FALSE)
    {

        R2 <- matrix(0, ncol=nc, nrow=nc)
        for (i in 1:nc) {
            for (j in 1:nc) {
                if (i<j) {
                    if ((grouptab[i] < min.size) | (grouptab[j] < min.size)) 
                        R2[i,j] <- NA
                    else
                        R2[i,j] <- abs((t1/sclus)[i]-(t1/sclus)[j])
                }
                else R2[i,j] <- NA
            }
        }
    }

    else
    {
        R2 <- matrix(0, ncol=nc, nrow=nc)
        for (i in 1:nc) {
            for (j in 1:nc) {
                if (d[i,j] != 0) {
                    if ((grouptab[i] < min.size) | (grouptab[j] < min.size)) 
                        R2[i,j] <- NA
                    else
                        R2[i,j] <- abs((t1/sclus)[i]-(t1/sclus)[j])
                }
                else R2[i,j] <- NA
            }
        }
    }

    R2
    RR <- c(R2)
    namesRR <- NULL
    for(i in 1: nc) namesRR <- c(namesRR, paste(1:nc, i, sep="~"))
    names(RR) <- namesRR
    RR[!is.na(RR)]

}

newclsim <- function(eT, object, filt=0.1, p.filt=0.05, edge=c("clsim", "pval", "zp5"))
{
    x1 <- strsplit(rownames(eT),"~") 
    x2 <- matrix(ncol=2,nrow=length(x1))
    for(i in 1:length(x1)) x2[i,] <- as.numeric(x1[[i]])
    #x2
    #clsim <- matrix(0,ncol=ncol(clsim.old), nrow=nrow(clsim.old))

    d <- clusterSim(object)
#     nc <- ncol(d)
#     d <- (d+t(d))/2
#     for(i in 1:nrow(d)) {
#        for(j in 1:nc) {
#            if(i>=j) d[i,j] <- 0
#        }
#     }
    d[d<filt] <- 0

    clsim2 <- d

    edge <- match.arg(edge)

    if (edge == "clsim")

    for (i in 1:nrow(x2))
    {
        if (eT[i,4] <= p.filt) {
            clsim2[x2[i,1],x2[i,2]] <- 0
            clsim2[x2[i,2],x2[i,1]] <- 0
        }
    }

    if (edge == "pval")
    for (i in 1:nrow(x2))
    {
        clsim2[x2[i,1],x2[i,2]] <- eT[i,4]
        clsim2[x2[i,2],x2[i,1]] <- eT[i,4]
    }

    if (edge == "zp5")
    for (i in 1:nrow(x2))
    {
        if (eT[i,4] <= p.filt) {
            clsim2[x2[i,1],x2[i,2]] <- 0
            clsim2[x2[i,2],x2[i,1]] <- 0
        }
        clsim2[clsim2 != 0] <- 0.5
    }
    clsim2
}
