pvalFromPermMat <- function(obs.av.dist, perms, quant)
{
  N <- nrow(perms)
  pvals <- matrix(as.double(NA), ncol=2, nrow=ncol(perms))
  dimnames(pvals) <- list(rownames(perms), c(paste(quant*100,"%quantile.perm",sep=""),"p.val.lower"))

  tempObs <- rep(obs.av.dist, nrow(perms))
  tempObs <- matrix(tempObs,byrow=TRUE,ncol=length(obs.av.dist))
  pvals[,2] <- colSums(perms <= tempObs)/N
  #pvals[,3] <- colMeans(perms)
  pvals[,1] <- apply(perms, 2, function(x) quantile(x, quant))
  pvals

}

comp_perm <- function(data, cll, N=500, ...)
{
  nclus <- length(unique(cll[!is.na(cll)]))
  mat <- matrix(ncol=nclus, nrow=N)
  for (i in 1:N)
  {
    cll2 <- sample(cll)
    mat[i,] <- comp_dist(data, cll2, ...)
  }
  mat
}

comp_dist <- function(data, cll, dist1 = distEuclidean, cent = colMeans, method=c("average","median","maximum"), std = TRUE)
{
  if (nrow(data) != length(cll))
    stop("Data size and length of cluster vector do not match.")

  ## remove NA's
  data <- data[!is.na(cll),]
  cll <- cll[!is.na(cll)]

  nclus <- length(unique(cll))

  method <- match.arg(method)

  av_dist <- rep(0, nclus)
  for (i in 1:nclus)
  {
    if (method=="median")
    {
      c1 <- cent(data[cll==i,], na.rm=TRUE)
      dim(c1) <- c(1,ncol(data))
      av_dist[i] <- quantile(dist1(data[cll == i,], c1), 0.5, na.rm = TRUE)
    }
    else if (method=="maximum")
    {
      c1 <- cent(data[cll==i,], na.rm=TRUE)
      dim(c1) <- c(1,ncol(data))
      av_dist[i] <- quantile(dist1(data[cll == i,], c1), 1, na.rm = TRUE)
    }
    else if (method == "average")
    {
      c1 <- cent(data[cll==i,], na.rm=TRUE)
      dim(c1) <- c(1,ncol(data))
      av_dist[i] <- sum(dist1(data[cll == i,], c1), na.rm = TRUE) /length(cll[cll == i])
    }
  }

  if (std==TRUE)
  {
  ## standardize by av. dist of all genes to overall center
    c <- cent(data, na.rm=TRUE)
    dim(c) <- c(1,ncol(data))
    avd <- sum(dist1(data, c), na.rm = TRUE) /nrow(data)
    av_dist/avd
  }

  else av_dist
}


comp_test <- function(data, cll, N=500, quant=0.05, ...)
{
  size <- as.vector(table(cll))
  obs.av.dist = comp_dist(data, cll, ...)
  perms <- comp_perm(data, cll, N)
  pvals <- pvalFromPermMat(obs.av.dist, perms, quant=quant)
  cbind(size,obs.av.dist,pvals)
}

