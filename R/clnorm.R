#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: clnorm.R 4249 2009-01-13 14:06:28Z scharl $
#

cl.norm <- function(time=10, sd=0.1, sd.ri=0, size=50, n=10) {

   if(length(size)==1)
      size <- rep(size,n)

   if(length(size)!=n)
      stop("Length of size must be equal to the number of clusters.")

   l <- list()
   for(c in 1:n) {
      l[[c]] <- matrix(0, nrow=time, ncol=size[c])
      x1 <- rnorm(time,0,1)
      for(i in 1:size[c]) l[[c]][,i] <- x1 + rnorm(time,1,sd) + rnorm(1,0,sd.ri)
   }
   l <- unlist(l)
   l <- matrix(l,ncol=time,byrow=TRUE)
   l
}

cl.arima <- function(time=10, sd=0.1, sd.ri=0, size=50, n=10, ar=NULL, o=NULL) {

   if(is.null(ar)) ar = runif(1, -1,1)

   if(is.null(o)) o = sample(1:2,1)

   if(!is.element(o,c(1,2)))
      stop("order must be either 1 or 2")

   if(length(size)==1)
      size <- rep(size,n)

   if(length(size)!=n)
      stop("Length of size must be equal to the number of clusters.")

   nn <- n

   l <- list()
   for(c in 1:nn) {
     l[[c]] <- matrix(0, nrow=time, ncol=size[c])
     o <- o
     x1 <- arima.sim(n = time,
                     list(ar = ar,
                          order = c(1, o, 0)))
     if(o) x1 <- x1[-seq_len(o)]
     if(sample(0:1, 1)) x1 <- rev(x1)
     x1 <- 4*(pnorm(x1, sd=4)-.5) 
     for(i in 1:size[c]) l[[c]][,i] <- x1 + rnorm(1, 0, sd.ri) + rnorm(time,0,sd)
   }
   l <- unlist(l)
   l <- matrix(l, ncol=time, byrow=TRUE)
   l
}

cl.pattern <- function(cent=cent, sd=0.1, sd.ri=0, size=50) {

   if(length(size)==1)
      size <- rep(size,nrow(cent))

   if(length(size)!=nrow(cent))
      stop("Length of size must be equal to the number of clusters.")

   l <- list()
   for(c in 1:nrow(cent)) {
      l[[c]] <- matrix(0, nrow=ncol(cent), ncol=size[c])
      x1 <- cent[c,]
      for(i in 1:size[c]) l[[c]][,i] <- x1 + rnorm(ncol(cent),0,sd) + rnorm(1,0,sd.ri)
   }
   l <- unlist(l)
   l <- matrix(l, ncol=ncol(cent), byrow=TRUE)
   l
}

cl.outlier <- function(cent=cent, sd=0.1, sd.ri=0, size=50) {

   if(length(size)==1)
      size <- rep(size,nrow(cent))

   if(length(size)!=nrow(cent))
      stop("Length of size must be equal to the number of clusters.")

   l <- list()
   for(c in 1:nrow(cent)){
      l[[c]] <- matrix(0, nrow=ncol(cent), ncol=size[c])
      x1 <- cent[c,]
      for(i in 1:size[c]){
           l[[c]][,i] <- x1 + rnorm(ncol(cent),0,sd) + rnorm(1,0,sd.ri)
           r <- sample(0:ncol(cent),1)
           l[[c]][r,i] <- l[[c]][r,i] + rnorm(1,0,2)
       }
   }
   l <- unlist(l)
   l <- matrix(l, ncol=ncol(cent), byrow=TRUE)
   l
}

cl.noise <- function(time=10, sd=2, size=50) {

   mnoise <- rnorm(size, sd=sd)
   snoise <- runif(size, 0.1, 0.3)
   C <- matrix(rnorm(size*time, mean=mnoise, sd=snoise), ncol=time)
   C
}



gcSim <- function(sim=c("arima","norm","pattern","noise","outlier"), time=10, sd=0.1, sd.ri=0, size=50, n=10, ar=NULL, o=NULL, cent){

   sim <- match.arg(sim)

   if(sim=="arima") x <- cl.arima(time=time, sd=sd, sd.ri=sd.ri, size=size, n=n, ar=ar, o=o)
   if(sim=="norm") x <- cl.norm(time=time, sd=sd, sd.ri=sd.ri, size=size, n=n)
   if(sim=="pattern") x <- cl.pattern(cent=cent, sd=sd, sd.ri=sd.ri, size=size)
   if(sim=="noise") x <- cl.noise(time=time, sd=sd, size=size)
   if(sim=="outlier") x <- cl.outlier(cent=cent, sd=sd, sd.ri=sd.ri, size=size)
   x
}

gcData <- function(...) {
       z <- rbind(...)
       #class(z) <- "ListofClusters"
       z
}
