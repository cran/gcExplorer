#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: jkdist.R 4249 2009-01-13 14:06:28Z scharl $
#

distJackCor <- function(x, centers)
{
   m = array(dim=c(ncol(centers),nrow(x),nrow(centers)))
   for(i in 1:ncol(centers)){
     m[i,,] = distCor(x[,-i,drop=FALSE],centers[,-i,drop=FALSE])
   }
   apply(m,2:3,min)
}   

distJackEuc <- function(x, centers)
{
   m = array(dim=c(ncol(centers),nrow(x),nrow(centers)))
   for(i in 1:ncol(centers)){
     m[i,,] = distEuclidean(x[,-i,drop=FALSE],centers[,-i,drop=FALSE])
   }
   apply(m,2:3,min)
}   

distJackMan <- function(x, centers)
{
   m = array(dim=c(ncol(centers),nrow(x),nrow(centers)))
   for(i in 1:ncol(centers)){
     m[i,,] = distManhattan(x[,-i,drop=FALSE],centers[,-i,drop=FALSE])
   }
   apply(m,2:3,min)
}   

distJackMax <- function(x, centers)
{
   m = array(dim=c(ncol(centers),nrow(x),nrow(centers)))
   for(i in 1:ncol(centers)){
     m[i,,] = distMax(x[,-i,drop=FALSE],centers[,-i,drop=FALSE])
   }
   apply(m,2:3,min)
}   

centSpline <- function(d)
{
     require(splines)
     d = as.data.frame(d)
     r = reshape(d,varying=list(Expr=colnames(d)), direction="long" )
     x = r[,1]
     y = r[,2]
     lm1 <- lm(y~ns(x, df=5))
     predict(lm1, data.frame(x=seq(1,ncol(d),length=ncol(d))))
}
