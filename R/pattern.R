#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: pattern.R 4249 2009-01-13 14:06:28Z scharl $
#

pattern <- function(time=8,v=5) {
   s <- seq(0,1, length=time)
   x <- list()
   x[[1]] <- c(0,0.3,1,0.9,1.6,2.8,1.9,1.6,0.9,1,0.3,0,-1.5,-1,rep(-2,40))[1:time]
   x[[2]] <- -x[[1]]
   x[[3]] <- c(0,v,v,rep(0,40))[1:time]
   x[[4]] <- -x[[3]]
   x[[5]] <- c(v,v,v,v,v,v,v/2,v/3,v/4,v/5,rep(0,40))[1:time]
   x[[6]] <- -x[[5]]
   x[[7]] <- rep(c(v,v,v/2,v/3,-v/3,-v/2,-v,-v, -v/2),5)[1:time]
   x[[8]] <- -x[[7]]
   x[[9]] <- s*v
   x[[10]] <- -x[[9]]
   m <- t(matrix(unlist(x),time,10))
   m
 }
