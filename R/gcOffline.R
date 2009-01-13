#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl
#  $Id: gcOffline.R 4249 2009-01-13 14:06:28Z scharl $
#

setGeneric("gcOffline", function(object, ...)
        standardGeneric("gcOffline"))

setMethod("gcOffline", signature(object="kccasimple"),
function(object, panel.function, panel.args=NULL, type=pdf, file="gcOffline", which=NULL, html=FALSE, ...)
{
   type(paste(file,"-graph.",deparse(substitute(type)),sep=""))
   gcExplorer(object, ...)

   dev.off()

   if (is.null(which))
      which <- 1:object@k

   if(html==FALSE)
   {

      for (i in seq(along=which)) {
         k <- which[i]
         if (k %in% 1:9) number <- paste("00",k,sep="")
         else if (k %in% 10:99) number <- paste("0",k,sep="")
         else number <- k
         type(paste(file,"-",number,".",deparse(substitute(type)),sep=""))
         do.call(panel.function, c(list(object), k, panel.args))
         dev.off()
      }
   }

   else
   {
      for (i in seq(along=which)) {
         k <- which[i]
         if (k %in% 1:9) number <- paste("00",k,sep="")
         else if (k %in% 10:99) number <- paste("0",k,sep="")
         else number <- k
         do.call(panel.function, c(list(object), k, file=paste(file), panel.args))
      }
   }

})
