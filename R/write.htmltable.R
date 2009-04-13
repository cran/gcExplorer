#
#  Copyright (C) 2003 Andreas Buness and Wolfgang Huber
#  $Id: write.htmltable.R 4251 2009-01-13 18:18:10Z leisch $
#

##------------------------------------------------------------
## writes a html table 
## x:        data frame
## filename: character with the name of the output file
## sortby:   name of a column in data frame din
##------------------------------------------------------------
write.htmltable <- function (x, filename, title="", sortby=NULL, decreasing=TRUE,
                             open="wt", formatNumeric=function(x) paste(signif(x, 3))) {

  if(!is.null(sortby)) {
    if(!sortby %in% colnames(x))
      stop(paste("Invalid argument \"sortby\": could not find a column in data frame x with name", sortby))
    soby = x[, sortby]
    if(!is.numeric(soby))
      stop("Invalid argument \"sortby\": column is not numeric")
    x = x[order(soby, decreasing=decreasing), ]
  }
  
  outfile <- file(paste(filename, ".html", sep=""), open=open)
  cat("<html>", "<STYLE>", 
      "<!--TD { FONT-FAMILY: Helvetica,Arial; FONT-SIZE: 14px;}-->",
      "<!--H1 { FONT-FAMILY: Helvetica,Arial; FONT-SIZE: 22px;}-->",
      "</STYLE>", "<head>", paste("<TITLE>", title, "</TITLE>", sep=""),
      "</head>", "<body bgcolor=#ffffff>", file = outfile, sep = "\n")
  
  if (title!="") 
    cat("<CENTER><H1 ALIGN=\"CENTER\">", title, " </H1></CENTER>\n", 
        file = outfile, sep = "\n")
  cat("<CENTER> \n", file = outfile)

  cat("<TABLE BORDER=0>", file = outfile, sep = "\n")
  
  cat("<TR>", file = outfile)
  for (j in 1:ncol(x))
    cat("<TD BGCOLOR=\"", c("#e0e0ff", "#d0d0f0")[j%%2+1], "\"><B>", colnames(x)[j], "</B></TD>\n", sep="", file = outfile)
  cat("</TR>", file = outfile)
  
  for (i in 1:nrow(x)) {
    cat("<TR>", file = outfile)
    for (j in 1:ncol(x)) {
      txt = switch(class(x[[j]]),
        numeric = formatNumeric(x[i, j]),
        as.character(x[i,j]))
      
      if (length(grep("^http:", txt))>0) {
        txt <- sub(";$", "", txt)              ## remove trailing semicolon
        s <- unlist(strsplit(txt, "[/?=]"))    ## split out last part of URL
        txt <- paste("<A HREF=\"", txt, "\" TARGET=\"z\">", s[length(s)], "</A>", sep="")
      }
      cat("<TD BGCOLOR=\"", c("#e0e0ff", "#d0d0f0", "#f0f0ff", "#e0e0f0")[i%%2*2+j%%2+1], "\">",
          txt, "</TD>\n", sep="", file = outfile)
    }
    cat("</TR>", file = outfile)
  }

  cat("</TABLE></CENTER>", "</body>", "</html>", sep = "\n", file = outfile)
  close(outfile)
}

