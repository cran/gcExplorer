#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl, Ingo Voglhuber
#  $Id: calcHCL.R 4248 2009-01-13 13:57:10Z scharl $
#

col2hue <- function(hex) {
   rgb <- col2rgb(hex)
   hue <- rgb2hsv(rgb)[1]*360
   return(hue)
}

calcHCL <- function(color, value, area) {
   if (color == "black")
   {
      return("black")
   }
   if (color == "grey")
   {
      level <- (0.7 - ( (0.7 / (area[2] - area[1])) * (value - area[1]) ))
      if (level < 0) level <- 0.3
      else level <- level + 0.3
      if (level > 1) level <- 1
      return(grey(level))
   }
	hue <- col2hue(color)
	lum <- round(75 - ( (75 / (area[2] - area[1])) * (value - area[1]) ))
	if (lum < 0) lum <- 25
	else lum <- lum + 25
	if (lum > 100) lum <- 100
	return(hcl(h = hue, c = 35, l = lum))
}
