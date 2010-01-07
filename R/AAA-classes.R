#
#  Copyright (C) 2009 Friedrich Leisch, Theresa Scharl, Ingo Voglhuber
#  $Id: AAA-classes.R 4249 2009-01-13 14:06:28Z scharl $
#

setClass("graphdata",
         representation(Ragraph="Ragraph",
                        kcca="kccasimple",
                        bgdata="ANY",
                        node.function="function",
                        edge.method="character",
                        theme="character",
                        colscale="numeric"))

