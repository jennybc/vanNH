#!/usr/bin/Rscript

## this script had two purposes
##   1 command line argument processing -- specifically getting the game
##   2 knitting to HTML the vanNH-nowPlaying.rmd

## note that purpose #2 can be achieved through a target like this in a Makefile:
# web: 06_vanNH-nowPlaying.rmd
#   Rscript -e "library(knitr); knit2html('06_vanNH-nowPlaying.rmd')"
## but purpose #1 is not easy to address that way; hence this script

options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  stop("you must supply the game identifier via the command line")
} else {
  game <- options[1]
}

library(knitr)
knit2html('06_vanNH-nowPlaying.rmd')
