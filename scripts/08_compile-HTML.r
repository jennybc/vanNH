#!/usr/bin/Rscript

## this script has two purposes
##   1 command line argument processing -- specifically getting the game
##   2 knitting vanNH-nowPlaying.rmd to HTML 

## note that purpose #2 can be achieved through a target like this in a Makefile:
# web: vanNH-nowPlaying.rmd
#   Rscript -e "library(knitr); knit2html('vanNH-nowPlaying.rmd')"
## but purpose #1 is not easy to address that way; hence this script

library(rmarkdown)

options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  #game <- "2014-04-12_vanNH-at-pdxST"
  #game <- "2014-04-20_sfoDF-at-vanNH"
  #game <- "2014-05-10_seaRM-at-vanNH"
  #game <- "2014-05-24_pdxST-at-vanNH"
  game <- "2014-05-31_vanNH-at-seaRM"
  #game <- "2014-06-07_seaRM-at-vanNH"
} else {
  game <- options[1]
}

local_html_file <- paste0(game, "_live-stats.html")
local_knit_file <- '06_vanNH-nowPlaying.knit.md'
local_utf8_file <- '06_vanNH-nowPlaying.utf8.md'

render('06_vanNH-nowPlaying.rmd', output_file = local_html_file,
       clean = FALSE, quiet = TRUE)

game_html_dir <- file.path("..", "games", game, "05_htmlArchive")
if(!file.exists(game_html_dir)) dir.create(out_dir)
game_md_file <- file.path(game_html_dir, paste0(game, "_live-stats.md"))
game_html_file <- file.path(game_html_dir, paste0(game, "_live-stats.html"))

file.rename(local_utf8_file, game_md_file)
message("wrote ", game_md_file)
file.rename(local_html_file, game_html_file)
message("wrote ", game_html_file)
file.remove(local_knit_file)
