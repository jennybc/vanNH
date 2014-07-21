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
  #game <- "2014-05-31_vanNH-at-seaRM"
  #game <- "2014-06-07_seaRM-at-vanNH"
  #game <- "2014-05-03_sfoDF-at-pdxST"
  game <- "2014-07-19_vanNH-at-wdcCT"
} else {
  game <- options[1]
}

local_html_file <- paste0(game, "_live-stats.html")
local_md_file <- paste0(game, "_live-stats.md")
local_figure_dir <- file.path(paste0(game, "_live-stats_files"), "figure-html")

render('09_vanNH-nowPlaying.rmd', output_file = local_html_file, quiet = TRUE)

game_html_dir <- file.path("..", "games", game, "09_html")
game_figure_dir <- file.path(game_html_dir, paste0(game, "_live-stats_files"),
                             "figure-html")
if(!file.exists(game_html_dir)) dir.create(game_html_dir)
if(!file.exists(game_figure_dir)) {
  dir.create(game_figure_dir, recursive = TRUE)
} else {
  foo <- file.remove(list.files(game_figure_dir, full.names = TRUE))
}
game_md_file <- file.path(game_html_dir, paste0(game, "_live-stats.md"))
game_html_file <- file.path(game_html_dir, paste0(game, "_live-stats.html"))

foo <- file.rename(local_md_file, game_md_file)
#message("wrote ", game_md_file)
foo <- file.rename(local_html_file, game_html_file)
#message("wrote ", game_html_file)
foo <- file.rename(local_figure_dir, game_figure_dir)
foo <- file.remove(dirname(local_figure_dir))
