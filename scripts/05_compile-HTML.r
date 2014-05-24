#!/usr/bin/Rscript

## this script has two purposes
##   1 command line argument processing -- specifically getting the game
##   2 knitting vanNH-nowPlaying.rmd to HTML 

## note that purpose #2 can be achieved through a target like this in a Makefile:
# web: vanNH-nowPlaying.rmd
#   Rscript -e "library(knitr); knit2html('vanNH-nowPlaying.rmd')"
## but purpose #1 is not easy to address that way; hence this script

library(knitr)
library(markdown)

options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  stop("you must supply the game identifier via the command line")
} else {
  game <- options[1]
}

pathToCSS <- "resources/css/jasonm23-markdown-css-themes"
pathToCSS <- file.path(path.expand("~/"), pathToCSS, "markdown7.css")
options(markdown.HTML.stylesheet = pathToCSS)
rm(pathToCSS)

out_dir <- file.path("..", "games", game, "05_htmlArchive")
if(!file.exists(out_dir)) dir.create(out_dir)
md_file <- file.path(out_dir, paste0(game, "_live-stats.md"))
html_file <- file.path(out_dir, paste0(game, "_live-stats.html"))
md_file <- knit('06_vanNH-nowPlaying.rmd', output = md_file, quiet = TRUE)
message("wrote ", md_file)
markdownToHTML(md_file, output = html_file,
               stylesheet = file.path(path.expand("~/"),
                                      "resources/css/jasonm23-markdown-css-themes",
                                      "markdown7.css"))
message("wrote ", html_file)