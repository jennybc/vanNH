#!/usr/bin/Rscript

library(googlesheets)
library(magrittr)
library(plyr)
suppressMessages(library(docopt))
"%||%" <- function(a, b) if (!is.null(a)) a else b

## default game
#hard_wired_game <- "2015-06-06_seaRM-at-vanNH"
hard_wired_game <- "2015-05-03_pdxST-at-vanNH"

"Extract raw data from a Google Sheet

Usage: 01_extract-data-from-google-spreadsheet.R [-g <game> -s <sheet> -p <point>]

Options:
     -g <game>, --game=<game>  Game
  -s <sheet>, --sheet=<sheet>  Title of Google Sheet
  -p <point>, --point=<point>  Point [default: 1]
" -> doc

opts <- docopt(doc)
#opts
#opts <- docopt(doc, "-p 'c(2,4)'")
#opts <- docopt(doc, "-p 68")

game <- opts$game %||% opts$g %||% opts$sheet %||% opts$s %||% hard_wired_game
sheet <- opts$sheet %||% opts$s %||% game
point <- opts$point %||% opts$p
## note that you can pass a valid R expression for the point option BUT do NOT
## let it contain any spaces
## NO: ./01_extract-data-from-google-spreadsheet.R -p "c(1, 2)"
## YES: ./01_extract-data-from-google-spreadsheet.R -p "c(1,2)"
point <- as.integer(eval(parse(text = point)))

cat(sprintf("Game identifier: %s\n", game))
cat(sprintf("Google sheet identifier: %s\n", sheet))
cat(sprintf("Point: %s\n", paste(point, collapse = ", ")))

ss <- gs_title(sheet, verbose = FALSE)

cat(sprintf("Worksheets found: %s\n", ss$n_ws))

target_ws <- which(ss$ws$ws_title %in% as.character(point))

if(length(target_ws) < 1) {
  cat("Target worksheet(s) not found\n")
  if(!interactive()) q()
}

cat(sprintf("Reading from worksheet(s) titled: %s\n",
            paste(ss$ws$ws_title[target_ws], collapse = ", ")))

cat("Writing to ...\n")

l_ply(target_ws, function(x) {
  point_level_info <-
    gs_read_cellfeed(ss, x, range = "D1:D4",
                     return_empty = TRUE, verbose = FALSE) %>%
    gs_simplify_cellfeed(col_names = FALSE)
  point_level_info_names <-
    gs_read_cellfeed(ss, x, range = "C1:C4", verbose = FALSE) %>%
    gs_simplify_cellfeed(col_names = FALSE) %>%
    gsub(':$', '', .)
  names(point_level_info) <- point_level_info_names

  gameplay <-
    gs_read(ss, x, range = cell_limits(c(2, NA), c(1, 2)), verbose = FALSE)
  if(nrow(gameplay) < 1) {
    cat(sprintf("No gameplay found in worksheet titled: %s\n", x))
    return(NULL)
  }
  gameplay$Offense[is.na(gameplay$Offense)] <- ""
  gameplay$Defense[is.na(gameplay$Defense)] <- ""


  file_num <- sprintf("%02d", as.integer(gs_ws_ls(ss)[x]))
  out_file <- file.path("..", "games", game, "01_raw-google",
                        paste0(game, "_point", file_num, ".txt"))
  cat(sprintf("  %s\n", basename(out_file)))

  writeLines(sprintf("%s: %s", names(point_level_info), point_level_info),
             out_file)
  suppressWarnings(
    write.table(gameplay, out_file, append = TRUE, quote = FALSE, sep = "\t",
                row.names = FALSE, col.names = TRUE)
  )
})
