library(yaml)
library(plyr)

options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  #game <- "2014-04-12_vanNH-at-pdxST"
  game <- "2014-04-26_vanNH-at-seaRM"
  #game <- "2014-04-20_sfoDF-at-vanNH"
  #game <- "foo"
} else {
  game <- options[1]
}

game_dir <- file.path("..", "games", game)
google_dir <- file.path(game_dir, "01_raw-google")
google_files <- list.files(google_dir, full.names = TRUE)
nPoints <- length(google_files)
message(game, " > 03_concatenate-google-extract.r:\n  ",
        nPoints, " point files found")

## function imports data for a point -- both gameplay and point-level info
jFun <- function(point) {
  point_string <- sprintf("%02d", point)
  point_file <- grep(paste0("point", point_string), google_files, value = TRUE)
  point_info <- yaml.load(paste(readLines(point_file, n = 4), collapse = "\n"))
  point_data <- read.delim(point_file, skip = 4,
                           stringsAsFactors = FALSE, strip.white = TRUE)
  return(list(point_info = point_info, point_data = point_data))
}
rawData <- alply(seq_len(nPoints), 1, jFun)
#str(rawData, list.len = 2) # List of 47

## concatenate game play data across all points
game_play <- ldply(rawData, function(x) x$point_data)
game_play <- rename(game_play, c("X1" = "point"))
game_play$point <- as.integer(game_play$point)
# str(game_play) # 'data.frame':  ? obs. of  3 variables: point, Offense, Defense

## concatenate point-level info across all points
point_info <-
  ldply(rawData, function(x) data.frame(x$point_info, stringsAsFactors = FALSE))
point_info <- rename(point_info, c("X1" = "point"))
point_info$point <- as.integer(point_info$point)
#str(point_info)

out_dir <- file.path("..", "games", game, "03_concat-google")
if(!file.exists(out_dir)) dir.create(out_dir)

message("  ", "writing ", nrow(game_play), " rows of concatenated game play")

out_file <- file.path(out_dir, paste0(game, "_gameplay-raw.tsv"))
write.table(game_play, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
#message("wrote ", out_file)

message("  ", "writing ", nrow(point_info), " rows of point info\n")

out_file <- file.path(out_dir, paste0(game, "_points-raw.tsv"))
write.table(point_info, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
#message("wrote ", out_file)
