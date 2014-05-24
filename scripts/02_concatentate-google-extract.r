library(yaml)
library(plyr)

options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  #game <- "2014-04-12_vanNH-at-pdxST"
  game <- "2014-04-20_sfoDF-at-vanNH"
} else {
  game <- options[1]
}

game_dir <- file.path("..", "games", game)
google_dir <- file.path(game_dir, "01_rawGoogleExtract")
google_files <- list.files(google_dir, full.names = TRUE)
nPoints <- length(google_files)
message(nPoints, " point files found\n")

jFun <- function(point) {
  point_string <- sprintf("%02d", point)
  point_file <- grep(paste0("point", point_string), google_files, value = TRUE)
  point_info <- yaml.load(paste(readLines(point_file, n = 4), collapse = "\n"))
  #point_info <- c(point = list(point), point_info)
  point_data <- read.delim(point_file, skip = 4, stringsAsFactors = FALSE)
  return(list(point_info = point_info, point_data = point_data))
}

rawData <- alply(seq_len(nPoints), 1, jFun)
#str(rawData, list.len = 2)

game_play <- ldply(rawData, function(x) x$point_data)
game_play <- rename(game_play, c("X1" = "point"))
game_play$point <- as.integer(game_play$point)
#str(game_play)

point_info <- ldply(rawData, function(x) {
  data.frame(x$point_info, stringsAsFactors = FALSE)})
point_info <- rename(point_info, c("X1" = "point"))
point_info$point <- as.integer(point_info$point)
#str(point_info)

out_dir <- file.path("..", "games", game, "03_concatGoogleExtract")
if(!file.exists(out_dir)) dir.create(out_dir)

out_file <- file.path(out_dir, paste0(game, "_gameplay-raw.tsv"))
write.table(game_play, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, paste0(game, "_points-raw.tsv"))
write.table(point_info, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)
