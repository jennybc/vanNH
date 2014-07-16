#' ---
#' title: "Marshal data across games"
#' output:
#'  html_document:
#'    keep_md: true
#' ---

#+ setup, include = FALSE
library(plyr)
## useful inside reorder(), to invert the resulting factor levels
neglength <- function(x) -1 * length(x)

#' How many games have I ingested?
length(games <- list.files(file.path("..", "games"), pattern = "-at-")) # 32

#' ### Bring in, concatenate, and write possession data.
poss_files <- list.files(file.path("..", "games", games, "06_possess-game"),
                         pattern = "_possessions.tsv", full.names = TRUE)
names(poss_files) <- games
poss_dat <-
  ldply(poss_files, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(poss_dat) # 2830 obs. of  18 variables:

out_dir <- file.path("..", "games", "2014_all-games")
if(!file.exists(out_dir)) dir.create(out_dir)

out_file <- file.path(out_dir, "2014_possessions.rds")
saveRDS(poss_dat, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_possessions.tsv")
write.table(poss_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_possessions.dput")
dput(poss_dat, out_file)
message("wrote ", out_file)

#' ### Bring in, concatenate, and write point data.
point_files <- list.files(file.path("..", "games", games, "06_possess-game"),
                          pattern = "_points-resolved.tsv", full.names = TRUE)
names(point_files) <- games
point_dat <-
  ldply(point_files, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(point_dat) # 1314 obs. of  17 variables:

out_file <- file.path(out_dir, "2014_points.rds")
saveRDS(point_dat, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_points.tsv")
write.table(point_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_points.dput")
dput(point_dat, out_file)
message("wrote ", out_file)

#' ### Bring in, concatenate, and write pass data.
pass_files <- list.files(file.path("..", "games", games, "07_pass-game"),
                          pattern = "_passes.tsv", full.names = TRUE)
names(pass_files) <- games
pass_dat <-
  ldply(pass_files, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(pass_dat) # 16033 obs. of  15 variables:

out_file <- file.path(out_dir, "2014_passes.rds")
saveRDS(pass_dat, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_passes.tsv")
write.table(pass_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_passes.dput")
dput(pass_dat, out_file)
message("wrote ", out_file)

#' ### Bring in, concatenate, and write player stats.
ps_files <- list.files(file.path("..", "games", games, "07_pass-game"),
                       pattern = "_player-stats.tsv", full.names = TRUE)
names(ps_files) <- games
ps_dat <-
  ldply(ps_files, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(ps_dat) # 1400 obs. of  14 variables:

out_file <- file.path(out_dir, "2014_player-game-stats.rds")
saveRDS(ps_dat, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_player-game-stats.tsv")
write.table(ps_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_player-game-stats.dput")
dput(ps_dat, out_file)
message("wrote ", out_file)
