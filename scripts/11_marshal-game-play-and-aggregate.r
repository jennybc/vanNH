#+ setup, include = FALSE
library(plyr)
## useful inside reorder(), to invert the resulting factor levels
neglength <- function(x) -1 * length(x)

#' ### Bring in game play data.

#' Whitelist of games for which game play will be read. Read 'em.
games <- c("2014-04-12_vanNH-at-pdxST", "2014-04-20_sfoDF-at-vanNH",
           "2014-04-26_vanNH-at-seaRM", "2014-05-10_seaRM-at-vanNH",
           "2014-05-17_vanNH-at-sfoDF", "2014-05-24_pdxST-at-vanNH",
           "2014-05-31_vanNH-at-seaRM", "2014-06-07_seaRM-at-vanNH",
           "2014-06-15_pdxST-at-vanNH", "2014-06-21_vanNH-at-sfoDF",
           "2014-06-28_vanNH-at-pdxST",
           "2014-04-12_seaRM-at-sfoDF", "2014-04-19_sfoDF-at-seaRM",
           "2014-04-26_pdxST-at-sfoDF", "2014-05-04_sfoDF-at-seaRM")
game_file <- file.path("..", "games", games, "07_resolvedGame",
                       paste0(games, "_gameplay-resolved.tsv"))
names(game_file) <- games
game_play <-
  ldply(game_file, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(game_play)

#' ### Concatenate all game play data 

out_dir <- file.path("..", "games", "2014_west")
if(!file.exists(out_dir)) dir.create(out_dir)

out_file <- file.path(out_dir, "2014_west_gameplay.rds")
saveRDS(game_play, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_gameplay.tsv")
write.table(game_play, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_gameplay.dput")
dput(game_play, out_file)
message("wrote ", out_file)





#' Write `poss_dat` to file.
out_file <- file.path(out_dir, "2014_west_possessions.rds")
saveRDS(poss_dat, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_possessions.tsv")
write.table(poss_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_possessions.dput")
dput(poss_dat, out_file)
message("wrote ", out_file)

#' ### Aggregate to the level of a point 

#' How does `point_dat` differ from `poss_dat`, other than aggregation?
#' 
#' *  Just know that `n_events` is not the number of the events of the last
#' possession, but rather is computed from `poss_dat$event` and gives the number
#' of events in the whole point.
point_dat <- ddply(poss_dat, ~ game + point, function(x) {
  n <- nrow(x)
  x$n_events <- NULL
  x <- rename(x, c("event" = "n_events", "poss_rel" = "n_poss"))
  x[n, ]
})
str(point_dat)
table(point_dat$score, useNA = "always")
table(point_dat$pl_code, useNA = "always")
table(point_dat$a_code, useNA = "always")
table(point_dat$b_code, useNA = "always")
table(point_dat$huck, useNA = "always")
addmargins(with(point_dat, table(score, huck)))

out_file <- file.path(out_dir, "2014_west_points.rds")
saveRDS(point_dat, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_points.tsv")
write.table(point_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_points.dput")
dput(point_dat, out_file)
message("wrote ", out_file)
