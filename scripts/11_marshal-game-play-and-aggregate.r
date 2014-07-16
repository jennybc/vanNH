#+ setup, include = FALSE
library(plyr)
## useful inside reorder(), to invert the resulting factor levels
neglength <- function(x) -1 * length(x)

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
