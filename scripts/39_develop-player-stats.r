#library(stringr)
library(plyr)

# games <- list.files(file.path("..", "games"), pattern = "-at-")
# pass_files <- list.files(file.path("..", "games", games), recursive = TRUE,
#   full.names = TRUE, pattern = "passes.tsv")
# identical(seq_along(games), laply(games, function(gg) grep(gg, pass_files)))
# names(pass_files) <- games
# pass_dat <-
#   ldply(pass_files, function(gg) read.delim(gg,
#                            colClasses = list(beg_plyr = "character",
#                                              innards = "character",
#                                              end_plyr = "character")),
#                            .id = "game")
#str(pass_dat) # 16032 obs. of  15 variables

game <- "2014-05-03_sfoDF-at-pdxST"
new_stats <- file.path("..", "games", game, "07_pass-game",
                       paste0(game, "_player-stats.tsv"))
new_stats <- read.delim(new_stats)
new_stats <- mutate(new_stats, player = paste(player, last, sep = "-"))
str(new_stats)
old_stats <- file.path("..", "games", game, "09_html",
                       paste0(game, "_player-stats.tsv"))
old_stats <- read.delim(old_stats)
str(old_stats)
names(old_stats) <- paste0(names(old_stats), rep(c('', '_old'), c(2, 4)))
intersect(names(new_stats), names(old_stats))
tmp <- join(old_stats, new_stats, by = c('game', 'player'))
str(tmp) # 35 obs. of  18 variables:
subset(tmp, points_old != points)
subset(tmp, Ds_old != def)
## sfoDF-13-Grant goals_old 0 goals 1 MLU says 1

game_play <- read.delim(file.path("..", "games", game, "06_possess-game",
                                  paste0(game, "_gameplay-resolved.tsv")))
str(game_play)
subset(game_play, pl_code %in% c("LG", "G") & pl_pnum == 13)
with(game_play, pl_pnum[pl_code %in% c("LG", "G")])
