#library(stringr)
library(plyr)

games <- list.files(file.path("..", "games"), pattern = "-at-")
ps_files <- list.files(file.path("..", "games", games), recursive = TRUE,
                       full.names = TRUE, pattern = "player-stats.tsv")
ps_files <- cbind(games,
                  matrix(ps_files, ncol = 2, byrow = TRUE,
                         dimnames = list(NULL, c("new", "old"))))
identical(seq_along(games),
          laply(games, function(gg) grep(gg, ps_files[ , "old"])))
identical(seq_along(games),
          laply(games, function(gg) grep(gg, ps_files[ , "new"])))

for(i in seq_len(nrow(ps_files))) {
  x <- ps_files[i, ]
message("game: ", i, " ", x["games"])
new_stats <- read.delim(x["new"], stringsAsFactors = FALSE)
new_stats <- mutate(new_stats, player = paste(player, last, sep = "-"))
#str(new_stats)
old_stats <- read.delim(x["old"], stringsAsFactors = FALSE)
#str(old_stats)
names(old_stats) <- paste0(names(old_stats), rep(c('', '_old'), c(2, 4)))
tmp <- join(old_stats, new_stats, by = c('game', 'player'))
print(rbind(old = nrow(old_stats), new = nrow(new_stats), joined = nrow(tmp)))
unequal_points <- with(tmp, points_old != points | is.na(points))
unequal_def <- with(tmp, Ds_old != def  | is.na(def))
n_discrepancies <- sum(unequal_points | unequal_def)
if(n_discrepancies > 0) {
  message(n_discrepancies, " discrepancies")
  print(tmp[unequal_points | unequal_def, ])
} else {
  message("AGREE!")
}
message("\n")
}

## this is a callahan
## 11 2014-05-04_phlSP-at-wdcCT phlSP-1-peters assists_old 3 assists 2
## 11 2014-05-04_phlSP-at-wdcCT phlSP-21-panna goals_old 1 goals 0

game <- "2014-05-04_phlSP-at-wdcCT"
game_play <- read.delim(file.path("..", "games", game, "06_possess-game",
                                  paste0(game, "_gameplay-resolved.tsv")))
str(game_play)
subset(game_play, pl_code %in% c("G") & pl_pnum == '21')
subset(game_play, poss_abs %in% 94:96)

names(pass_files) <- games
pass_dat <-
  ldply(pass_files, function(gg) read.delim(gg,
                           colClasses = list(beg_plyr = "character",
                                             innards = "character",
                                             end_plyr = "character")),
                           .id = "game")
str(pass_dat) # 16032 obs. of  15 variables

