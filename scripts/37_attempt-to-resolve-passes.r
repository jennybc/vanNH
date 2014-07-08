## a one-off script to test my draft function for resolving passes

library(plyr)
in_file <- file.path("..", "games", "2014_west", "2014_west_gameplay.rds")
game_play <- readRDS(in_file, in_file)
str(game_play)

game_play <-
  mutate(game_play,
         who = ifelse(poss_team == pl_team, 'O', 'D'),
         pl_code = mapvalues(pl_code, # no joy w/ revalue() due to level ''
                             from = '', to = '*'))
j_revalues <- c("OBP" = "P", "*" = "CTH", "A" = "CTH",
                "LG" = "G", "L" = "CTH", "PUA" = "PU",
                "LA" = "CTH", "HB" = "D", "FB" = "D",
                "VTT" = "OV", "VST" = "OV")
game_play <- mutate(game_play, pl_code = revalue(pl_code, j_revalues))

game_play <-
  within(game_play, {
    is_alpha <- pl_code %in% c("CTH", "PU")
    is_omega <- pl_code %in% c("CTH", "D", "G", "TD", "OV")
    e_code <- paste(who, pl_code, sep = "-")
  })
str(game_play)

source("50_pass-resolve-script.r")
foo <- ddply(game_play, ~ game + poss_abs + poss_rel + point, resolve_passes)
str(foo, max.level = 1)
