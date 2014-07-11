library(plyr)

## when run in batch mode, provide game identifier on command line
options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  #game <- "2014-04-12_vanNH-at-pdxST"
  #game <- "2014-04-20_sfoDF-at-vanNH"
  #game <- "2014-04-26_vanNH-at-seaRM"
  #game <- "2014-05-10_seaRM-at-vanNH"
  #game <- "2014-05-17_vanNH-at-sfoDF"
  #game <- "2014-05-24_pdxST-at-vanNH"
  #game <- "2014-05-31_vanNH-at-seaRM"
  #game <- "2014-06-07_seaRM-at-vanNH"
  #game <- "2014-06-15_pdxST-at-vanNH"
  #game <- "2014-05-04_sfoDF-at-seaRM"
  game <- "2014-04-26_pdxST-at-sfoDF"
} else {
  game <- options[1]
}

## parse the game identifier
tmp <- strsplit(game, split = "_")[[1]]
game_date <- tmp[1]
tmp <- strsplit(tmp[2], split = "-")[[1]]
away_team <- tmp[1]
home_team <- tmp[3]
jTeams <- sort(c(away_team, home_team))

game_dir <- file.path("..", "games", game, "06_possess-game")
in_file <- file.path(game_dir, paste0(game, "_gameplay-resolved.tsv"))
game_play <- read.delim(in_file, stringsAsFactors = FALSE,
                        ## do this because of seaRM-rupp-00
                        colClasses = list(pl_pnum = "character"))

#str(game_play)
message(game, ":\n  ", nrow(game_play), " rows of resolved game play found")

game_play <-
  mutate(game_play,
         who = ifelse(poss_team == pl_team, 'O', 'D'),
         pl_code = mapvalues(pl_code, # no joy w/ revalue() due to level ''
                             from = '', to = '*'))
j_revalues <- c("OBP" = "P", "*" = "CTH", "A" = "CTH",
                "LG" = "G", "L" = "CTH", "PUA" = "PU",
                "LA" = "CTH", "HB" = "D", "FB" = "D",
                "VTT" = "OV", "VST" = "OV")
game_play <-
  mutate(game_play,
         pl_code = revalue(pl_code, j_revalues, warn_missing = FALSE))

source("50_pass-resolve-script.r")
# x <- subset(game_play, poss_abs == 70)
# resolve_passes(x)
pass_dat <- ddply(game_play, ~ poss_abs + poss_rel + point, resolve_passes)
#str(pass_dat, max.level = 1)

out_dir <- file.path("..", "games", game, "07_pass-game")
if(!file.exists(out_dir)) dir.create(out_dir)

message("  ", nrow(pass_dat), " rows of resolved pass data will be written\n")

out_file <- file.path(out_dir, paste0(game, "_passes.tsv"))
write.table(pass_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
#message("wrote ", out_file)

#with(pass_dat, table(desc, n_inn))
out_file <- file.path(out_dir, paste0(game, "_passes-tally.tsv"))
write.table(as.data.frame(with(pass_dat, table(desc, n_inn))), out_file,
            quote = FALSE, sep = "\t", row.names = FALSE)

# head(foo)
# 
# with(subset(foo, desc == "ao" & n_inn == 1),
#      table(innards))
# 
# subset(foo, desc == "an")
#      table(innards))

