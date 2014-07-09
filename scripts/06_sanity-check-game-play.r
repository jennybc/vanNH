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
  #game <- "2014-04-26_pdxST-at-sfoDF"
  game <- "2014-06-28_vanNH-at-pdxST"
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

game_dir <- file.path("..", "games", game, "05_cleanedGame")
in_file <- file.path(game_dir, paste0(game, "_gameplay-clean.tsv"))
game_play <- read.delim(in_file, stringsAsFactors = FALSE,
                        ## do this because of seaRM-rupp-00
                        colClasses = list(pull_pnum = "character",
                                          recv_pnum = "character"))
#str(game_play)
message(game, ":\n  ", nrow(game_play), " rows of clean game play found")

## detect pulls with no valid pull code
pull_codes <- c('P', 'OBP')
no_explicit_pull <- with(game_play, event == 1 & !(pull_code %in% pull_codes))
if(any(no_explicit_pull)) {
  message("\nALERT: point(s) with NO explicit pull code (P, OBP)")
  game_play[no_explicit_pull, ]
}

## detect pickups off the pull with no valid PU code
pickup_codes <- c('PU', 'PUA')
no_explicit_pu <- with(game_play, event == 2 & !(recv_code %in% pickup_codes))
if(any(no_explicit_pu)) {
  message("\nALERT: point(s) with NO explicit pickup (PU, PUA) off the pull")
  game_play[no_explicit_pu, ]
}

## TO DO
## make sure any GL --> LG

## TO DO
## check for any explicit assists

## TO DO
## check for more than one goal
## here's code lifted from later ... makes more sense to do this here
# its_a_goal <- grepl("L*G", x$pl_code)
# if(sum(its_a_goal) > 1) {
#   message("point: ", point, " ... more than one goal code detected!")
# }

## TO DO?
## detect points with no explicit goal and make sure it's at end of a period

message("\n  sanity checks of clean game play DONE")
message("  ", Sys.time(), "\n")
