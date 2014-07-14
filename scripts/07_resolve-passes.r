library(plyr)
library(reshape2)

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
  #game <- "2014-04-26_pdxST-at-sfoDF"
  #game <- "2014-05-03_sfoDF-at-pdxST"
  game <- "2014-06-07_wdcCT-at-bosWC"
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
message(game, " > 07_resolve-passes.r:\n  ", nrow(game_play),
        " rows of resolved game play found")

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
# x <- subset(game_play, poss_abs == 8)
# resolve_passes(x)
pass_dat <- ddply(game_play, ~ poss_abs + poss_rel + point, resolve_passes)
#str(pass_dat, max.level = 1)

## compare pass data to whitelist, to alert us to novel, nonstandard play
source("07_white-list-pass-candidates.r")
pass_dat <- name_rows(pass_dat)
is_white_listed <-
  suppressMessages(as.integer(rownames(match_df(pass_dat, white_list))))
pass_dat <- name_rows(pass_dat)
tmp <- rep_len(FALSE, nrow(pass_dat))
tmp[is_white_listed] <- TRUE
pass_dat$wl <- tmp
message(sprintf("  %d/%d rows of pass data are whitelisted (%.1f%%)",
                sum(pass_dat$wl), nrow(pass_dat), 100 * mean(pass_dat$wl), ""))
if(!all(pass_dat$wl)) {
  message(sprintf("  ALERT %d rows of pass data are novel", sum(!pass_dat$wl)))
  pass_dat[!pass_dat$wl, ]
}

## map the pass data onto pass classes, e.g. compl, taway, ...
source("07_pass-map.r")
pass_dat <- suppressMessages(join(pass_dat, pass_map))
## add a level that does not come up in the previous pass map
pass_dat <-
  mutate(pass_dat, pclass = factor(pclass,
                                   levels = c(levels(pclass), "nopass")))

## special handling for {O-CTH, O-PU} {O-TO, O-OV}
fill_me <-
  with(pass_dat, beg_code %in% c('O-CTH', 'O-PU') &
         end_code %in% c('O-TO', 'O-OV'))
fill_me[is.na(fill_me)] <- FALSE
player_different <- with(pass_dat, beg_plyr != end_plyr)
end_player_defined <- !grepl("-$", pass_dat$end_plyr)
player_different <- player_different & end_player_defined
pass_dat$pclass[fill_me & !player_different] <- "nopass"
pass_dat$pclass[fill_me & player_different] <- "compl"

## special handling for {O-CTH, O-PU} O-PU
## so far these seem to usually involve lots of subs off and in
## IRL, raise an alert so we look at these
fill_me <-
  with(pass_dat, beg_code %in% c('O-CTH', 'O-PU') & end_code == 'O-PU')
fill_me[is.na(fill_me)] <- FALSE
pass_dat$pclass[fill_me] <- "nopass"
if(any(fill_me)) {
  message(sprintf("  ALERT %d rows of pass data end with pickup (PU)",
                  sum(fill_me)))
  pass_dat[fill_me, ]
}

message("  breakdown of passes:")
table(pass_dat$pclass, useNA = "always")

n_compl <- sum(pass_dat$pclass == 'compl')
n_pass <- sum(pass_dat$pclass %in% c('compl', 'ofoul', 'taway', 'defd', 'drop'))
message(sprintf("  %d/%d passes were completed (%.1f%%)", n_compl, n_pass,
                100 * n_compl / n_pass))

out_dir <- file.path("..", "games", game, "07_pass-game")
if(!file.exists(out_dir)) dir.create(out_dir)

message("  ", nrow(pass_dat), " rows of pass data will be written")

out_file <- file.path(out_dir, paste0(game, "_passes.tsv"))
write.table(pass_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
#message("wrote ", out_file)

#with(pass_dat, table(desc, n_inn))
out_file <- file.path(out_dir, paste0(game, "_passes-tally.tsv"))
write.table(as.data.frame(with(pass_dat, table(desc, n_inn))), out_file,
            quote = FALSE, sep = "\t", row.names = FALSE)

## moving on to player summaries
build_roster_path <- function(team) {
  file.path("..", "rosters", paste0("2014_", team, "-roster.tsv"))}
away_roster_file <- build_roster_path(away_team)
home_roster_file <- build_roster_path(home_team)
read_roster <- function(roster_file) {
  x <- read.delim(roster_file, stringsAsFactors = FALSE,
                  ## do this because of seaRM-rupp-00
                  colClasses = list(number = "character"))
  x[c('number', 'last')]
}
away_roster <- read_roster(away_roster_file)
home_roster <- read_roster(home_roster_file)
message("  roster read for ", away_team, " and ", home_team)
away_roster <- data.frame(team = away_team, away_roster)
home_roster <- data.frame(team = home_team, home_roster)
dual_roster <- rbind(away_roster, home_roster)
dual_roster <- mutate(dual_roster, player = paste(team, number, sep = "-"))

pl_dat <-
  data.frame(player = I(with(pass_dat, sort(unique(c(beg_plyr, end_plyr))))))
tmp <- do.call("rbind", strsplit(pl_dat$player, "-"))
pl_dat <- data.frame(pl_dat, team = tmp[ , 1], number = tmp[ , 2],
                     stringsAsFactors = FALSE)
pl_dat <- suppressMessages(join(pl_dat, rbind(away_roster, home_roster)))
pl_dat <- subset(pl_dat, !grepl("-$", pl_dat$player))

jFun <- function(the_plyr) {
  x <- subset(pass_dat, beg_plyr == the_plyr | end_plyr == the_plyr)
  z <-
    with(x,
         data.frame(player = the_plyr,
                    #team = strsplit(the_plyr, "-")[[1]][1],
                    goals = sum(end_code == 'O-G' & end_plyr == the_plyr,
                                na.rm = TRUE),
                    assists = sum(end_code == 'O-G' & beg_plyr == the_plyr,
                                  na.rm = TRUE),
                    throws = sum(beg_plyr == the_plyr & pclass != 'nopass'),
                    completions = sum(beg_plyr == the_plyr & pclass == 'compl'),
                    catches = sum(end_plyr == the_plyr & pclass == 'compl',
                                  na.rm = TRUE),
                    def = sum(end_plyr == the_plyr & pclass == 'defd',
                              na.rm = TRUE),
                    drop = sum(beg_plyr == the_plyr & pclass == 'drop')))
  z <- mutate(z, points = goals + assists,
              comp_pct = round(completions / throws, 2))
  vars_how_i_want <-
    c('player', 'points', 'comp_pct', 'goals', 'assists',
      'throws', 'completions', 'catches', 'def', 'drop')
  z <- z[vars_how_i_want]
  return(z)
  ## non-essential stats I could add later:
  ## BE = bookend; player gets a D then scores goal later in same possession
  ## double happiness is special case of BE, when it's very next throw
  ## Callahans, Greatests (how would I even know about Greatests?)
  ## all the stuff that relies on systematic processing of who's on the line:
  ## points played (overal, on O, on D), plus/minus
}
pl_stats <- ldply(pl_dat$player, jFun)
#pl_stats
pl_stats <- suppressMessages(join(pl_stats, dual_roster))
pl_stats_by_team <- split(pl_stats, pl_stats$team)
pl_stats_by_team <- llply(pl_stats_by_team, function(z) {
  vars_how_i_want <-
    c('player', 'last', 'points', 'comp_pct', 'goals', 'assists',
      'throws', 'completions', 'catches', 'def', 'drop', 'team', 'number')
  z <- z[vars_how_i_want]
  z <- arrange(z, desc(points), desc(goals), desc(assists), desc(def))
  rownames(z) <- NULL
  return(z)
})
#str(pl_stats_by_team)
#pl_stats_by_team

pl_stats_combined <- do.call("rbind", pl_stats_by_team)
pl_stats_combined <- data.frame(game = game, pl_stats_combined)
#str(pl_stats_combined)

message("  writing player stats to file: ",
        nrow(pl_stats_by_team[[away_team]]), " for ", away_team, ", ",
        nrow(pl_stats_by_team[[home_team]]), " for ", home_team)

out_file <- file.path(out_dir, paste0(game, "_player-stats.tsv"))
write.table(pl_stats_combined, out_file,
            quote = FALSE, sep = "\t", row.names = FALSE)
#message("wrote ", out_file)

message("  ", Sys.time(), "\n")
