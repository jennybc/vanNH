library(plyr)
library(yaml)

## I predict this function will be moved to a file of helper functions and,
## ultimately, into a helper package
replace_NA_with_empty_string <- function(x) {x[is.na(x)] <- ""; return(x)}

## when run in batch mode, provide game identifier on command line
options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  #game <- "2014-04-12_vanNH-at-pdxST"
  #game <- "2014-04-20_sfoDF-at-vanNH"
  #game <- "2014-04-26_vanNH-at-seaRM"
  #game <- "2014-05-10_seaRM-at-vanNH"
  #game <- "2014-05-17_vanNH-at-sfoDF"
  #game <- "2014-05-24_pdxST-at-vanNH"
  game <- "2014-05-31_vanNH-at-seaRM"
  #game <- "2014-06-07_seaRM-at-vanNH"
  #game <- "2014-06-15_pdxST-at-vanNH"
  #game <- "2014-05-04_sfoDF-at-seaRM"
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

game_dir <- file.path("..", "games", game, "04_clean-game")
in_file <- file.path(game_dir, paste0(game, "_gameplay-clean.tsv"))
game_play <- read.delim(in_file, stringsAsFactors = FALSE,
                        ## do this because of seaRM-rupp-00
                        colClasses = list(pull_pnum = "character",
                                          recv_pnum = "character"))
#str(game_play)
message(game, ":\n  ", nrow(game_play), " rows of clean game play found")

game_dir <- file.path("..", "games", game, "03_concat-google")
in_file <- file.path(game_dir, paste0(game, "_points-raw.tsv"))
point_info <- read.delim(in_file, stringsAsFactors = FALSE)
#str(point_info)
message("  ", nrow(point_info), " rows of point info play found")

## replace long team names with my official short versions
mlu_teams <- read.delim(file.path("..", "data", "mlu-teams.tsv"))
## I use *p*match() to accomodate, eg, "Portland" instead of "Portland Stags"
point_info$pull_team <-
  factor(mlu_teams$team[pmatch(point_info$Pulling.team, mlu_teams$longName,
                               duplicates.ok = TRUE)],
         levels = jTeams)
point_info$Pulling.team <- NULL
## rename vars
point_info <-
  rename(point_info, c("Period" = "period", "Clock.before.point" = "clk_before",
         "Clock.after.point" = "clk_after"))

## replace NAs in game_play$pull_pnum and game_play$recv_pnum with ""
game_play <-
  transform(game_play,
            recv_pnum = replace_NA_with_empty_string(recv_pnum),
            pull_pnum = replace_NA_with_empty_string(pull_pnum))

## collapse recv_code/recv_pnum/pull_code/pull_pnum into pl_team, pl_pnum,
## pl_code
jFun <- function(x) {
  x$play_by_recv_team <- x$pull_pnum == '' & x$pull_code == ''
  x$pl_team <- with(x, ifelse(play_by_recv_team, "recv_team", "pull_team"))
  x$pl_pnum <- with(x, ifelse(play_by_recv_team, recv_pnum, pull_pnum))
  x$pl_code <- with(x, ifelse(play_by_recv_team, recv_code, pull_code))
  x[c('point', 'event', 'pl_team', 'pl_pnum', 'pl_code')]
}
game_play <- ddply(game_play, ~ point, jFun)

## function to get the "other" team, assuming team is a two level factor
get_opponent <- function(x) {
  jLevels <- levels(x)
  x <- ifelse(unclass(x) == 1, 2, 1)
  return(factor(jLevels[x], levels = jLevels))
}

## replace pl_team values 'pull_team' / 'recv_team' with, e.g. 'vanNH' / 'seaRM'
jFun <- function(x) {
  point <- x$point[1]
  pull_team <- point_info$pull_team[point_info$point == point]
  recv_team <- get_opponent(pull_team)
  period <- point_info$period[point_info$point == point]
  x$pl_team <- revalue(x$pl_team, c("pull_team" = as.character(pull_team),
                                    "recv_team" = as.character(recv_team)))
  x$pl_team <- factor(x$pl_team, levels = jTeams)
  return(data.frame(period = period, x))
}
game_play <- ddply(game_play, ~ point, jFun)

## assign the assist
## Is there an advantage to doing this before/after determining possession?
jFun <- function(x) {
  n <- nrow(x)
  point <- x$point[1]
  its_a_goal <- grepl("L*G", x$pl_code)
  if(sum(its_a_goal) > 1) {
    message("point: ", point, " ... more than one goal code detected!")
  }
  ## I do it this way in case there is foul on the goal catch, which
  ## means the n-th event is not the goal itself    
  goal_row <- which(its_a_goal)
  if(length(goal_row) > 0) {
    goal_row <- max(goal_row)
    sc_team <- x$pl_team[goal_row]
    ## I do it this way in case there is an intervening defensive foul, which
    ## means the assist is not in goal_row - 1
    ## WARNING: this won't work correctly for a Callahan
    assist_row <- rev(which(with(x, pl_team == sc_team)))[2]
    ## in my experience, existing code can be '', 'L', 'PU'
    assist_code <- x$pl_code[assist_row]
    if(grepl("A", assist_code)) {
      message("ALERT: point", x$point[1], "has an explicit assist (A)")
    } else {
      x$pl_code[assist_row] <- paste0(assist_code, 'A')
    }
  }
  return(x)
}
game_play <- ddply(game_play, ~ point, jFun)

## determine who's got possession

## define some code groups
goal_codes <- c('G', 'LG')
assist_codes <- c('A', 'LA', 'PUA')
more_offense_codes <- c('', 'PU', 'L', 'TD', 'VST', 'VTT', 'TO')
offense_codes <- c(more_offense_codes, goal_codes, assist_codes)
d_codes <- c('D', 'HB', 'FB')
pickup_codes <- c('PU', 'PUA')

## determine who possesses the disc: the easy stuff, i.e. possession can be
## determined directly from the event code
jFun <- function(x) {
  x$poss_team <- factor(NA, levels = jTeams)
  x$poss_team[1:2] <- x$pl_team[2]
  play_is_offensive <- x$pl_code %in% offense_codes
  x$poss_team[play_is_offensive] <- x$pl_team[play_is_offensive]
  play_is_defensive <- x$pl_code %in% d_codes
  if(any(play_is_defensive)) {
    x$poss_team[play_is_defensive] <- get_opponent(x$pl_team[play_is_defensive])
  }
  return(x)
}
game_play <- ddply(game_play, ~ point, jFun)
game_play <- mutate(game_play, poss_team = factor(poss_team, levels = jTeams))
poss_ok <- sum(!is.na(game_play$poss_team))
n <- nrow(game_play)
message("  possession established directly for ", poss_ok, "/", n, " = ",
        round(100 * poss_ok/n, 2), "% of game play events")

## now we have to do some look ahead / behind to determine possession :(
infer_possession <- function(x) {
  naDat <- data.frame(row = which(is.na(x$poss_team)))
  naDat <- mutate(naDat,
                  pl_code = x$pl_code[row],
                  row_bef = ifelse(row - 1 < 1, NA, row - 1),
                  row_aft = ifelse(row + 1 > nrow(x), NA, row + 1),
                  pl_team = x$pl_team[row],
                  poss_bef = x$poss_team[row_bef])
  naDat$poss_aft <-
    with(naDat, ifelse(is.na(row_aft), NA, x$poss_team[row_aft]))
    
  ## if previous possession is known and EITHER:
  ##   possession after is known "iso, no poss change"
  ##   OR
  ##   pl_team != possession before and pl_code == 'F' "simple defensive foul"
  ## possession can be set to possession before
  copy_poss_before <- with(naDat,
                           !is.na(poss_bef) & 
                             (!is.na(poss_aft) |
                                (poss_bef != pl_team & pl_code == 'F')))
  rows_to_fill <- naDat$row[copy_poss_before]
  x$poss_team[rows_to_fill] <- x$poss_team[rows_to_fill - 1] 
  x
}
game_play <- ddply(game_play, ~ point, infer_possession)
poss_ok <- sum(!is.na(game_play$poss_team))
message("  after first infer, possession established for ", poss_ok, "/",
        n, " = ", round(100 * poss_ok/n, 2), "% of game play events")

## address SO/SI clumps
na_and_sub <- with(game_play, is.na(poss_team) & grepl("S[OI]", pl_code))
if(any(na_and_sub)) {
  naDat <- data.frame(row = which(na_and_sub))
  naDat <- mutate(naDat,
                  diff = c(1, diff(row)),
                  start = diff != 1,
                  clump = cumsum(start) + 1,
                  pl_code = game_play$pl_code[row],
                  row_bef = ifelse(row - 1 < 1, NA, row - 1),
                  row_aft = ifelse(row + 1 > n, NA, row + 1))
  ## get the poss_team before and after individual SO/SI events
  naDat$poss_bef <-
    with(naDat, ifelse(is.na(row_bef), NA,
                       as.character(game_play$poss_team[row_bef])))
  naDat$poss_aft <-
    with(naDat, ifelse(is.na(row_aft), NA,
                       as.character(game_play$poss_team[row_aft])))
  ## within clump, concatenate flanking poss_team, excluding NAs; reduce to
  ## unique
  naDat <- ddply(naDat, ~ clump, function(z) {
    flanking_poss <- with(z, c(poss_bef, poss_aft))
    flanking_poss <- unique(flanking_poss[!is.na(flanking_poss)])
    ## if there is exactly one value, USE IT
    if(length(flanking_poss) == 1) {
      check_it <- FALSE
      return(data.frame(z, poss = flanking_poss))
    } else if(length(flanking_poss) == 2) { ## if two values, go with after
      check_it <- TRUE
      return(data.frame(z, poss = with(z, poss_aft[!is.na(poss_aft)])))
    } else {
      check_it <- TRUE
      return(z)
    }
  })
  game_play$poss_team[naDat$row] <- naDat$poss
  message("  check the possession resolution for these SO/SI clumps:")
  print(game_play[naDat$row, ])
  poss_ok <- sum(!is.na(game_play$poss_team))
  message("  after addressing SO/SIs, possession established for ", poss_ok, "/",
          n, " = ", round(100 * poss_ok/n, 2), "% of game play events")
}

## if NAs remain in poss_team, try to infer once more
poss_ok <- sum(!is.na(game_play$poss_team))
if(poss_ok < n) {
  game_play <- ddply(game_play, ~ point, infer_possession)
  poss_ok <- sum(!is.na(game_play$poss_team))
  message("  after second and LAST infer, possession established for ",
          poss_ok, "/", n, " = ", round(100 * poss_ok/n, 2),
          "% of game play events")
}

out_dir <- file.path("..", "games", game, "06_possess-game")
if(!file.exists(out_dir)) dir.create(out_dir)

message("  ", nrow(game_play), " rows of resolved game play will be written")

out_file <- file.path(out_dir, paste0(game, "_gameplay-resolved.tsv"))
write.table(game_play, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
#message("wrote ", out_file)

## new variable scor_team records who scored
just_goals <-
  subset(game_play, grepl("L*G", pl_code), select = c(point, pl_team))
just_goals <- rename(just_goals, c("pl_team" = "scor_team"))
point_info <- suppressMessages(join(point_info, just_goals))

## new variables for individual team scores
jFun <- function(x) {x[is.na(x)] <- FALSE; x}
point_info$teamOne <- cumsum(jFun(with(point_info, scor_team == jTeams[1])))
point_info$teamTwo <- cumsum(jFun(with(point_info, scor_team == jTeams[2])))
point_info <-
  rename(point_info, c("teamOne" = jTeams[1], "teamTwo" = jTeams[2]))

latest_score <- point_info[nrow(point_info), jTeams]

message("  ", nrow(point_info), " resolved points to be written")
message("  ",
        paste(paste(names(latest_score), latest_score[1, ], sep = ": "),
              collapse = " "))
message("  ", Sys.time(), "\n")

out_file <- file.path(out_dir, paste0(game, "_points-resolved.tsv"))
write.table(point_info, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
#message("wrote ", out_file)

# write most recent (usually the final) score to yaml for use in, e.g., index
score_yaml <-
  file.path("..", "games", game, paste0(game, "_at-last-point.yaml"))
writeLines(as.yaml(point_info[nrow(point_info), ]), score_yaml)
#message("wrote ", score_yaml)
