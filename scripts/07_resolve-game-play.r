library(plyr)
library(yaml)

## when run in batch mode, provide game identifier on command line
options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  game <- "2014-04-12_vanNH-at-pdxST"
  #game <- "2014-04-20_sfoDF-at-vanNH"
  #game <- "2014-04-26_vanNH-at-sfoDF"
  #game <- "2014-05-10_seaRM-at-vanNH"
  #game <- "2014-05-17_vanNH-at-sfoDF"
  #game <- "2014-05-24_pdxST-at-vanNH"
  #game <- "2014-05-31_vanNH-at-seaRM"
  #game <- "2014-06-07_seaRM-at-vanNH"
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
game_play <- read.delim(in_file, stringsAsFactors = FALSE)
#str(game_play)

game_dir <- file.path("..", "games", game, "03_concatGoogleExtract")
in_file <- file.path(game_dir, paste0(game, "_points-raw.tsv"))
point_info <- read.delim(in_file, stringsAsFactors = FALSE)
#str(point_info)

## replace long team names with my official short versions
mlu_teams <- read.delim(file.path("..", "data", "mlu-teams.tsv"))
point_info$pull_team <-
  factor(mlu_teams$team[match(point_info$Pulling.team, mlu_teams$longName)],
         levels = jTeams)
point_info$Pulling.team <- NULL
## rename vars
point_info <-
  rename(point_info, c("Period" = "period", "Clock.before.point" = "clk_before",
         "Clock.after.point" = "clk_after"))

## replace NAs in game_play$pullNum and game_play$recvNum with ""
jFun <- function(x) {x[is.na(x)] <- ""; return(x)}
game_play <-
  transform(game_play, pullNum = jFun(pullNum), recvNum = jFun(recvNum))

## collapse recvCode/recvNum/pullCode/pullNum into pl_team, pl_pnum, pl_code
jFun <- function(x) {
  point <- x$point[1]  
  double_code <- with(x, pullCode != '' & recvCode != '')
  if(any(double_code)) {
    stop("point ", point, ": rows with game play from both teams ... exiting")
    x[double_code, ]
  }
  
  x$play_by_recv_team <- x$pullNum == ''
  x$pl_team <- with(x, ifelse(play_by_recv_team, "recv_team", "pull_team"))
  x$pl_pnum <- with(x, ifelse(play_by_recv_team, recvNum, pullNum))
  x$pl_code <- with(x, ifelse(play_by_recv_team, recvCode, pullCode))
  
  x[c('point', 'event', 'pl_team', 'pl_pnum', 'pl_code')]
}
game_play <- ddply(game_play, ~ point, jFun)

## function to get the "other" team
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
                                    "recv_team" = as.character (recv_team)))
  x$pl_team <- factor(x$pl_team, levels = jTeams)
  return(data.frame(period = period, x))
}
game_play <- ddply(game_play, ~ point, jFun)

## assign the assist
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
    assist_row <- rev(which(with(x, pl_team == sc_team)))[2]
    ## in my experience, existing code can be '', 'L', 'PU'
    assist_code <- x$pl_code[assist_row]
    if(grepl("A$", assist_code)) {
      message(paste("ALERT: point", x$point[1], "has an explicit assist (A)\n"))
    } else {
      x$pl_code[assist_row] <- paste0(assist_code, 'A')
    }
  }
  return(x)
}
game_play <- ddply(game_play, ~ point, jFun)

out_dir <- file.path("..", "games", game, "07_resolvedGame")
if(!file.exists(out_dir)) dir.create(out_dir)

out_file <- file.path(out_dir, paste0(game, "_gameplay-resolved.tsv"))
write.table(game_play, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

## new variable scor_team records who scored
just_goals <-
  subset(game_play, grepl("L*G", pl_code), select = c(point, pl_team))
just_goals <- rename(just_goals, c("pl_team" = "scor_team"))
point_info <- join(point_info, just_goals)

## new variables for individual team scores
jFun <- function(x) {x[is.na(x)] <- FALSE; x}
point_info$teamOne <- cumsum(jFun(with(point_info, scor_team == jTeams[1])))
point_info$teamTwo <- cumsum(jFun(with(point_info, scor_team == jTeams[2])))
point_info <-
  rename(point_info, c("teamOne" = jTeams[1], "teamTwo" = jTeams[2]))

out_file <- file.path(out_dir, paste0(game, "_points-resolved.tsv"))
write.table(point_info, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

# write most recent (usually the final) score to yaml for use in, e.g., index
score_yaml <-
  file.path("..", "games", game, paste0(game, "_at-last-point.yaml"))
## workaround because as.yaml() segfaults if a factor contains an NA
## scor_team is a factor and will be NA for any point that ends due to time
yaml_fodder <- as.list(point_info[nrow(point_info), ])
yaml_fodder <- lapply(yaml_fodder, function(x) {
  if(is.factor(x) & any(is.na(x))) {
    return(as.character(x))
  } else {
    return(x)
  }
})
writeLines(as.yaml(yaml_fodder), score_yaml)
message("wrote ", score_yaml)

