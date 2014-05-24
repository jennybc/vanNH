library(plyr)

## when run in batch mode, provide game identifier on command line
options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  #game <- "2014-04-12_vanNH-at-pdxST"
  game <- "2014-04-20_sfoDF-at-vanNH"
  #game <- "2014-05-10_seaRM-at-vanNH"
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

game_dir <- file.path("..", "games", game, "03_concatGoogleExtract")

in_file <- file.path(game_dir, paste0(game, "_gameplay-raw.tsv"))
game_play <- read.delim(in_file, stringsAsFactors = FALSE)
str(game_play)

in_file <- file.path(game_dir, paste0(game, "_points-raw.tsv"))
point_info <- read.delim(in_file, stringsAsFactors = FALSE)
str(point_info)

## add short version of the teams
mlu_teams <- read.delim(file.path("..", "data", "mlu-teams.tsv"))
point_info$pullTeam <-
  factor(mlu_teams$team[match(point_info$Pulling.team, mlu_teams$longName)],
         levels = jTeams)

## replace NAs in game_play$Offense and game_play$Defense with ""
jFun <- function(x) {x[is.na(x)] <- ""; return(x)}
game_play <-
  transform(game_play, Offense = jFun(Offense), Defense = jFun(Defense))

## formalize this later ... I want to see NO LETTERS here!
# table(substr(game_play$Defense, 1, 1))
# table(substr(game_play$Offense, 1, 1))
#       1   2   3   4   5   6   7   8   9 
# 415  27  40  23  19   5  17  13  72   9 

## separate, e.g. 81D into 81 and D
jFun <- function(x) {
  tmp <- regexpr("[0-9]+", x)
  return(substring(x, tmp, tmp + attr(tmp, "match.length") - 1))
}
game_play <-
  transform(game_play, oNum = I(jFun(Offense)), dNum = I(jFun(Defense)))

jFun <- function(x) {
  tmp <- regexpr("[a-zA-Z]+", x)
  return(substring(x, tmp, tmp + attr(tmp, "match.length") - 1))
}
game_play <-
  transform(game_play, oCode = I(jFun(Offense)), dCode = I(jFun(Defense)))

## make sure all codes are upper case
game_play <- transform(game_play,
                       oCode = toupper(oCode), dCode = toupper(dCode))

## add an event counter within point
game_play$event <- ddply(game_play['point'], ~ point, row)[ , 2]

## add variables to hold team on O and on D
game_play$oTeam <- game_play$dTeam <- factor(NA, levels = jTeams)
game_play$dTeam <- rep(point_info$pullTeam,
                       aggregate(event ~ point, game_play, max)$event)
## populate oTeam based on dTeam
get_opponent <- function(x) {
  jLevels <- levels(x)
  x <- ifelse(unclass(x) == 1, 2, 1)
  return(jLevels[x])
}
game_play$oTeam <- get_opponent(game_play$dTeam)

## find rows where a play is recorded for both the O and the D
## insert a row to make two rows
## first row will hold O info, second will hold D

## originally this happened for some D and F events in 2014-04-12_vanNH-at-pdxST
## specifically, these points:
## D: 1, 13, 22, 23, 29, 30, 32, 43
## F: 6, 9, 20, 22, 24, 35
## but it has been eliminated earlier now
## this code needs to be refactored
jFun <- function(x) {
  fix_me <- which(with(x, dNum != "" & oNum != ""))
  needs_fix <- length(fix_me) > 0
  while(needs_fix) {
    fix_this <- fix_me[1]
    x <- x[rep(1:nrow(x), ifelse(1:nrow(x) %in% fix_this, 2, 1)), ]
    x[fix_this, c('Defense', 'dNum', 'dCode')] <- ''
    x[fix_this + 1, c('Offense', 'oNum', 'oCode')] <- ''
    fix_me <- which(with(x, dNum != "" & oNum != ""))
    needs_fix <- length(fix_me) > 0
  } 
  x$event <- 1:nrow(x)
  return(x)
}
game_play <- ddply(game_play, ~ point, jFun)
#which(with(game_play, dNum != "" & oNum != ""))

## tidy up
game_play <- with(game_play,
                  data.frame(point, event,
                             oTeam, dTeam,
                             oNum, oCode, dNum, dCode))

## determine who's on O vs. D, at the event level
## ... leave this for another day
## let's just figure out who scored and assign the assist
jFun <- function(x) {
  n <- nrow(x)
  jCodes <- c(x$oCode[n], x$dCode[n])
  is_a_score <- 'G' %in% jCodes | 'LG' %in% jCodes
  if(is_a_score) {
    if(x$dNum[n] != '') {
      scTeam <- x$dTeam[n]
      if(x$dNum[n - 1] == '') { # assister was fouled by the defense
        x$dCode[n - 2] <- 'A'
      } else {
        x$dCode[n - 1] <- 'A'
      }
    } else {
      scTeam <- x$oTeam[n]
      if(x$oNum[n - 1] == '') { # assister was fouled by the defense
        x$oCode[n - 2] <- 'A'
      } else {
        x$oCode[n - 1] <- 'A'
      }
    }
  } else {
    scTeam <- NA
  }
  return(data.frame(x, scTeam))
}
game_play <- ddply(game_play, ~ point, jFun)

out_dir <- file.path("..", "games", game, "04_cleanedGame")
if(!file.exists(out_dir)) dir.create(out_dir)

out_file <- file.path(out_dir, paste0(game, "_gameplay-clean.tsv"))
write.table(game_play, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

## add who scored to point_info, among other things
point_info$scTeam <- game_play$scTeam[game_play$event == 1]
point_info <-
  with(point_info,
       data.frame(point, Period, Clock.before.point, Clock.after.point,
                  pullTeam, scTeam))
jLevels <- levels(point_info$pullTeam)
jFun <- function(x) {x[is.na(x)] <- FALSE; x}
point_info$teamOne <- cumsum(jFun(with(point_info, scTeam == jLevels[1])))
point_info$teamTwo <- cumsum(jFun(with(point_info, scTeam == jLevels[2])))
point_info <-
  rename(point_info, c("teamOne" = jLevels[1], "teamTwo" = jLevels[2]))

out_file <- file.path(out_dir, paste0(game, "_points-clean.tsv"))
write.table(point_info, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)
