library(plyr)
library(methods)
library(testthat)

## when run in batch mode, provide game identifier on command line
options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  #game <- "2014-04-12_vanNH-at-pdxST"
  #game <- "2014-04-20_sfoDF-at-vanNH"
  #game <- "2014-05-10_seaRM-at-vanNH"
  #game <- "2014-05-24_pdxST-at-vanNH"
  game <- "2014-05-31_vanNH-at-seaRM"
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

game_dir <- file.path("..", "games", game, "03_concatGoogleExtract")

in_file <- file.path(game_dir, paste0(game, "_gameplay-raw.tsv"))
game_play <- read.delim(in_file, stringsAsFactors = FALSE)
#str(game_play)

in_file <- file.path(game_dir, paste0(game, "_points-raw.tsv"))
point_info <- read.delim(in_file, stringsAsFactors = FALSE)
#str(point_info)

## add short version of the teams
mlu_teams <- read.delim(file.path("..", "data", "mlu-teams.tsv"))
point_info$pullTeam <-
  factor(mlu_teams$team[match(point_info$Pulling.team, mlu_teams$longName)],
         levels = jTeams)

## Offense and Defense are misleading variable names
## rename to suggest they record actions by the "receiving" and "pulling" teams,
## respectively
game_play <- rename(game_play, c("Offense" = "recvRaw", "Defense" = "pullRaw"))

## replace NAs in game_play$recvRaw and game_play$pullRaw with ""
jFun <- function(x) {x[is.na(x)] <- ""; return(x)}
game_play <-
  transform(game_play, recvRaw = jFun(recvRaw), pullRaw = jFun(pullRaw))

## remove leading single quote from recvRaw and pullRaw
## I have seen this happen for seaRM player 00
jFun <- function(x) gsub("'","", x)
game_play[c('pullRaw', 'recvRaw')] <-
  colwise(jFun)(game_play[c('pullRaw', 'recvRaw')])

## game play data should be empty or start with a digit
jFun <- function(x) {
  grepl("^\\d", x, perl = TRUE) | x == ""
}
code_seems_valid <- colwise(jFun)(game_play[c('pullRaw', 'recvRaw')])
## only valid exception is TO for timeout
expect_true(all(game_play$pullRaw[!code_seems_valid$pullRaw] == "TO"))
expect_true(all(game_play$recvRaw[!code_seems_valid$recvRaw] == "TO"))

## separate, e.g. 81D into 81 and D
jFun <- function(x) {
  tmp <- regexpr("[0-9]+", x)
  return(substring(x, tmp, tmp + attr(tmp, "match.length") - 1))
}
game_play <-
  transform(game_play, recvNum = I(jFun(recvRaw)), pullNum = I(jFun(pullRaw)))

jFun <- function(x) {
  tmp <- regexpr("[a-zA-Z]+", x)
  return(substring(x, tmp, tmp + attr(tmp, "match.length") - 1))
}
game_play <-
  transform(game_play, recvCode = I(jFun(recvRaw)), pullCode = I(jFun(pullRaw)))

## make sure all codes are upper case
game_play <- transform(game_play,
                       recvCode = toupper(recvCode),
                       pullCode = toupper(pullCode))

## add an event counter within point
game_play$event <- ddply(game_play['point'], ~ point, row)[ , 2]

## function to get the "other" team
get_opponent <- function(x) {
  jLevels <- levels(x)
  x <- ifelse(unclass(x) == 1, 2, 1)
  return(jLevels[x])
}

## add variables to hold pulling and receiving teams 
game_play$pullTeam <-
  point_info$pullTeam[match(game_play$point, point_info$point)]
game_play$recvTeam <- get_opponent(game_play$pullTeam)

## tidy up
game_play <- with(game_play,
                  data.frame(point, pullTeam, recvTeam, event,
                             pullRaw, pullNum, pullCode,
                             recvRaw, recvNum, recvCode))

## detect pulls with no valid pull code
pull_codes <- c('P', 'OBP')
no_explicit_pull <- with(game_play, event == 1 & !(pullCode %in% pull_codes))
if(any(no_explicit_pull)) {
  message(paste("ALERT: point(s) with NO explicit pull code (P, OBP)"))
  print(game_play[no_explicit_pull, ])
}

## find rows where a play is recorded for both the O and the D
## when it's a defensive foul: insert a row to make two rows
## first row will hold O info, second will hold D
jFun <- function(x) {
  fix_me <- which(with(x, pullNum != "" & recvNum != ""))
  needs_fix <- length(fix_me) > 0
  while(needs_fix) {
    fix_this <- fix_me[1]
    codes <- c(recvCode = x[fix_this, "recvCode"],
               pullCode = x[fix_this, "pullCode"])
  
    if(sum(grepl("F", codes)) == 1) {
      is_a_foul <- TRUE
      } else {
        is_a_foul <- FALSE
      }
    
    if(sum(grepl("S[OI]", codes)) == 2) {
      is_a_dual_sub <- TRUE
    } else {
      is_a_dual_sub <- FALSE
    }
    
    if(!is_a_foul & !is_a_dual_sub) {
      print(x[fix_this + (-1:1), ])
      stop(paste("Row", fix_this, "of point", x$point[1],
                 'indicates events for both teams, yet neither is a foul, i.e. carries code F\n'))
    }
    
    x <- x[rep(1:nrow(x), ifelse(1:nrow(x) %in% fix_this, 2, 1)), ]
    if(is_a_dual_sub | x[fix_this, "recvCode"] == "F") {
      x[fix_this, c('recvRaw', 'recvNum', 'recvCode')] <- ''
      x[fix_this + 1, c('pullRaw', 'pullNum', 'pullCode')] <- ''
    } else {
      x[fix_this + 1, c('recvRaw', 'recvNum', 'recvCode')] <- ''
      x[fix_this, c('pullRaw', 'pullNum', 'pullCode')] <- ''
    }
    fix_me <- which(with(x, pullNum != "" & recvNum != ""))
    needs_fix <- length(fix_me) > 0
  } 
  x$event <- 1:nrow(x)
  return(x)
}
game_play <- ddply(game_play, ~ point, jFun)
#which(with(game_play, pullNum != "" & recvNum != ""))

## drop recvRaw, pullRaw
game_play <- subset(game_play, select = -c(recvRaw, pullRaw))

## determine which team scored and assign the assist
jFun <- function(x) {
  n <- nrow(x)
  codes <- c(recvCode = x[n, "recvCode"], pullCode = x[n, "pullCode"])
  is_a_goal <- grepl("L*G", codes)
  if(any(is_a_goal)) {
    sc_team_rel <- substr(names(codes)[is_a_goal], 1, 4) # pull or recv?
    sc_team_abs <-                                       # e.g. vanNH or seaRM?
      switch(sc_team_rel, recv = as.character(x$recvTeam[1]),
             pull = as.character(x$pullTeam[1]), NA)
    sc_team_num <- x[[paste0(sc_team_rel, "Num")]]  # all nums for scoring team
    sc_team_num <- sc_team_num[-n]                  # peel the goal off the end
    ## I do it this way in case there is an intervening defensive foul, which
    ## means the (n-1)-th event is not the assist
    assist_row <- max(which(sc_team_num != ''))
    ## in my experience, existing code can be '', 'L', 'PU'
    assist_code <- x[assist_row, paste0(sc_team_rel, "Code")]
    if(grepl("A$", assist_code)) {
      message(paste("ALERT: point", x$point[1], "has an explicit assist (A)\n"))
    } else {
      x[assist_row, paste0(sc_team_rel, "Code")] <- paste0(assist_code, 'A')
    }
  } else {
    sc_team_abs <- NA
  }
  #print(x[(n - 2):n, ])
  #cat("scoring team:", sc_team_abs, "\n\n")
  return(data.frame(x, scorTeam = sc_team_abs))
}
game_play <- ddply(game_play, ~ point, jFun)

## determine who's on O vs. D, at the event level
## ... leave this for another day

out_dir <- file.path("..", "games", game, "04_cleanedGame")
if(!file.exists(out_dir)) dir.create(out_dir)

out_file <- file.path(out_dir, paste0(game, "_gameplay-clean.tsv"))
write.table(game_play, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

## add who scored to point_info, among other things
point_info$scorTeam <- game_play$scorTeam[game_play$event == 1]
point_info <-
  with(point_info,
       data.frame(point, Period, Clock.before.point, Clock.after.point,
                  pullTeam, scorTeam))
jLevels <- levels(point_info$pullTeam)
jFun <- function(x) {x[is.na(x)] <- FALSE; x}
point_info$teamOne <- cumsum(jFun(with(point_info, scorTeam == jLevels[1])))
point_info$teamTwo <- cumsum(jFun(with(point_info, scorTeam == jLevels[2])))
point_info <-
  rename(point_info, c("teamOne" = jLevels[1], "teamTwo" = jLevels[2]))

out_file <- file.path(out_dir, paste0(game, "_points-clean.tsv"))
write.table(point_info, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

