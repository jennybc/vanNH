library(plyr)
library(methods) # needed so testthat works when I call this via RScript
library(yaml)
library(stringr)

## when run in batch mode, provide game identifier on command line
options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  #game <- "2014-04-12_vanNH-at-pdxST"
  #game <- "2014-04-20_sfoDF-at-vanNH"
  #game <- "2014-04-26_vanNH-at-sfoDF"
  game <- "2014-05-10_seaRM-at-vanNH"
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

## eliminate game play rows for which recvRaw == pullRaw == ''
nBefore <- nrow(game_play)
jFun <- function(z) {
  n <- nrow(z)
  offset_index <- c(2:n, n) # get entry from row below; last element: get self
  z <- mutate(z, both = paste0(recvRaw, pullRaw),
              bothOffset = both[offset_index],
              is_empty = both == bothOffset & both == '')
  return(subset(z, !is_empty, select = c(point, recvRaw, pullRaw))  )
}
game_play <- ddply(game_play, ~ point, jFun)
nAfter <- nrow(game_play)
nDiff <- nBefore - nAfter
if(nDiff != 0) {
  message(nDiff, " out of ", nBefore,
          " rows eliminated; game play cells were empty")
}

## remove leading single quote from recvRaw and pullRaw, if present
## I have seen this happen for seaRM player 00
## seems to arise from the "extract data from Google spreadsheet" step
jFun <- function(x) gsub("'","", x)
game_play[c('pullRaw', 'recvRaw')] <-
  colwise(jFun)(game_play[c('pullRaw', 'recvRaw')])

## game play data should be empty, start with a ?, start with a digit, or be
## [TO|to]
jFun <- function(x) {
  x == "" | grepl("^[\\?\\d]", x, perl = TRUE) | 
    grepl("TO", x, ignore.case = TRUE)
}
code_seems_valid <- colwise(jFun)(game_play[c('pullRaw', 'recvRaw')])
weird_code <- !apply(code_seems_valid, 1, all)
if(any(weird_code)) {
  message("these rows have game play that's not empty, not a TO, yet doesn't start with a digit")
  game_play[weird_code, ]
}
  
## separate raw game play into a number and a code
## e.g. 81D into 81 and D
jFun <- function(x) {
  ret_val <- str_extract(x, perl("^[\\?\\d]+"))
  ret_val[is.na(ret_val)] <- ""
  return(ret_val)
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
game_play <-
  transform(game_play,
            recvCode = toupper(recvCode), pullCode = toupper(pullCode))

## add an event counter within point
game_play$event <- ddply(game_play['point'], ~ point, row)[ , 2]

## detect pulls with no valid pull code
pull_codes <- c('P', 'OBP')
no_explicit_pull <- with(game_play, event == 1 & !(pullCode %in% pull_codes))
if(any(no_explicit_pull)) {
  message(paste("ALERT: point(s) with NO explicit pull code (P, OBP)"))
  print(game_play[no_explicit_pull, ])
}

## detect pickups off the pull with no valid PU code
pickup_codes <- c('PU', 'PUA')
no_explicit_pu <- with(game_play, event == 2 & !(recvCode %in% pickup_codes))
if(any(no_explicit_pu)) {
  message(paste("ALERT: point(s) with NO explicit pickup off the pull code (PU, PUA)"))
  print(game_play[no_explicit_pu, ])
}

## TO DO?
## detect points with no explicit goal and make sure it's at end of a period
# goal_regexp <- "L*G"
# row_of_last_event <- daply(game_play, ~ point, function(x) x$event[nrow(x)])
# ddply(game_play, ~ point, function(x) x[nrow(x), ])

## tidy up
keeper_vars <- c("point", "event",
                 "pullRaw", "pullNum", "pullCode",
                 "recvRaw", "recvNum", "recvCode")
game_play <- game_play[keeper_vars]

## function to find double game play rows
find_double_game_plays <-
  function(z) with(z, which(pullNum != "" & recvNum != ""))

## identify rows with game play recorded for both teams
fix_me <- find_double_game_plays(game_play)
message("rows with game play recorded for both teams")
game_play[fix_me, ]

## split double game play rows into two separate rows
## the only tricky thing is deciding the order
## big picture: try to keep the O play first, D play second
jFun <- function(x) {
  offense_codes <- c('', 'PU', 'L', 'G', 'LG', 'TO', 'LTO')
  foul_codes <- c('F', 'VP')
  sub_codes <- c('SO', 'SI')
  fix_me <- find_double_game_plays(x)
  needs_fix <- length(fix_me) > 0
  while(needs_fix) {
    fix_this <- fix_me[1]
    codes <- c(recvCode = x[fix_this, "recvCode"],
               pullCode = x[fix_this, "pullCode"])
    
    ## most common situation: one code is from offense_codes, other from
    ## foul_codes
    if(all(codes %in% offense_codes == !(codes %in% foul_codes))) {
      if(names(codes)[codes %in% offense_codes] == "recvCode") {
        jOrder <- c("recv", "pull")
      } else {
        jOrder <- c("pull", "recv")
      }
    } else { # just pick an order
      jOrder <- c("pull", "recv")
      if(!all(grepl("S[OI]+", codes))) {
        message(paste("Row", fix_this, "of point", x$point[1],
                      "indicates events for both teams\n, but it's a novel code",
                      "combination. LOOK AT THIS DATA!"))
        print(x[fix_this + (-1:1), ])
      }      
    }
    
    ## duplicate the affected row
    x <- x[rep(1:nrow(x), ifelse(1:nrow(x) %in% fix_this, 2, 1)), ]
    ## 'empty' out raw, num, and code for either recv or pull
    x[fix_this, paste0(jOrder[2], c('Raw', 'Num', 'Code'))] <- ''
    x[fix_this + 1, paste0(jOrder[1], c('Raw', 'Num', 'Code'))] <- ''
        
    ## update the to do list
    fix_me <- find_double_game_plays(x)
    needs_fix <- length(fix_me) > 0
  } 
  x$event <- 1:nrow(x)
  return(x)
}
game_play <- ddply(game_play, ~ point, jFun)

## do any double game play rows remain?
fix_me <- find_double_game_plays(game_play)
if(length(fix_me) > 0) {
  message("double game play rows we are not prepared to address and that remain")
  game_play[fix_me, ]
} else {
  message("no double game play rows remain")
}

## I want to have NO double game play rows.
## here are some that still remain.

# 2014-06-07_seaRM-at-vanNH point 25
# pullTeam recvTeam event pullNum pullCode recvNum recvCode scorTeam
# vanNH    seaRM    16      13       PU                     vanNH
# vanNH    seaRM    17      18                              vanNH
# vanNH    seaRM    18      37       TO       ?       VP    vanNH
# vanNH    seaRM    19      89       PU                     vanNH
# vanNH    seaRM    20      91                              vanNH

## function to get the "other" team
get_opponent <- function(x) {
  jLevels <- levels(x)
  x <- ifelse(unclass(x) == 1, 2, 1)
  return(factor(jLevels[x], levels = jLevels))
}

## add variables to hold pulling and receiving teams 
game_play$pullTeam <-
  point_info$pullTeam[match(game_play$point, point_info$point)]
game_play$recvTeam <- get_opponent(game_play$pullTeam)

## drop recvRaw, pullRaw
game_play <- subset(game_play, select = -c(recvRaw, pullRaw))

## determine which team scored and assign the assist
jFun <- function(x) {
  n <- nrow(x)
  point <- x$point[1]
  is_a_goal <- function(x) grepl("L*G", x)
  goal_ind <- as.matrix(colwise(is_a_goal)(x[c('recvCode', 'pullCode')]))
  if(sum(laply(goal_ind, sum)) > 1) {
    message("point: ", point, " ... more than one goal code detected!")
  }
  goal_row <- which(aaply(goal_ind, 1, any))
  if(length(goal_row) > 0) {
    ## I do it this way in case there is foul on the goal catch, which
    ## means the n-th event is not the goal itself    
    goal_row <- max(goal_row)
    codes <- c(recvCode = x[goal_row, "recvCode"],
               pullCode = x[goal_row, "pullCode"])
    sc_team_rel <- substr(names(codes)[is_a_goal(codes)], 1, 4) # pull or recv?
    sc_team_abs <-                                       # e.g. vanNH or seaRM?
      switch(sc_team_rel, recv = as.character(x$recvTeam[1]),
             pull = as.character(x$pullTeam[1]), NA)
    sc_team_num <- x[[paste0(sc_team_rel, "Num")]]  # all nums for scoring team
    sc_team_num <- sc_team_num[-(goal_row:n)]       # peel the goal off the end
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
  return(data.frame(x, scorTeam = sc_team_abs))
}
game_play <- ddply(game_play, ~ point, jFun)

## tidy up
keeper_vars <- c("point", "pullTeam", "recvTeam", "event",
                 "pullNum", "pullCode", "recvNum", "recvCode", "scorTeam")
game_play <- game_play[keeper_vars]

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

score_yaml <-
  file.path("..", "games", game, paste0(game, "_at-last-point.yaml"))
## workaround because as.yaml() segfaults if a factor contains an NA
## scorTeam is a factor and will be NA for any point that ends due to time
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
