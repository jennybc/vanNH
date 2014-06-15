## a one-off script to help develop my method for deciding who's got possession
## based on game play

library(plyr)

games <- c("2014-04-12_vanNH-at-pdxST", "2014-04-20_sfoDF-at-vanNH",
           "2014-04-26_vanNH-at-seaRM", "2014-05-10_seaRM-at-vanNH",
           "2014-05-17_vanNH-at-sfoDF", "2014-05-24_pdxST-at-vanNH",
           "2014-05-31_vanNH-at-seaRM", "2014-06-07_seaRM-at-vanNH")
game_file <- file.path("..", "games", games, "07_resolvedGame",
                       paste0(games, "_gameplay-resolved.tsv"))
names(game_file) <- games
gpDat <- ldply(game_file, function(gg) {
  read.delim(gg,
             colClasses = list(pl_team = "character",
                               pl_pnum = "character",
                               pl_code = "character"))
}, .id = "game")
str(gpDat) # 5028 obs. of  7 variables

## define some code groups
goal_codes <- c('G', 'LG')
assist_codes <- c('A', 'LA', 'PUA')
more_offense_codes <- c('', 'PU', 'L', 'TD', 'VST', 'VTT', 'TO')
offense_codes <- c(more_offense_codes, goal_codes, assist_codes)
d_codes <- c('D', 'HB', 'FB')

## function to get the "other" team
get_opponent <- function(x) {
  jLevels <- levels(x)
  x <- ifelse(unclass(x) == 1, 2, 1)
  return(factor(jLevels[x], levels = jLevels))
}

## informal check for PUs after the pull ... god, this should be ok by now
table(with(gpDat, pl_code[event == 2]))
## PU PUA 
## 351   2 
## YES!

## function to display gameplay around specific rows
show_game_play <- function(the_rows) {
  rows_i_want <- sort(unique(unlist(lapply(the_rows,
                                           function(x)
                                             seq(from = x - 2, to = x + 2)))))
  rows_i_want <- rows_i_want[rows_i_want > 0 & rows_i_want < nrow(gpDat) + 1]
  foo <- data.frame(rows_i_want, yo = c(1, diff(rows_i_want)))
  foo$start <- foo$yo != 1
  foo$clump <- cumsum(foo$start) + 1
  tmp <- dlply(foo, ~ clump, function(x) gpDat[x$rows_i_want, ])
  tmp
}

## determine who possess the disc: the easy stuff, i.e. possession can be
## determined directly from the event code
jFun <- function(x) {
  
  game <- as.character(x$game)[1]
  tmp <- strsplit(game, split = "_")[[1]]
  tmp <- strsplit(tmp[2], split = "-")[[1]]
  away_team <- tmp[1]
  home_team <- tmp[3]
  jTeams <- sort(c(away_team, home_team))
  x <- transform(x, pl_team = factor(pl_team, levels = jTeams))
    
  x$poss_team <- factor(NA, levels = jTeams)
  x$poss_team[1:2] <- x$pl_team[2]
  play_is_offensive <- x$pl_code %in% offense_codes
  x$poss_team[play_is_offensive] <- x$pl_team[play_is_offensive]
  play_is_defensive <- x$pl_code %in% d_codes
  if(any(play_is_defensive))
    x$poss_team[play_is_defensive] <-
    get_opponent(x$pl_team[play_is_defensive])
  x <- transform(x, pl_team = as.character(pl_team),
                 poss_team = as.character(poss_team))
  return(x)
}
gpDat <- ddply(gpDat, ~ game + point, jFun)

## how'm I doing?
poss_tab <-
  as.data.frame(addmargins(table(gpDat$poss_team, useNA = "always",
                                 dnn = "pl_code")))
poss_tab
#   pl_code Freq
# 1   pdxST  727
# 2   seaRM 1267
# 3   sfoDF  561
# 4   vanNH 2361
# 5    <NA>  112
# 6     Sum 5028

## proportion of plays with NA for poss_team
with(poss_tab, Freq[is.na(pl_code)] / Freq[pl_code %in% "Sum"])
# [1] 0.02227526

## what are the codes when poss is unknown?
tricky_codes <-
  as.data.frame(addmargins(table(with(gpDat, pl_code[is.na(gpDat$poss_team)]),
                                 useNA = "always", dnn = "pl_code")))
tricky_codes
#   pl_code Freq
# 1       F   82
# 2      SI   14
# 3      SO   14
# 4      VP    2
# 5    <NA>    0
# 6     Sum  112

## now we have to do some look ahead / behind to determine possession :(
naDat <- data.frame(row = which(is.na(gpDat$poss_team)))
naDat <- mutate(naDat,
                pl_code = gpDat$pl_code[row],
                game = gpDat$game[row],
                row_bef = row - 1,
                row_aft = row + 1,
                pb = gpDat$point[row_bef],
                point = gpDat$point[row],
                pa = gpDat$point[row_aft],
                pl_team = gpDat$pl_team[row],
                possb = gpDat$poss_team[row_bef],
                possa = gpDat$poss_team[row_aft])
str(naDat) # 112 obs. of  11 variables:

## if previous possession is known and EITHER:
##   possession after is known and pb = point = pa "iso, no poss change"
##   OR
##   pl_team != possession before and pl_code == 'F' "simple defensive foul"
## possession can be set to possession before
copy_poss_before <- with(naDat,
                         !is.na(possb) & 
                           ((!is.na(possa) & pb == pa) |
                              (possb != pl_team & pl_code == 'F')))
summary(copy_poss_before)
#    Mode   FALSE    TRUE    NA's 
# logical      29      83       0 
rows_to_fill <- naDat$row[copy_poss_before]
gpDat$poss_team[rows_to_fill] <- gpDat$poss_team[rows_to_fill - 1] 
naDat <- subset(naDat, !(row %in% rows_to_fill))

## what's left?
str(naDat) # 29 obs. of  11 variables:
(tmp <- show_game_play(naDat$row))
## it's in 12 'clumps'
summary(laply(tmp, function(x) any(grepl("S[OI]", x$pl_code))))
## 11 of the 12 have SO/SIs

## find SO/SI clumps
sub_dat <- data.frame(row = with(naDat, row[grepl("S[OI]", pl_code)]))
sub_dat <- mutate(sub_dat,
                  diff = c(1, diff(row)),
                  start = diff != 1,
                  clump = cumsum(start) + 1)
## consult the poss_team after the SO/SI clump
sub_dat <- ddply(sub_dat, ~ clump, function(x)
  data.frame(x[c('row', 'clump')], poss_row = max(x$row) + 1))
sub_dat <- mutate(sub_dat,
                  proposed_poss_team = gpDat$poss_team[sub_dat$poss_row])
## only proceed if the proposed poss_team is known
sub_dat <- subset(sub_dat, !is.na(proposed_poss_team))
gpDat$poss_team[sub_dat$row] <- sub_dat$proposed_poss_team
naDat <- subset(naDat, !(row %in% sub_dat$row))

## what's left?
str(naDat) # 1 obs. of  11 variables
naDat
(tmp <- show_game_play(naDat$row))

## second iteration of the look behind / ahead thing
naDat <- data.frame(row = which(is.na(gpDat$poss_team)))
naDat <- mutate(naDat,
                pl_code = gpDat$pl_code[row],
                game = gpDat$game[row],
                row_bef = row - 1,
                row_aft = row + 1,
                pb = gpDat$point[row_bef],
                point = gpDat$point[row],
                pa = gpDat$point[row_aft],
                pl_team = gpDat$pl_team[row],
                possb = gpDat$poss_team[row_bef],
                possa = gpDat$poss_team[row_aft])
str(naDat) # 1 obs. of  11 variables:

## if previous possession is known and EITHER:
##   possession after is known and pb = point = pa "iso, no poss change"
##   OR
##   pl_team != possession before and pl_code == 'F' "simple defensive foul"
## possession can be set to possession before
copy_poss_before <- with(naDat,
                         !is.na(possb) & 
                           ((!is.na(possa) & pb == pa) |
                              (possb != pl_team & pl_code == 'F')))
summary(copy_poss_before)
#    Mode    TRUE    NA's 
# logical       1       0 
rows_to_fill <- naDat$row[copy_poss_before]
gpDat$poss_team[rows_to_fill] <- gpDat$poss_team[rows_to_fill - 1] 
naDat <- subset(naDat, !(row %in% rows_to_fill))

## what's left?
str(naDat) # 0 obs. of  11 variables:

## DONE