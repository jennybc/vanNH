library(plyr)

## useful inside reorder(), to invert the resulting factor levels
neglength <- function(x) -1 * length(x)

games <- c("2014-04-12_vanNH-at-pdxST", "2014-04-20_sfoDF-at-vanNH",
           "2014-04-26_vanNH-at-seaRM", "2014-05-10_seaRM-at-vanNH",
           "2014-05-17_vanNH-at-sfoDF", "2014-05-24_pdxST-at-vanNH",
           "2014-05-31_vanNH-at-seaRM", "2014-06-07_seaRM-at-vanNH",
           "2014-06-15_pdxST-at-vanNH")
game_file <- file.path("..", "games", games, "07_resolvedGame",
                       paste0(games, "_gameplay-resolved.tsv"))
names(game_file) <- games
game_play <-
  ldply(game_file, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(game_play) # 5664 obs. of  8 variables

## function to create numbered possessions
## feed it raw poss_team (as a vector) or a matrix/data.fram with poss_team and
## point and optionally game (latter seems a very rare special case)
determine_possession <- function(x) {
  if(is.vector(x)) {
    n <- length(x)
    is_start <- c(TRUE, !(x[2:n] == x[seq_len(n - 1)]))
  } else {
    n <- nrow(x)
    if(is.null(x$game)) {
      is_start <-
        c(TRUE, !(x[2:n, 'poss_team'] == x[seq_len(n - 1), 'poss_team'] &
                    x[2:n, 'point'] == x[seq_len(n - 1), 'point']))
    } else {
      is_start <-
        c(TRUE, !(x[2:n, 'poss_team'] == x[seq_len(n - 1), 'poss_team'] &
                    x[2:n, 'point'] == x[seq_len(n - 1), 'point'] &
                    x[2:n, 'game'] == x[seq_len(n - 1), 'game']))
    }    
    return(cumsum(is_start))
  }
}

## create variables that denote possessions

## Why would I ever need an absolute possession variable for the entire season?
## I made this possible in case I ever analyze groups of points from different
## games where two "point 9"'s could end up adjacent to each other
# mutate(game_play,
#       poss_abs = determine_possession(game_play[c('poss_team', 'point', 'game')]))

## absolute possession variable within game
game_play <- ddply(game_play, ~ game, function(x)
  mutate(x, poss_abs = determine_possession(x[c('poss_team', 'point')])))
str(game_play) # 5664 obs. of  9 variables

## relative possession variable, i.e. within point
game_play <- ddply(game_play, ~ point + game, function(x)
  data.frame(x,
             poss_rel = determine_possession(x[c('poss_team', 'point')])))
str(game_play) # 5664 obs. of  10 variables:

## get the pulling team, which is a point-level thing
game_play <- ddply(game_play, ~ game + point, function(x) {
  data.frame(x, pull_team = x$pl_team[1])
})
str(game_play) # 5664 obs. of  11 variables:

vars_how_i_want <- c('game', 'period', 'point', 'pull_team',
                     'poss_abs', 'poss_rel', 'event',
                     'poss_team', 'pl_team', 'pl_pnum', 'pl_code')
game_play <- game_play[vars_how_i_want]

out_dir <- file.path("..", "games", "2014_west-vs-vanNH")
if(!file.exists(out_dir)) dir.create(out_dir)

out_file <- file.path(out_dir, "2014_west-vs-vanNH_gameplay.rds")
saveRDS(game_play, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west-vs-vanNH_gameplay.tsv")
write.table(game_play, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west-vs-vanNH_gameplay.dput")
dput(game_play, out_file)
message("wrote ", out_file)

## now aggregate at the level of a possession  

## how poss_dat differs from game_play, other than aggregation:
## score = logical indicating if possession ends with a goal
## scor_team = who scored ... NA if nobody did
## who = o_line vs. d_line
poss_dat <- ddply(game_play, ~ game + poss_abs, function(x) {
  pull_team <- x$pull_team[1]
  n <- nrow(x)
  score <- which(grepl("L*G", x$pl_code))
  scor_team <- as.character(if(any(score)) x$pl_team[max(score)] else NA)
  who <- ifelse(x$poss_team[n] == pull_team, "d_line", "o_line")
  if(x$pl_code[n] == 'F') {
    x$pl_code[n] <- if(who == 'o_line') "off F" else "TA"
  }
  data.frame(x[n, ], score = any(score), scor_team, who)
})
str(poss_dat) # 842 obs. of  14 variables:

## sanity checks of poss_dat
(tmp <- ddply(poss_dat, ~ game,
              function(x) with(subset(x, score), table(scor_team))))
colSums(subset(tmp, select = -game))
## yes agrees with actual final scores
addmargins(with(poss_dat, table(who, score)))

## if possession ends due to end of period, set pl_code to 'eop'
poss_dat <- ddply(poss_dat, ~ game + point, function(x) {
  n <- nrow(x)
  if(!x$score[n]) x$pl_code[n] <- 'eop'
  return(x)
})

## revalue pl_code in poss_dat, then reorder by frequency 
## i.e. if possession ends in a throwaway, make code reflect that better
poss_dat$pl_code <-
  mapvalues(poss_dat$pl_code, # revalue() won't work due to factor level ''
            from = c(  '', 'PU',  'L'),
            to   = c('TA', 'TA', 'TA'))
poss_dat$pl_code <- with(poss_dat, reorder(pl_code, pl_code, neglength))
as.data.frame(table(poss_dat$pl_code, dnn = "a_code"))

## create a new version of the pl_code that is coarser: a_code
poss_dat$a_code <-
  mapvalues(poss_dat$pl_code,
            from = c('D', 'HB', 'FB', 'G', 'LG'),
            to   = c('D',  'D',  'D', 'G',  'G'))
poss_dat$a_code <- with(poss_dat, reorder(a_code, a_code, neglength))
as.data.frame(table(poss_dat$a_code, dnn = "a_code"))

## create a new version of the a_code that is YET coarser: b_code
poss_dat$b_code <-
  mapvalues(poss_dat$a_code,
            from = c(    'D',   'TA',    'TD',   'VTT',   'VST', 'off F'),
            to   = c('def +','off -', 'off -', 'off -', 'off -', 'off -'))
poss_dat$b_code <- with(poss_dat, reorder(b_code, b_code, neglength))
as.data.frame(table(poss_dat$b_code, dnn = "b_code"))

out_file <- file.path(out_dir, "2014_west-vs-vanNH_possessions.rds")
saveRDS(poss_dat, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west-vs-vanNH_possessions.tsv")
write.table(poss_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west-vs-vanNH_possessions.dput")
dput(poss_dat, out_file)
message("wrote ", out_file)
