#+ setup, include = FALSE
library(plyr)
## useful inside reorder(), to invert the resulting factor levels
neglength <- function(x) -1 * length(x)

#' ### Bring in game play data.

#' Whitelist of games for which game play will be read. Read 'em.
games <- c("2014-04-12_vanNH-at-pdxST", "2014-04-20_sfoDF-at-vanNH",
           "2014-04-26_vanNH-at-seaRM", "2014-05-10_seaRM-at-vanNH",
           "2014-05-17_vanNH-at-sfoDF", "2014-05-24_pdxST-at-vanNH",
           "2014-05-31_vanNH-at-seaRM", "2014-06-07_seaRM-at-vanNH",
           "2014-06-15_pdxST-at-vanNH", "2014-06-21_vanNH-at-sfoDF",
           "2014-06-28_vanNH-at-pdxST",
           "2014-04-12_seaRM-at-sfoDF", "2014-04-19_sfoDF-at-seaRM",
           "2014-04-26_pdxST-at-sfoDF", "2014-05-04_sfoDF-at-seaRM")
game_file <- file.path("..", "games", games, "07_resolvedGame",
                       paste0(games, "_gameplay-resolved.tsv"))
names(game_file) <- games
game_play <-
  ldply(game_file, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(game_play)

#' ### Concatenate all game play data 

out_dir <- file.path("..", "games", "2014_west")
if(!file.exists(out_dir)) dir.create(out_dir)

out_file <- file.path(out_dir, "2014_west_gameplay.rds")
saveRDS(game_play, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_gameplay.tsv")
write.table(game_play, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_gameplay.dput")
dput(game_play, out_file)
message("wrote ", out_file)




#' ### Aggregate to the level of a possession.  

#' How does `poss_dat` differ from `game_play`, other than aggregation?
#' 
#' *  `n_events` = number of events
#' * `score` = logical indicating if possession ends with a goal
#' * `scor_team` = who scored ... `NA` if nobody did
#' * `who` = o_line vs. d_line
poss_dat <- ddply(game_play, ~ game + poss_abs, function(x) {
  score <- which(grepl("L*G", x$pl_code))
  ## Get rid of any rows after a goal. why? because of cases like point 35 of
  ## 2014-04-12_vanNH-at-pdxST, in which a defensive foul is recorded after a
  ## successful goal; the goal was not being picked up here as the final event
  ## of the possession and was, instead, being recorded as an *offensive* foul.
  if(length(score) > 0)
    x <- x[seq_len(score), ]
  ## If possession ends with a *defensive* foul, remove the final row; examples:
  ## 2014-04-26_vanNH-at-seaRM poss_abs 23, 2014-05-17_vanNH-at-sfoDF poss_abs 
  ## 92, 2014-05-31_vanNH-at-seaRM poss_abs 57; all have possessions in which a 
  ## thrower has the disc, there's a foul by the defense and ... the throw is 
  ## not caught ... we need to see the offensive throwaway as the last event of
  ## the possession, not the defensive foul
  n <- nrow(x)
  if(x$pl_team[n] != x$poss_team[n] & x$pl_code[n] == 'F') {
    x <- x[seq_len(n - 1), ]
    n <- nrow(x)
  }
  pull_team <- x$pull_team[1]
  n <- nrow(x)
  huck <- grepl("L", x$pl_code)
  scor_team <- as.character(if(any(score)) x$pl_team[max(score)] else NA)
  who <- ifelse(x$poss_team[n] == pull_team, "d_line", "o_line")
  if(x$pl_code[n] == 'F' & x$pl_team[n] == x$poss_team[n]) {
    x$pl_code[n] <- if(who == 'o_line') "off F" else "TA"
  }
  data.frame(x[n, ], n_events = n, huck = any(huck),
             score = any(score), scor_team, who)
})
str(poss_dat)

#' Sanity check and explore `poss_dat`. *TO DO*: rigorously check against known
#' final scores.
(tmp <- ddply(poss_dat, ~ game,
              function(x) with(subset(x, score), table(scor_team))))
colSums(subset(tmp, select = -game))
addmargins(with(poss_dat, table(who, score)))

#' Harmonize factor levels for `scor_team` with those of other team factor
#' variables.
poss_dat <- transform(poss_dat , scor_team = factor(scor_team, levels = jTeams))
str(poss_dat)

#' If possession ends due to end of period, set `pl_code` to `eop`.
poss_dat <- ddply(poss_dat, ~ game + point, function(x) {
  n <- nrow(x)
  if(!x$score[n]) x$pl_code[n] <- 'eop'
  return(x)
})

#' Groom `pl_code` and create derivatives, so they give explicit information on
#' how possessions end, at different levels of detail. Also reorder levels by 
#' frequency, with most frequent code appearing first.
poss_dat$pl_code <-
  mapvalues(poss_dat$pl_code, # revalue() won't work due to factor level ''
            from = c(  '', 'PU',  'L'),
            to   = c('TA', 'TA', 'TA'))
poss_dat$pl_code <- with(poss_dat, reorder(pl_code, pl_code, neglength))
as.data.frame(table(poss_dat$pl_code, dnn = "a_code"))

poss_dat$a_code <- # a_code is coarser than pl_code but still detailed
  mapvalues(poss_dat$pl_code,
            from = c('D', 'HB', 'FB', 'G', 'LG'),
            to   = c('D',  'D',  'D', 'G',  'G'))
poss_dat$a_code <- with(poss_dat, reorder(a_code, a_code, neglength))
as.data.frame(table(poss_dat$a_code, dnn = "a_code"))

poss_dat$b_code <- #b_code is the coarsest
  mapvalues(poss_dat$a_code,
            from = c(    'D',   'TA',    'TD',   'VTT',   'VST', 'off F'),
            to   = c('def +','off -', 'off -', 'off -', 'off -', 'off -'))
poss_dat$b_code <- with(poss_dat, reorder(b_code, b_code, neglength))
as.data.frame(table(poss_dat$b_code, dnn = "b_code"))

#' Write `poss_dat` to file.
out_file <- file.path(out_dir, "2014_west_possessions.rds")
saveRDS(poss_dat, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_possessions.tsv")
write.table(poss_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_possessions.dput")
dput(poss_dat, out_file)
message("wrote ", out_file)

#' ### Aggregate to the level of a point 

#' How does `point_dat` differ from `poss_dat`, other than aggregation?
#' 
#' *  Just know that `n_events` is not the number of the events of the last
#' possession, but rather is computed from `poss_dat$event` and gives the number
#' of events in the whole point.
point_dat <- ddply(poss_dat, ~ game + point, function(x) {
  n <- nrow(x)
  x$n_events <- NULL
  x <- rename(x, c("event" = "n_events", "poss_rel" = "n_poss"))
  x[n, ]
})
str(point_dat)
table(point_dat$score, useNA = "always")
table(point_dat$pl_code, useNA = "always")
table(point_dat$a_code, useNA = "always")
table(point_dat$b_code, useNA = "always")
table(point_dat$huck, useNA = "always")
addmargins(with(point_dat, table(score, huck)))

out_file <- file.path(out_dir, "2014_west_points.rds")
saveRDS(point_dat, out_file)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_points.tsv")
write.table(point_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)

out_file <- file.path(out_dir, "2014_west_points.dput")
dput(point_dat, out_file)
message("wrote ", out_file)
