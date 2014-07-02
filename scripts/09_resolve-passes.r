#+ setup, echo = FALSE, results = 'hide'
library(plyr)
library(ggplot2)
library(knitr)
in_file <- file.path("..", "games", "2014_west", "2014_west_gameplay.rds")
game_play <- readRDS(in_file, in_file)
str(game_play)

#' ## Naive exploration of event codes
#' 
#' Each game breaks down into points, which further break down into possessions 
#' and ultimately into *events*. Currently the raw game play data has
{{nrow(game_play)}}
#' rows or events. An event is recorded like so: `89PU`. A player's number, followed by an event code. For the most common event, in which a player catches then throws the disc, we record just the player's number: e.g., `45`.
#' 
#' A completed pass begins with a throw and ends with a catch, with both players
#' belonging to the team on offense. In an ideal world, each possession would
#' just be a sequence of completed passes and it would be simple to resolve each
#' pass by looking at all possible pairs of adjacent rows or events to determine
#' who threw and who caught. Indeed, the most common event is a "catch-throw",
#' denoted `*` below, but there are legitimate events and codes for plays other
#' than pick-up (code `PU`), throw, and catch:
#' 
#'   * Fouls and other violations; codes `F`, `VP`, `VTT`, `VST`.
#'   * Timeouts; code `TO`.
#'   * Subs out and in, e.g. due to injury; codes `SO`, `SI`.
#'   * D's; codes `D`, `HB`, `FB`.
#'   * Drops; code `TD`.
#'   * Scoring; codes `A` for assist and `G` for goal.
#'   
#' First, let's get the frequency of individual event codes, separated by
#' whether the code applies to a player currently on offense (`O`) or defense
#' (`D`).

#+ echo = FALSE
game_play <-
  mutate(game_play,
         who = ifelse(poss_team == pl_team, 'O', 'D'),
         pl_code = mapvalues(pl_code, # no joy w/ revalue() due to level ''
                             from = '', to = '*'))
## TO DO: figure out why I have to (re)set factor levels here
code_who_freq <- ddply(game_play, ~ pl_code,
      function(x) table(factor(x$who, levels = c('D', 'O'))))
code_who_freq$sum <- rowSums(code_who_freq[c('D', 'O')])
code_who_freq <- mutate(code_who_freq, pl_code = reorder(pl_code, sum))
code_who_freq <-
  mutate(code_who_freq, pl_code = factor(pl_code,
                                         levels = rev(levels(pl_code))))
code_who_freq <- arrange(code_who_freq, pl_code)

#+ code-who-freq-table, results = 'asis', echo = FALSE
kable(code_who_freq)

#' ## Data quality and self-consistency: single event codes
#' 
#' ### Events limited to offense or defense
#' 
#' Certain events only make sense for the offense or the defense. Let's make
#' sure the observed data follow these rules.
o_codes <-
  c('*', 'PU', 'A', 'G', 'TD', 'LG', 'TO', 'L', 'PUA', 'LA', 'VTT', 'VST')
sum(with(code_who_freq, O[pl_code %in% o_codes]))
sum(with(code_who_freq, D[pl_code %in% o_codes]))
d_codes <- c('P', 'D', 'OBP', 'HB', 'FB')
sum(with(code_who_freq, D[pl_code %in% d_codes]))
sum(with(code_who_freq, O[pl_code %in% d_codes]))

#' ### Goals and assists
#' The number of assists should equal the number of goals (since we have not had any Callahan's in 2014). Is this true?

(n_assist <- sum(with(code_who_freq, sum[pl_code %in% c('A', 'PUA', 'LA')])))
(n_goals <- sum(with(code_who_freq, sum[pl_code %in% c('G', 'LG')])))
n_assist == n_goals

#' ### Subs out and in
#' 
#' The number of subs out should equal the number of subs in. It is also
#' interesting to compare subbing across the offense and defense; this is often
#' mirrored, though not always.
(o_subs <- with(code_who_freq, O[pl_code %in% c('SO', 'SI')]))
diff(o_subs)
(d_subs <- with(code_who_freq, D[pl_code %in% c('SO', 'SI')]))
diff(d_subs)

#' ### Pulls, points, goals
#' 
#' The number of pulls should equal the number of points played, which should be
#' greater than or equal to the number of goals.
(n_pulls <- sum(with(code_who_freq, sum[pl_code %in% c('P', 'OBP')])))
(n_pts <- sum(ddply(game_play, ~ game, summarize, n_pts = max(point))$n_pts))
n_pulls == n_pts
n_pts >= n_goals

#' ## Recoding events
#' 
#' I will collapse certain event codes that don't need to be distinguished for
#' my ultimate goal of resolving passes:
#' 
#'   * pulls: `P` + `OBP` --> `P`
#'   * catch-throws: `*` + `A` + `L` + `LA` --> `CTH`
#'   * goals: `G` + `LG` --> `G`
#'   * pick-ups: `PU` + `PUA` --> `PU`
#'   * D's: `HB` + `FB` + `D` --> `D`
#'   * offensive violations: `VTT` + `VST` --> `OV`
#'   
#' and will also retabulate.

#+ echo = FALSE
game_play <-
  mutate(game_play,
         pl_code = revalue(pl_code, c("OBP" = "P", "*" = "CTH", "A" = "CTH",
                                      "LG" = "G", "L" = "CTH", "PUA" = "PU",
                                      "LA" = "CTH", "HB" = "D", "FB" = "D",
                                      "VTT" = "OV", "VST" = "OV")))
## TO DO: figure out why I have to (re)set factor levels here
code_who_freq <- ddply(game_play, ~ pl_code,
                       function(x) table(factor(x$who, levels = c('D', 'O'))))
code_who_freq$sum <- rowSums(code_who_freq[c('D', 'O')])
code_who_freq <- mutate(code_who_freq, pl_code = reorder(pl_code, sum))
code_who_freq <-
  mutate(code_who_freq, pl_code = factor(pl_code,
                                         levels = rev(levels(pl_code))))
code_who_freq <- arrange(code_who_freq, pl_code)

#+ code-who-freq-table-again, results = 'asis', echo = FALSE
kable(code_who_freq)

#' ## Data quality and self-consistency: adjacent event codes
#' 
#' ### Are pulls always followed by pick-ups?
table(with(game_play, pl_code[which(pl_code == 'P') + 1]))

#' ### Are pickups always the first event in a possession?
#' First, we isolate pickups and disregard pickups immediately after the pull.
length(pickups <- which(game_play$pl_code == "PU"))
length(pickups <- pickups[game_play$pl_code[pickups - 1] != 'P'])
#' Now, grab the immediately previous event, note its code and note which
#' possession it belongs to, obtained from `poss_rel`, which identifies
#' possessions within a point. This is just a positive integer, so I can subtract
#' the possession number of this preceding event from that of the pickup. If the
#' pickup is the first event of a possession, this difference will be 1.
foo <- with(game_play,
            data.frame(gprow = pickups,
                       pre_code = pl_code[pickups - 1],
                       pre_poss = poss_rel[pickups - 1],
                       pu_code = pl_code[pickups],
                       pu_poss = poss_rel[pickups]))
foo$diff <- with(foo, pu_poss - pre_poss)
foo <- arrange(foo, diff, pre_code)
table(foo$diff)
#' I want to take a closer look at the pickups that share the possession number with the previous event, i.e. where the difference is 0.
foo <- subset(foo, diff == 0)
table(foo$pre_code)
#' The timeout (`TO`) and sub in (`SI`) codes are understandable. Let's
#' eliminate them so we can scrutinize what's left.
foo <- subset(foo, !(pre_code %in% c('TO', 'SI')))
table(foo$pre_code)
write.table(game_play[rep(sort(foo$gprow), each = 4) + -2:1, ],
            "blah.tsv", row.names = FALSE, sep = "\t", quote = FALSE)
#' Here's where I followed up in the Google spreadsheets and, if necessary, on video:
#' 
#'   * week 02 2014-04-20_sfoDF-at-vanNH, period 2, point 13, score is 3-8
#'     - data in Google spreadsheet seems to have changed; re-pulled and this puzzle went away
#'   * week 06 2014-05-17_vanNH-at-sfoDF, period 2, point 14, score is 5-7
#'     - data in Google spreadsheet seems to have changed; re-pulled and this puzzle went away
#'   * week 09 2014-06-07_seaRM-at-vanNH, period 3, point 25, score is 8-15
#'     - Here's the puzzling sequence of events. vanNH have possession. 37 catches the disc. There is a pick called on the defense then vanNH calls a timeout. This was originally recorded with `37TO` in the offensive cell and `?VP` in the defensive cell. It's a bit tricky to recover the actual sequence of events from this: 37 catches, then the defense picks, then vanNH call timeout. My clean and expand script puts defensive acts in the row *after* offensive acts, in general, and gets fooled here by the fact that `37TO` refers to two separate acts, with the pick happening in between. Rather than handle this special case in my own script I've edited the spreadsheet to have vanNH 37 catch, have the defensive pick in the adjacent cell and then, in the next row, record a timeout call by `?`. I've re-pulled the data from this point.
#'  * week 10 2014-06-15_pdxST-at-vanNH, period 4, point 34, score is 15-16
#'     - This is related to the previous case, in that involves a defensive foul and a timeout. Again, the recording of multiple events in one row makes it very tricky to determine the order of actions. Here it's complicated by the fact that the foul called on the defense was called on the bench *during the timeout* itself. I added a row, put a clean `8` for a catch by vanNH 8, then in the following row retained the `2F` code for the unsportsmanlike conduct call on the defense's bench during the timeout and entered a `?TO` for the offense to record the timeout call. I suspect all timeouts should just be entered on their own line, quite possibly with no associated number, i.e. just as `TO`. I've re-pulled the data from this point, but it will continue to get picked up here as non-standard. I think the foul during a timeout will remain a very rare phenomenon.
#'  * week12 (playoffs) 2014-06-28_vanNH-at-pdxST, period 2, point 13, score is 6-5
#'    - This is an *offensive foul* (pick) that causes the "catch" part of a catch throw to occupy a different cell and row and the corresponding "throw", with the offensive `VP` recorded in between. I guess this is a legit use of `PU`.
#'  * week12 (playoffs) 2014-06-28_vanNH-at-pdxST, period 3, point 22, score is 10-9
#'    - Some garbled game play had already been fixed on the Google spreadsheet, so I re-pulled that and this puzzle has gone away.
#'  * week 02 2014-04-19_sfoDF-at-seaRM, period 4, point 34, score is 13-17
#'    - Some garbled game play had already been fixed on the Google spreadsheet, so I re-pulled that
#' 
#' ## Frequency of adjacent event code pairs
#' 
#' In order to develop the pass-resolving logic, I want to study the frequency of these code pairs for adjacent events. I will only look at adjacent events *within a possession*, so the number of these pairs will be smaller than the total number of events minus 1. The number of pairs is, in fact, `nrow(hap_dat)`.

#+ form-hap-freq, echo = FALSE, results = 'hide'
#hap_freq <- as.data.frame(table(hap_dat$hap, dnn = "hap"))
#str(hap_freq)
#n_pl_code <- length(unique(game_play$pl_code))

#'   
#'     although most apply o and the code could be providing information about the offense or the defense (), so, in theory, there are `n_pl_code^2` possibilities for the codes.

#+ form-hap, echo = FALSE, results = 'hide'
# x <- subset(game_play, game == "2014-06-28_vanNH-at-pdxST" & point == 2 &
#               poss_rel == 1)
# hap_dat <- ddply(game_play, ~ game + poss_abs, function(x) {
#   n <- nrow(x)
#   x$hap <- with(x, paste(ifelse(poss_team == pl_team, 'O', 'D'),
#                          ifelse(pl_code == '', '*', pl_code), sep = "-"))
#   data.frame(with(x, data.frame(hap = paste(hap[-n], hap[-1]))))
# })
#str(hap_dat)


# hap_freq <- mutate(hap_freq, hap = reorder(hap, -1 * Freq))
# hap_freq <- arrange(hap_freq, hap)
# str(hap_freq)
# head(hap_freq)
# 
# p <- ggplot(hap_freq, aes(x = hap, y = Freq))
# p + geom_bar(stat = "identity")
# 

## could an event be the beginning or end of a pass?
# pass_beg <- with(x, pl_code %in% c('PU', '', 'A', 'PUA', 'L'))
# pass_end <- with(x, pl_code %in% c('', 'L', 'A', 'AL', 'LA', 'G', 'LG', 'TD'))
# data.frame(x, pass_beg, pass_end)
