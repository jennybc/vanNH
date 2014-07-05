#' ---
#' title: "Prepare to resolve passes"
#' author: "Jenny Bryan"
#' date: "July 2014"
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 2
#' ---

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
#' rows or events. A typical event is recorded like so: `89PU`, i.e. as a 
#' player's number, followed by an event code. The most common event is a 
#' "catch-throw", in which a player catches then throws the disc. In this case, 
#' we record just the player's number: e.g., `45`. Below, we give this an *ad 
#' hoc* code of `*`, which is easier to work with than the empty code.
#' 
#' The goal of this analysis is to prepare for resolving individual passes from 
#' event level data. A completed pass begins with a throw and ends with a catch,
#' with both players belonging to the team on offense. In an ideal world, each 
#' possession would just be a sequence of completed passes and it would be 
#' simple to resolve each pass by looking at all possible pairs of adjacent rows
#' or events to determine who threw and who caught. But it's not so simple,
#' since there are plenty of other legitimate events sprinkled throughout the
#' game play:
#' 
#' * Pickup; code `PU`.
#' * Fouls and other violations; codes `F`, `VP`, `VTT`, `VST`.
#' * Timeouts; code `TO`.
#' * Subs out and in, e.g. due to injury; codes `SO`, `SI`.
#' * D's; codes `D`, `HB`, `FB`.
#' * Drops; code `TD`.
#' * Scoring; codes `A` for assist and `G` for goal.
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

#+ echo = FALSE, message = FALSE
j_revalues <- c("OBP" = "P", "*" = "CTH", "A" = "CTH",
                "LG" = "G", "L" = "CTH", "PUA" = "PU",
                "LA" = "CTH", "HB" = "D", "FB" = "D",
                "VTT" = "OV", "VST" = "OV")
game_play <- mutate(game_play, pl_code = revalue(pl_code, j_revalues))
o_codes <- unique(revalue(o_codes, j_revalues))
d_codes <- unique(revalue(d_codes, j_revalues))
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
#' Isolate pickups.
length(pickups <- which(game_play$pl_code == "PU"))
#' Now disregard pickups immediately after the pull. Here's what remains.
length(pickups <- pickups[game_play$pl_code[pickups - 1] != 'P'])
#' Now, I grab the immediately previous event, note its code and note which 
#' possession it belongs to, obtained from `poss_abs`, which identifies 
#' possessions within a game. This is just a positive integer, so I can
#' subtract the possession number of this preceding event from that of the
#' pickup. If the pickup is the first event of a possession, this difference
#' will be 1. If the pickup belongs to the same possession as the previous
#' event, this difference will be 0 and we need to take a closer look.

#- echo = FALSE
foo <- with(game_play,
            data.frame(gprow = pickups,
                       pre_code = pl_code[pickups - 1],
                       pre_poss = poss_abs[pickups - 1],
                       pu_code = pl_code[pickups],
                       pu_poss = poss_abs[pickups]))
foo$diff <- with(foo, pu_poss - pre_poss)
foo <- arrange(foo, diff, pre_code)

table(foo$diff)
#' Now scrutinize the pickups that share the possession number with the previous
#' event, i.e. where the difference is 0, and look at the event codes.

#+ echo = FALSE
foo <- droplevels(subset(foo, diff == 0))

table(foo$pre_code)

#' #### Pickups that occur in the middle of a possession
#' 
#' Pickups by the possessing team after a timeout (`TO`) or a sub in (`SI`) are
#' understandable. Let's eliminate them so we can focus on what's left.

#+ echo = FALSE
foo <- droplevels(subset(foo, !(pre_code %in% c('TO', 'SI'))))

table(foo$pre_code)

#' When I first did this, I had digested 14 games from the Western Conference. I
#' followed up on ~10 points in the Google spreadsheets and/or on video, which 
#' lead to various resolutions. I predict this cycle will continue as I absorb 
#' more data from the 2014 season; I suppose I'll keep adding notes here about 
#' the pathologies that are possible.
#' 
#' #### Surprising pick-ups due to data entry errors that have already been
#' fixed
#' 
#' For these points, I found that the data in the Google spreadsheets had been
#' corrected since I extracted. Therefore, I re-extracted these points and
#' re-did all downstream processing. These pickups no longer require special
#' attention because they *do* initiate a new possession.
#' 
#'   - week 02 2014-04-20_sfoDF-at-vanNH, period 2, point 13, score is 3-8
#'   - week 06 2014-05-17_vanNH-at-sfoDF, period 2, point 14, score is 5-7
#'   - week 12 (playoffs) 2014-06-28_vanNH-at-pdxST, period 3, point 22, score is 10-9
#'   - week 02 2014-04-19_sfoDF-at-seaRM, period 4, point 34, score is 13-17
#' 
#' #### Surprising pick-ups due to data entry errors that I then fixed
#' 
#' Re: point 25, period 3, week 09 2014-06-07_seaRM-at-vanNH (score is 8-15)....
#' vanNH have possession. 37 catches the disc. Pick called on the defense then 
#' vanNH calls a timeout. Originally recorded with `37TO` in the offensive cell 
#' and `?VP` in the defensive cell. It's a bit tricky to recover the actual 
#' sequence of events from this: 37 catches, then the defense picks, then vanNH 
#' call timeout. My clean and expand script puts defensive acts in the row 
#' *after* offensive acts, in general, and gets fooled here by the fact that 
#' `37TO` refers to two separate offensive acts, with the pick happening in 
#' between. Rather than handle this special case in my own script I edited the 
#' spreadsheet to have vanNH 37 catch, have the defensive pick in the adjacent 
#' cell and then, in the next row, record a timeout call by `?`. I've re-pulled 
#' the data from this point. This increases my belief that we should record
#' timeouts as `TO`, with no number, in its own cell and row.
#' 
#' Re: point 22, period 3, Western Conference Final 2014-06-28_vanNH-at-pdxST 
#' (score is 10-9, around 1:32 in the video). vanNH-8 is the intended receiver 
#' of a throw from 11 but is fouled by pdxST-13 and the disc hits the ground. 
#' vanNH-8 picks it up and play resumes. This was recorded as `8PU` but I 
#' removed the `PU`, since the end result is we proceed as if he caught it. I 
#' removed this `PU` code and re-pulled the data from this point.

#+ echo = FALSE
to_examine <- game_play[rep(sort(foo$gprow), each = 4) + -2:1, ]

#' #### Surprising pick-ups that shall remain
#' 
#' There are
{{nrow(foo)}}
#' `PU`s that require a close look. Here's the game play data for these pickups, including the two rows before and one row after.

#+ echo = FALSE, results = 'asis'
kable(to_examine)

#+ include = FALSE
## I looked at this data in Excel when troubleshooting.
#write.table(to_examine, "blah.tsv", row.names = FALSE, sep = "\t", quote = FALSE)

#' Here are notes about the remaining pickups:
#' 
#'  * week 10 2014-06-15_pdxST-at-vanNH, period 4, point 34, score is 15-16
#'     - This is tricky due to a combination of factors. First, the recording of
#'     an offensive catch and timeout in the same cell. Second the recording of
#'     a defensive foul in the same row. It's even worse in this case, because
#'     the unsportsmanlike conduct call was made during the timeout. I added a
#'     row, put a clean `8` for a catch by vanNH 8, then in the following row
#'     retained the `2F` code for the unsportsmanlike conduct call on the
#'     defense's bench during the timeout and entered a `?TO` for the offense to
#'     record the timeout call. I suspect all timeouts should just be entered on
#'     their own line, quite possibly with no associated number, i.e. just as
#'     `TO`. I've re-pulled the data from this point, but it will continue to
#'     get picked up here as non-standard. I think the foul during a timeout
#'     will remain a very rare phenomenon.

#'  * week12 (playoffs) 2014-06-28_vanNH-at-pdxST, period 2, point 13, score is
#'  6-5.
#'     - This is an *offensive foul* (pick) that causes the "catch" part of a
#'     catch throw to occupy a different cell and row from the corresponding
#'     "throw", with the offensive `VP` recorded in between. I guess this is a
#'     legit use of `PU` in the middle of a possession.
#'    
#' ### Are D's always the last event in a possession?
#' Isolate D's.
length(ds <- which(game_play$pl_code == "D"))

#+ echo = FALSE
foo <- with(game_play,
            data.frame(gprow = ds,
                       d_code = pl_code[ds],
                       d_poss = poss_abs[ds],
                       d_game = game[ds],
                       d_point = point[ds],
                       after_code = pl_code[ds + 1],
                       after_poss = poss_abs[ds + 1],
                       after_game = game[ds + 1],
                       after_point = point[ds + 1]))
foo <- subset(foo, d_game == after_game & d_point == after_point)

#' Filter out any D's that are the last event in a point, since it's nonsensical
#' to look forward in this case. The D is the last event in a possession by
#' definition in this case. Here's what's left.
nrow(foo)

#' Compute this difference: possession number of next event - possession number
#' of the D. I expect this to mostly be 1, i.e. the D causes a change of
#' possession.
foo$diff <- with(foo, after_poss - d_poss)
foo <- arrange(foo, diff, after_code)
table(foo$diff)

#' Filter out the D's I understand, i.e. those that cause a change of possession and scrutinize what's left (if anything).

#+ echo = FALSE, results = 'hide'
foo <- subset(foo, diff != 1)
table(foo$diff)
to_examine <- game_play[rep(sort(foo$gprow), each = 5) + -2:2, ]

#+ echo = FALSE
foo

#+ echo = FALSE, results = 'asis'
if(nrow(to_examine) > 0)
  kable(subset(to_examine, select = c(game, point, poss_abs, event, poss_team,
                                      pl_team, pl_pnum, pl_code, who)))

#' ## Understanding where event codes fall within possessions
#' 
#' ### Tabulating the first and last event codes for all possessions
#' 
#' I ignore pulls, which are an artificial first event for the first possession 
#' of every point. Having looked at the data, I've also learned to ignore clumps
#' of subs off and in at the beginning of a possession. The scenario is that 
#' someone lays out for a successful D but gets injured in the process. The 
#' possession changes, but the first 2 or 4 events are `SO` + `SI`. After
#' ignoring such events at the beginning of a possession, I now grab the first
#' ("alpha") and last ("omega") row of game play for each possession and cross
#' tabulate `who` (offense vs defense) and `pl_code`.

#+ echo = FALSE, results = 'hide'
poss_ao <- ddply(game_play, ~ game + poss_abs, function(x) {
  alpha_row <- min(which(!(x$pl_code %in% c('P', 'SO', 'SI'))))
  y <- data.frame(x[c(alpha_row, nrow(x)), ])
  data.frame(y, where = I(c('alpha', 'omega')))
})
str(poss_ao)

#' Here are the results for the first event in a possession:
#+ echo = FALSE
with(subset(poss_ao, where == "alpha"), table(pl_code, who))
#' The code I most expect to see here is a pickup `PU` by the offense. The 
#' timeouts `TO` are also expected. Many timeouts occur immediately after a 
#' turnover, so the offense can bring on an o_line. For better or worse, the
#' initial pickup `PU` sometimes doesn't even get recorded (or maybe it doesn't
#' even occur, if the timeout is called from the sideline?) and the first code 
#' recorded for the new possession is `TO`. Let's inspect the codes recorded 
#' just before and just after these `TO`s.

#+ echo = FALSE, results = 'hide', message = FALSE
to <- match_df(game_play,
               subset(poss_ao, where == "alpha" & pl_code == "TO"))
length(to_rows <- as.numeric(rownames(to)))
# a_ply(to_rows, 1, function(i) {
#   cat("\n")
#   print(game_play[i + (-2:2), ])
#   cat("\n")
# })

#+ echo = TRUE
with(game_play[to_rows - 1, ], table(who, pl_code))
with(game_play[to_rows + 1, ], table(who, pl_code))

#' Events that are plausible before a possession-initiating timeout `TO`: a `D` 
#' or, by the offense, `CTH`, `OV`, `PU`, `TD`. Events that are plausible after 
#' a possession-initiating timeout `TO`: a pickup `PU` by the offense.
#' 
#' Now let's look at possessions that start with `CTH`. Ideally these all
#' represent interception D's. Here are the codes recorded just before these
#' possession-initiating `CTH`s.

#+ echo = FALSE, message = FALSE
cth <- match_df(game_play,
                subset(poss_ao, where == "alpha" & pl_code == "CTH"))
length(cth_rows <- as.numeric(rownames(cth)))

with(game_play[cth_rows - 1, ], table(who, pl_code))
#' All is well if I see only `D`s here. __DONE__ vetting alpha events.

#' Here are the results for the last event in a possession:
#+ echo = FALSE
with(subset(poss_ao, where == "omega"), table(pl_code, who))
#' The most expected outcomes here are a goal `G` by the offense and a `D` by 
#' the defense. It also makes alot of sense to see a possession end because of a
#' drop `TD` or travel or stall `OV` by the offense. I plan to set these aside
#' and take a closer look at what's left.
#' 
#' Before I eliminate these expected results, I first want to look explicitly for
#' possessions where the same event is alpha and omega, i.e. possessions that 
#' consist of exactly one event. How many possessions are affected and what are 
#' the event codes for these possessions?

#+ echo = FALSE, results = 'hide'
alpha_is_omega <- ddply(subset(poss_ao, select = -where),
                        ~ game + poss_abs, function(x) {
  dups <- duplicated(x)
  if(any(dups)) {
    return(data.frame(ind = rep(TRUE, length(dups))))
  } else {
    return(data.frame(ind = rep(FALSE, length(dups))))
  }
})
str(alpha_is_omega)
table(alpha_is_omega$ind)
poss_ao[head(which(alpha_is_omega$ind)), ]

#+ echo = FALSE
addmargins(table(poss_ao[alpha_is_omega$ind &
                           poss_ao$where == "omega", "pl_code"]))

#' The code I expected to see here is `PU`, which indicates someone picked up 
#' and proceeded to throw it away. A `CTH` also makes sense; these are just a special case of the possession-initiating `CTH`s investigated above. As long as these are the only one-event-possession codes, we can move on.
#' 
#' Let's eliminate the observations that comply with expectations and then
#' retabulate.

#+ echo = FALSE
poss_ao <- subset(poss_ao, !alpha_is_omega$ind)
poss_ao <- subset(poss_ao, !(where == "alpha"))
poss_ao <- subset(poss_ao, !(where == "omega" & who == "D" & pl_code == "D"))
poss_ao <- subset(poss_ao, !(where == "omega" & who == "O" &
                               pl_code %in% c('G', 'OV', 'TD', 'CTH')))

#' Here are the updated results for unusual last events in a possession:
with(subset(poss_ao, where == "omega"), table(pl_code, who))

#' Here's how a possession can end with a pickup: the team on offense calls a
#' timeout, then they pickup the disc and throw it away. So let's check that all
#' of our possession-ending `PU`s do indeed follow a timeout `TO`.

#+ echo = FALSE, results = 'hide', message = FALSE
pu <- match_df(game_play,
               subset(poss_ao, where == "omega" & pl_code == "PU"))
length(pu_rows <- as.numeric(rownames(pu)))

#+ echo = FALSE, message = FALSE
with(game_play[pu_rows - 1, ], table(who, pl_code))
#' All is well if I see only `TO`s by the offense here.
 
#' Now let's look at possessions that end with a foul on the offense.
#+ echo = FALSE, message = FALSE
off_f <-
  match_df(game_play,
           subset(poss_ao, where == "omega" & pl_code == "F" & who == 'O'))
length(off_f_rows <- as.numeric(rownames(off_f)))
a_ply(off_f_rows, 1, function(i) {
  cat("\n")
  print(game_play[i + (-2:2), ])
  cat("\n")
})
#' Yes, this game play makes sense to me.

#' Now let's look at possessions that end with a foul on the defense.
#+ echo = FALSE, message = FALSE
def_f <-
  match_df(game_play,
           subset(poss_ao, where == "omega" & pl_code == "F" & who == 'D'))
length(def_f_rows <- as.numeric(rownames(def_f)))
a_ply(def_f_rows, 1, function(i) {
  cat("\n")
  print(game_play[i + (-2:2), ])
  cat("\n")
})
#' I see the defensive foul called on a successful goal from
#' 2014-04-12_vanNH-at-pdxST point 35. Then I see three instances of offense
#' having the disc (two are `PU`s, although that appears to be a coincidence),
#' getting fouled by the defense, and then proceeding to throw it away. Yes,
#' this game play makes sense to me.
#' 
#' __DONE__ vetting alpha and omega events.

#+ echo = FALSE, results = 'hide'
## these are ad hoc looks that helped me resolve all sorts of things; I expect
## they might be useful again as I ingest more data
subset(poss_ao, where == "alpha" & who == "D")
subset(poss_ao, where == "alpha" & pl_code == "OV")
subset(poss_ao, where == "alpha" & pl_code == "TD")
subset(poss_ao, where == "alpha" & pl_code == "SO")

#' ## Summary of what I've learned
#' 
#' There are certain codes that are only used for the offense:
o_codes
#' There are certain codes that are only used for the defense:
d_codes
#' There are codes used for both the offense and the defense:
setdiff(unique(game_play$pl_code),union(o_codes, d_codes))

#' 
#' Do possessions always start with a pickup? No.
#' 
#' Do pickups always start a new possession? No.
#' 
#'    always the first event in a possession?


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
