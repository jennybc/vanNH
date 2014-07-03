


### Bring in game play data.
Whitelist of games for which game play will be read. Read 'em.


```r
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
```

```
## 'data.frame':	9246 obs. of  8 variables:
##  $ game     : Factor w/ 15 levels "2014-04-12_vanNH-at-pdxST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ period   : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ point    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ event    : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ pl_team  : chr  "vanNH" "pdxST" "pdxST" "pdxST" ...
##  $ pl_pnum  : chr  "8" "7" "24" "13" ...
##  $ pl_code  : chr  "P" "PU" "" "" ...
##  $ poss_team: chr  "pdxST" "pdxST" "pdxST" "pdxST" ...
```

### Concatenate all game play data and add possession identifiers.  
Define a function to create numbered possessions. *Code hidden*.



Create variables that denote possessions. `poss_abs` holds an absolute possession number *within a specific game*. `poss_rel` holds a relative possession number *within a specific point.*


```r
game_play <- ddply(game_play, ~ game, function(x)
  mutate(x, poss_abs = determine_possession(x[c('poss_team', 'point')])))
game_play <- ddply(game_play, ~ point + game, function(x)
  data.frame(x,
             poss_rel = determine_possession(x[c('poss_team', 'point')])))
str(game_play)
```

```
## 'data.frame':	9246 obs. of  10 variables:
##  $ game     : Factor w/ 15 levels "2014-04-12_vanNH-at-pdxST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ period   : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ point    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ event    : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ pl_team  : chr  "vanNH" "pdxST" "pdxST" "pdxST" ...
##  $ pl_pnum  : chr  "8" "7" "24" "13" ...
##  $ pl_code  : chr  "P" "PU" "" "" ...
##  $ poss_team: chr  "pdxST" "pdxST" "pdxST" "pdxST" ...
##  $ poss_abs : int  1 1 1 1 1 1 1 2 2 2 ...
##  $ poss_rel : int  1 1 1 1 1 1 1 2 2 2 ...
```


Create new variable `pull_team`, which carries the pulling team. Obviously is
constant within a point.


```r
game_play <- ddply(game_play, ~ game + point,
                   function(x) data.frame(x, pull_team = x$pl_team[1]))
str(game_play)
```

```
## 'data.frame':	9246 obs. of  11 variables:
##  $ game     : Factor w/ 15 levels "2014-04-12_vanNH-at-pdxST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ period   : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ point    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ event    : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ pl_team  : chr  "vanNH" "pdxST" "pdxST" "pdxST" ...
##  $ pl_pnum  : chr  "8" "7" "24" "13" ...
##  $ pl_code  : chr  "P" "PU" "" "" ...
##  $ poss_team: chr  "pdxST" "pdxST" "pdxST" "pdxST" ...
##  $ poss_abs : int  1 1 1 1 1 1 1 2 2 2 ...
##  $ poss_rel : int  1 1 1 1 1 1 1 2 2 2 ...
##  $ pull_team: Factor w/ 4 levels "vanNH","pdxST",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Work on team variables that should be factors. Reorder levels for `pull_team`
and convert `poss_team` to factor. Level order set to final 2014 Western
Conference ranking.


```r
jTeams <- c("vanNH", "pdxST", "seaRM", "sfoDF")
jFun <- function(x, xlevels = jTeams) factor(x, levels = xlevels)
game_play <- transform(game_play, pull_team = jFun(pull_team),
                       poss_team = factor(poss_team, levels = jTeams))
str(game_play)
```

```
## 'data.frame':	9246 obs. of  11 variables:
##  $ game     : Factor w/ 15 levels "2014-04-12_vanNH-at-pdxST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ period   : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ point    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ event    : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ pl_team  : chr  "vanNH" "pdxST" "pdxST" "pdxST" ...
##  $ pl_pnum  : chr  "8" "7" "24" "13" ...
##  $ pl_code  : chr  "P" "PU" "" "" ...
##  $ poss_team: Factor w/ 4 levels "vanNH","pdxST",..: 2 2 2 2 2 2 2 1 1 1 ...
##  $ poss_abs : int  1 1 1 1 1 1 1 2 2 2 ...
##  $ poss_rel : int  1 1 1 1 1 1 1 2 2 2 ...
##  $ pull_team: Factor w/ 4 levels "vanNH","pdxST",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Tidy up and write game play to file.


```r
vars_how_i_want <- c('game', 'period', 'point', 'pull_team',
                     'poss_abs', 'poss_rel', 'event',
                     'poss_team', 'pl_team', 'pl_pnum', 'pl_code')
game_play <- game_play[vars_how_i_want]

out_dir <- file.path("..", "games", "2014_west")
if(!file.exists(out_dir)) dir.create(out_dir)

out_file <- file.path(out_dir, "2014_west_gameplay.rds")
saveRDS(game_play, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_west/2014_west_gameplay.rds
```

```r
out_file <- file.path(out_dir, "2014_west_gameplay.tsv")
write.table(game_play, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)
```

```
## wrote ../games/2014_west/2014_west_gameplay.tsv
```

```r
out_file <- file.path(out_dir, "2014_west_gameplay.dput")
dput(game_play, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_west/2014_west_gameplay.dput
```

### Aggregate to the level of a possession.  
How does `poss_dat` differ from `game_play`, other than aggregation?

*  `n_events` = number of events
* `score` = logical indicating if possession ends with a goal
* `scor_team` = who scored ... `NA` if nobody did
* `who` = o_line vs. d_line


```r
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
```

```
## 'data.frame':	1439 obs. of  16 variables:
##  $ game     : Factor w/ 15 levels "2014-04-12_vanNH-at-pdxST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ period   : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ point    : int  1 1 2 3 4 4 5 6 6 7 ...
##  $ pull_team: Factor w/ 4 levels "vanNH","pdxST",..: 1 1 1 2 1 1 1 2 2 2 ...
##  $ poss_abs : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ poss_rel : int  1 2 1 1 1 2 1 1 2 1 ...
##  $ event    : int  7 13 8 5 8 14 11 13 17 9 ...
##  $ poss_team: Factor w/ 4 levels "vanNH","pdxST",..: 2 1 2 1 2 1 2 1 2 1 ...
##  $ pl_team  : chr  "vanNH" "vanNH" "pdxST" "vanNH" ...
##  $ pl_pnum  : chr  "81" "81" "6" "98" ...
##  $ pl_code  : chr  "D" "LG" "G" "G" ...
##  $ n_events : int  7 6 8 5 8 6 11 13 4 9 ...
##  $ huck     : logi  FALSE TRUE TRUE FALSE FALSE FALSE ...
##  $ score    : logi  FALSE TRUE TRUE TRUE FALSE TRUE ...
##  $ scor_team: Factor w/ 4 levels "vanNH","pdxST",..: NA 1 2 1 NA 1 2 NA 2 1 ...
##  $ who      : Factor w/ 2 levels "o_line","d_line": 1 2 1 1 1 2 1 1 2 1 ...
```

Sanity check and explore `poss_dat`. *TO DO*: rigorously check against known
final scores.


```r
(tmp <- ddply(poss_dat, ~ game,
              function(x) with(subset(x, score), table(scor_team))))
```

```
##                         game vanNH pdxST sfoDF seaRM
## 1  2014-04-12_vanNH-at-pdxST    21    23     0     0
## 2  2014-04-20_sfoDF-at-vanNH    26     0    15     0
## 3  2014-04-26_vanNH-at-seaRM    15     0     0    20
## 4  2014-05-10_seaRM-at-vanNH    29     0     0    23
## 5  2014-05-17_vanNH-at-sfoDF    12     0    13     0
## 6  2014-05-24_pdxST-at-vanNH    16    19     0     0
## 7  2014-05-31_vanNH-at-seaRM    29     0     0    24
## 8  2014-06-07_seaRM-at-vanNH    27     0     0    12
## 9  2014-06-15_pdxST-at-vanNH    20    18     0     0
## 10 2014-06-21_vanNH-at-sfoDF    17     0    15     0
## 11 2014-06-28_vanNH-at-pdxST    14    13     0     0
## 12 2014-04-12_seaRM-at-sfoDF     0     0    18    16
## 13 2014-04-19_sfoDF-at-seaRM     0     0    20    21
## 14 2014-04-26_pdxST-at-sfoDF     0    18    16     0
## 15 2014-05-04_sfoDF-at-seaRM     0     0    16    18
```

```r
colSums(subset(tmp, select = -game))
```

```
## vanNH pdxST sfoDF seaRM 
##   226    91   113   134
```

```r
addmargins(with(poss_dat, table(who, score)))
```

```
##         score
## who      FALSE TRUE  Sum
##   o_line   560  354  914
##   d_line   315  210  525
##   Sum      875  564 1439
```

Harmonize factor levels for `scor_team` with those of other team factor
variables.


```r
poss_dat <- transform(poss_dat , scor_team = factor(scor_team, levels = jTeams))
str(poss_dat)
```

```
## 'data.frame':	1439 obs. of  16 variables:
##  $ game     : Factor w/ 15 levels "2014-04-12_vanNH-at-pdxST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ period   : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ point    : int  1 1 2 3 4 4 5 6 6 7 ...
##  $ pull_team: Factor w/ 4 levels "vanNH","pdxST",..: 1 1 1 2 1 1 1 2 2 2 ...
##  $ poss_abs : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ poss_rel : int  1 2 1 1 1 2 1 1 2 1 ...
##  $ event    : int  7 13 8 5 8 14 11 13 17 9 ...
##  $ poss_team: Factor w/ 4 levels "vanNH","pdxST",..: 2 1 2 1 2 1 2 1 2 1 ...
##  $ pl_team  : chr  "vanNH" "vanNH" "pdxST" "vanNH" ...
##  $ pl_pnum  : chr  "81" "81" "6" "98" ...
##  $ pl_code  : chr  "D" "LG" "G" "G" ...
##  $ n_events : int  7 6 8 5 8 6 11 13 4 9 ...
##  $ huck     : logi  FALSE TRUE TRUE FALSE FALSE FALSE ...
##  $ score    : logi  FALSE TRUE TRUE TRUE FALSE TRUE ...
##  $ scor_team: Factor w/ 4 levels "vanNH","pdxST",..: NA 1 2 1 NA 1 2 NA 2 1 ...
##  $ who      : Factor w/ 2 levels "o_line","d_line": 1 2 1 1 1 2 1 1 2 1 ...
```

If possession ends due to end of period, set `pl_code` to `eop`.


```r
poss_dat <- ddply(poss_dat, ~ game + point, function(x) {
  n <- nrow(x)
  if(!x$score[n]) x$pl_code[n] <- 'eop'
  return(x)
})
```

Groom `pl_code` and create derivatives, so they give explicit information on
how possessions end, at different levels of detail. Also reorder levels by 
frequency, with most frequent code appearing first.


```r
poss_dat$pl_code <-
  mapvalues(poss_dat$pl_code, # revalue() won't work due to factor level ''
            from = c(  '', 'PU',  'L'),
            to   = c('TA', 'TA', 'TA'))
poss_dat$pl_code <- with(poss_dat, reorder(pl_code, pl_code, neglength))
as.data.frame(table(poss_dat$pl_code, dnn = "a_code"))
```

```
##    a_code Freq
## 1       G  445
## 2      TA  325
## 3       D  271
## 4      TD  158
## 5      LG  119
## 6     eop   56
## 7     VTT   22
## 8     VST   17
## 9      HB   16
## 10  off F    6
## 11     FB    4
```

```r
poss_dat$a_code <- # a_code is coarser than pl_code but still detailed
  mapvalues(poss_dat$pl_code,
            from = c('D', 'HB', 'FB', 'G', 'LG'),
            to   = c('D',  'D',  'D', 'G',  'G'))
poss_dat$a_code <- with(poss_dat, reorder(a_code, a_code, neglength))
as.data.frame(table(poss_dat$a_code, dnn = "a_code"))
```

```
##   a_code Freq
## 1      G  564
## 2     TA  325
## 3      D  291
## 4     TD  158
## 5    eop   56
## 6    VTT   22
## 7    VST   17
## 8  off F    6
```

```r
poss_dat$b_code <- #b_code is the coarsest
  mapvalues(poss_dat$a_code,
            from = c(    'D',   'TA',    'TD',   'VTT',   'VST', 'off F'),
            to   = c('def +','off -', 'off -', 'off -', 'off -', 'off -'))
poss_dat$b_code <- with(poss_dat, reorder(b_code, b_code, neglength))
as.data.frame(table(poss_dat$b_code, dnn = "b_code"))
```

```
##   b_code Freq
## 1      G  564
## 2  off -  528
## 3  def +  291
## 4    eop   56
```

Write `poss_dat` to file.


```r
out_file <- file.path(out_dir, "2014_west_possessions.rds")
saveRDS(poss_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_west/2014_west_possessions.rds
```

```r
out_file <- file.path(out_dir, "2014_west_possessions.tsv")
write.table(poss_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)
```

```
## wrote ../games/2014_west/2014_west_possessions.tsv
```

```r
out_file <- file.path(out_dir, "2014_west_possessions.dput")
dput(poss_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_west/2014_west_possessions.dput
```

### Aggregate to the level of a point 
How does `point_dat` differ from `poss_dat`, other than aggregation?

*  Just know that `n_events` is not the number of the events of the last
possession, but rather is computed from `poss_dat$event` and gives the number
of events in the whole point.


```r
point_dat <- ddply(poss_dat, ~ game + point, function(x) {
  n <- nrow(x)
  x$n_events <- NULL
  x <- rename(x, c("event" = "n_events", "poss_rel" = "n_poss"))
  x[n, ]
})
str(point_dat)
```

```
## 'data.frame':	620 obs. of  17 variables:
##  $ game     : Factor w/ 15 levels "2014-04-12_vanNH-at-pdxST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ period   : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ point    : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ pull_team: Factor w/ 4 levels "vanNH","pdxST",..: 1 1 2 1 1 2 2 1 2 1 ...
##  $ poss_abs : int  2 3 4 6 7 9 10 11 12 13 ...
##  $ n_poss   : int  2 1 1 2 1 2 1 1 1 1 ...
##  $ n_events : int  13 8 5 14 11 17 9 14 7 8 ...
##  $ poss_team: Factor w/ 4 levels "vanNH","pdxST",..: 1 2 1 1 2 2 1 2 1 2 ...
##  $ pl_team  : chr  "vanNH" "pdxST" "vanNH" "vanNH" ...
##  $ pl_pnum  : chr  "81" "6" "98" "13" ...
##  $ pl_code  : Factor w/ 11 levels "G","TA","D","TD",..: 5 1 1 1 1 5 1 1 1 1 ...
##  $ huck     : logi  TRUE TRUE FALSE FALSE FALSE TRUE ...
##  $ score    : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ scor_team: Factor w/ 4 levels "vanNH","pdxST",..: 1 2 1 1 2 2 1 2 1 2 ...
##  $ who      : Factor w/ 2 levels "o_line","d_line": 2 1 1 2 1 2 1 1 1 1 ...
##  $ a_code   : Factor w/ 8 levels "G","TA","D","TD",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ b_code   : Factor w/ 4 levels "G","off -","def +",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
table(point_dat$score, useNA = "always")
```

```
## 
## FALSE  TRUE  <NA> 
##    56   564     0
```

```r
table(point_dat$pl_code, useNA = "always")
```

```
## 
##     G    TA     D    TD    LG   eop   VTT   VST    HB off F    FB  <NA> 
##   445     0     0     0   119    56     0     0     0     0     0     0
```

```r
table(point_dat$a_code, useNA = "always")
```

```
## 
##     G    TA     D    TD   eop   VTT   VST off F  <NA> 
##   564     0     0     0    56     0     0     0     0
```

```r
table(point_dat$b_code, useNA = "always")
```

```
## 
##     G off - def +   eop  <NA> 
##   564     0     0    56     0
```

```r
table(point_dat$huck, useNA = "always")
```

```
## 
## FALSE  TRUE  <NA> 
##   402   218     0
```

```r
addmargins(with(point_dat, table(score, huck)))
```

```
##        huck
## score   FALSE TRUE Sum
##   FALSE    52    4  56
##   TRUE    350  214 564
##   Sum     402  218 620
```

```r
out_file <- file.path(out_dir, "2014_west_points.rds")
saveRDS(point_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_west/2014_west_points.rds
```

```r
out_file <- file.path(out_dir, "2014_west_points.tsv")
write.table(point_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)
```

```
## wrote ../games/2014_west/2014_west_points.tsv
```

```r
out_file <- file.path(out_dir, "2014_west_points.dput")
dput(point_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_west/2014_west_points.dput
```


---
title: "11_marshal-game-play-and-aggregate.r"
author: "jenny"
date: "Wed Jul  2 22:37:46 2014"
---
