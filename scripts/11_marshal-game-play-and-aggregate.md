# My Title



### Bring in, concatenate, and write possession data.


```r
poss_files <- list.files(file.path("..", "games", games, "06_possess-game"),
                         pattern = "_possessions.tsv", full.names = TRUE)
names(poss_files) <- games
poss_dat <-
  ldply(poss_files, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(poss_dat) # 2830 obs. of  18 variables:
```

```
## 'data.frame':	2830 obs. of  18 variables:
##  $ game     : Factor w/ 32 levels "2014-04-12_seaRM-at-sfoDF",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ period   : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ point    : int  1 2 2 3 4 4 4 5 5 5 ...
##  $ pull_team: chr  "sfoDF" "seaRM" "seaRM" "seaRM" ...
##  $ event    : int  5 7 9 5 7 15 18 8 14 27 ...
##  $ poss_abs : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ poss_rel : int  1 1 2 1 1 2 3 1 2 3 ...
##  $ poss_team: chr  "seaRM" "sfoDF" "seaRM" "sfoDF" ...
##  $ pl_team  : chr  "seaRM" "sfoDF" "seaRM" "sfoDF" ...
##  $ pl_pnum  : chr  "48" "11" "9" "40" ...
##  $ pl_code  : chr  "LG" "TA" "G" "G" ...
##  $ n_events : int  5 7 2 5 7 8 3 8 6 13 ...
##  $ huck     : logi  TRUE FALSE FALSE FALSE FALSE FALSE ...
##  $ score    : logi  TRUE FALSE TRUE TRUE FALSE FALSE ...
##  $ scor_team: chr  "seaRM" NA "seaRM" "sfoDF" ...
##  $ who      : chr  "o_line" "o_line" "d_line" "o_line" ...
##  $ a_code   : chr  "G" "TA" "G" "G" ...
##  $ b_code   : chr  "G" "off -" "G" "G" ...
```

```r
out_dir <- file.path("..", "games", "2014_all-games")
if(!file.exists(out_dir)) dir.create(out_dir)

out_file <- file.path(out_dir, "2014_possessions.rds")
saveRDS(poss_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_possessions.rds
```

```r
out_file <- file.path(out_dir, "2014_possessions.tsv")
write.table(poss_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_possessions.tsv
```

```r
out_file <- file.path(out_dir, "2014_possessions.dput")
dput(poss_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_possessions.dput
```

### Bring in, concatenate, and write point data.


```r
point_files <- list.files(file.path("..", "games", games, "06_possess-game"),
                          pattern = "_points-resolved.tsv", full.names = TRUE)
names(point_files) <- games
point_dat <-
  ldply(point_files, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(point_dat) # 1314 obs. of  17 variables:
```

```
## 'data.frame':	1314 obs. of  17 variables:
##  $ game      : Factor w/ 32 levels "2014-04-12_seaRM-at-sfoDF",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ point     : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ period    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ clk_before: chr  "10:00:00" NA NA NA ...
##  $ clk_after : chr  NA NA NA NA ...
##  $ pull_team : chr  "sfoDF" "seaRM" "seaRM" "sfoDF" ...
##  $ n_poss    : int  1 2 1 3 3 4 1 1 5 2 ...
##  $ scor_team : chr  "seaRM" "seaRM" "sfoDF" "seaRM" ...
##  $ h_or_b    : chr  "hold" "break" "hold" "hold" ...
##  $ seaRM     : int  1 2 2 3 3 3 4 4 5 6 ...
##  $ sfoDF     : int  0 0 1 1 2 3 3 4 4 4 ...
##  $ pdxST     : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ vanNH     : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ phlSP     : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ wdcCT     : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ bosWC     : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ nykRM     : int  NA NA NA NA NA NA NA NA NA NA ...
```

```r
out_file <- file.path(out_dir, "2014_points.rds")
saveRDS(point_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_points.rds
```

```r
out_file <- file.path(out_dir, "2014_points.tsv")
write.table(point_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_points.tsv
```

```r
out_file <- file.path(out_dir, "2014_points.dput")
dput(point_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_points.dput
```

### Bring in, concatenate, and write pass data.


```r
pass_files <- list.files(file.path("..", "games", games, "07_pass-game"),
                          pattern = "_passes.tsv", full.names = TRUE)
names(pass_files) <- games
pass_dat <-
  ldply(pass_files, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(pass_dat) # 16033 obs. of  15 variables:
```

```
## 'data.frame':	16033 obs. of  15 variables:
##  $ game     : Factor w/ 32 levels "2014-04-12_seaRM-at-sfoDF",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ poss_abs : int  1 1 1 2 2 2 2 2 2 3 ...
##  $ poss_rel : int  1 1 1 1 1 1 1 1 1 2 ...
##  $ point    : int  1 1 1 2 2 2 2 2 2 2 ...
##  $ beg_event: int  2 3 4 2 3 4 5 6 7 8 ...
##  $ end_event: int  3 4 5 3 4 5 6 7 7 9 ...
##  $ desc     : chr  "ao" "ao" "ao" "ao" ...
##  $ n_inn    : int  0 0 0 0 0 0 0 0 -1 0 ...
##  $ beg_code : chr  "O-PU" "O-CTH" "O-CTH" "O-PU" ...
##  $ beg_plyr : chr  "seaRM-20" "seaRM-9" "seaRM-20" "sfoDF-11" ...
##  $ innards  : chr  "" "" "" "" ...
##  $ end_code : chr  "O-CTH" "O-CTH" "O-G" "O-CTH" ...
##  $ end_plyr : chr  "seaRM-9" "seaRM-20" "seaRM-48" "sfoDF-5" ...
##  $ wl       : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
##  $ pclass   : chr  "compl" "compl" "compl" "compl" ...
```

```r
out_file <- file.path(out_dir, "2014_passes.rds")
saveRDS(pass_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_passes.rds
```

```r
out_file <- file.path(out_dir, "2014_passes.tsv")
write.table(pass_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_passes.tsv
```

```r
out_file <- file.path(out_dir, "2014_passes.dput")
dput(pass_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_passes.dput
```

### Bring in, concatenate, and write player stats.


```r
ps_files <- list.files(file.path("..", "games", games, "07_pass-game"),
                       pattern = "_player-stats.tsv", full.names = TRUE)
names(ps_files) <- games
ps_dat <-
  ldply(ps_files, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(ps_dat) # 1400 obs. of  14 variables:
```

```
## 'data.frame':	1400 obs. of  14 variables:
##  $ game       : chr  "2014-04-12_seaRM-at-sfoDF" "2014-04-12_seaRM-at-sfoDF" "2014-04-12_seaRM-at-sfoDF" "2014-04-12_seaRM-at-sfoDF" ...
##  $ player     : chr  "seaRM-9" "seaRM-48" "seaRM-6" "seaRM-35" ...
##  $ last       : chr  "harkness" "clark" "trytiak" "koss" ...
##  $ points     : int  7 6 5 3 3 2 2 1 1 1 ...
##  $ comp_pct   : num  0.9 0.8 0.8 0.75 0.83 1 0.94 0.77 0.84 0.67 ...
##  $ goals      : int  5 6 1 1 1 1 0 1 0 0 ...
##  $ assists    : int  2 0 4 2 2 1 2 0 1 1 ...
##  $ throws     : int  21 5 15 24 6 17 35 22 49 3 ...
##  $ completions: int  19 4 12 18 5 17 33 17 41 2 ...
##  $ catches    : int  21 11 16 17 5 13 24 21 32 3 ...
##  $ def        : int  1 1 1 2 0 0 1 0 2 1 ...
##  $ drop       : int  1 1 0 2 0 0 2 3 3 0 ...
##  $ team       : chr  "seaRM" "seaRM" "seaRM" "seaRM" ...
##  $ number     : chr  "9" "48" "6" "35" ...
```

```r
out_file <- file.path(out_dir, "2014_player-game-stats.rds")
saveRDS(ps_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_player-game-stats.rds
```

```r
out_file <- file.path(out_dir, "2014_player-game-stats.tsv")
write.table(ps_dat, out_file, quote = FALSE, sep = "\t", row.names = FALSE)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_player-game-stats.tsv
```

```r
out_file <- file.path(out_dir, "2014_player-game-stats.dput")
dput(ps_dat, out_file)
message("wrote ", out_file)
```

```
## wrote ../games/2014_all-games/2014_player-game-stats.dput
```


---
title: "11_marshal-game-play-and-aggregate.r"
author: "jenny"
date: "Tue Jul 15 22:27:41 2014"
---
