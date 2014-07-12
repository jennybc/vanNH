library(stringr)
library(plyr)

pass_files <- list.files(file.path("..", "games"), recursive = TRUE,
                         full.names = TRUE, pattern = "passes.tsv")
names(pass_files) <-
  apply(str_split_fixed(basename(pass_files), "_", 3)[ , 1:2],
        1, paste, collapse = "_")
pass_dat <-
  ldply(pass_files, function(gg) read.delim(gg,
                              colClasses = list(beg_plyr = "character",
                                                innards = "character",
                                                end_plyr = "character")),
        .id = "game")
str(pass_dat) # 7295 obs. of  13 variables

## create a data.frame `white_list` to match (beg_code, end_code, innards)
## against; will white-list anything I have seen and approved before; does not
## determine if something is a pass attempt or a completion, etc. but rather
## helps to detect impossible game play or game play that my scripts have
## garbled
source("07_white-list-pass-candidates.r")

## create a logical variable in pass_dat to indicate if each row's pass data is
## on the white list
pass_dat <- name_rows(pass_dat)
is_white_listed <-
  suppressMessages(as.integer(rownames(match_df(pass_dat, white_list))))
pass_dat <- name_rows(pass_dat)
tmp <- rep_len(FALSE, nrow(pass_dat))
tmp[is_white_listed] <- TRUE
pass_dat$wl <- tmp
message(sprintf("%d/%d rows of pass data are whitelisted (%.1f%%)",
                sum(pass_dat$wl), nrow(pass_dat), 100 * mean(pass_dat$wl), ""))
if(!all(pass_dat$wl)) {
  message(sprintf("%d rows of pass data are novel", sum(!pass_dat$wl)))
  pass_dat[!pass_dat$wl, ]
}

table(pass_dat$end_code, useNA = "always")
#   D-D O-CTH   O-G  O-OV  O-TD  O-TO   O-F  O-PU  <NA> 
#   313  5808   564    41   163    53     7     5   341 

## look up PU as end code
subset(pass_dat, end_code == 'O-PU',
       select = c(game, point, beg_event, beg_code, beg_plyr,
                  innards, end_code, end_plyr))
## these 5 remain; make sure handled correctly once I resolve completions, etc
# game point beg_event beg_code beg_plyr
# 3714 2014-05-10_seaRM-at-vanNH    41        11     O-PU seaRM-29
# 4405 2014-05-24_pdxST-at-vanNH    12        16    O-CTH  pdxST-7
# 4693 2014-05-24_pdxST-at-vanNH    34        25    O-CTH pdxST-42
# 6321 2014-06-15_pdxST-at-vanNH    34        14    O-CTH pdxST-24
# 7020 2014-06-28_vanNH-at-pdxST    13         4    O-CTH vanNH-22
# innards end_code end_plyr
# 3714 D-F,O-SO,D-SO,O-SI,D-SI     O-PU  seaRM-4
# 4405               O-SO,O-SI     O-PU  pdxST-7
# 4693           D-F,D-SO,D-SI     O-PU pdxST-42
# 6321           D-F,D-SO,D-SI     O-PU pdxST-24
# 7020                    O-VP     O-PU vanNH-22

## look at n_inn for end_code = NA
with(subset(pass_dat, is.na(end_code)), table(desc, n_inn))
#    n_inn
# desc  -1   0
#   an 338   3
#   ao   0   0
#   aa   0   0
## what's up with an but n_inn = 0 or 1?
subset(pass_dat, is.na(end_code) & n_inn >= 0)
## in all cases, there is a def foul recorded at end of the possession
# 2014-04-26_vanNH-at-seaRM 11 this is a throwaway, with a D-F just prior
# 2014-05-17_vanNH-at-sfoDF 23 ditto
# 2014-05-31_vanNH-at-seaRM 35 ditto
## if there are dual offensive and defensive fouls but the offensive one causes
## a turnover, it's important to ensure, via game play recording and processing
## scripts, that the offensive foul is last, so it is correctly detected as the
## possession ending event

subset(pass_dat, desc == 'ao' & n_inn == 0 & beg_code == 'O-CTH' &
         beg_plyr == end_plyr & end_code == 'O-OV',
       select = c(game, point, beg_code, beg_plyr, end_code, end_plyr))
##                           game point beg_code beg_plyr end_code end_plyr
## 2145 2014-04-26_pdxST-at-sfoDF    19    O-CTH  pdxST-2     O-OV  pdxST-2
## necessary because the catch annotated with 'L'
## lesson: {O-CTH, O-PU} O-OV is only a pass if player numbers not same

## desc = an n_inn = -1
## this is what a throwaway looks like: O-CTH NA, O-PU NA
## I want to check that the beg and end event are same
nrow(foo <- subset(pass_dat, desc == 'an' & n_inn == -1)) # 338
with(foo, table(beg_event == end_event)) # 338 TRUE

## this can happen: O-CTH O-VP O-PU (with beg and end being same player)
## 2014-06-28_vanNH-at-pdxST point 13
## berezan catches, someone else on offense picks, we move bwards, berezan throws
subset(pass_dat, innards == 'O-F',
       select = c(game, point, beg_code, beg_plyr, innards, end_code, end_plyr))
## 2014-05-24_pdxST-at-vanNH point 33 this is legit: pdxST-8 has the disc,
## there's a contact foul on the offense, disc goes to that spot (backwards),
## pdxST-8 then throws to pdxST-73; O-PU, O-F, O-CTH
## 2014-05-31_vanNH-at-seaRM point 10 this is looks legit: vanNH-19 has the disc,
## there's some foul on the offense, vanNH-19 eventually throws to vanNH-89

pass_tally <- as.data.frame(with(subset(pass_dat, desc == 'an'),
                                 table(paste(beg_code, end_code))))
pass_tally$Var1 <- with(pass_tally, reorder(Var1, Freq))
pass_tally <- arrange(pass_tally, desc(Freq))
pass_tally

source("07_pass-map.r")

str(pass_dat)
tmp <- join(pass_dat, pass_map)
str(tmp)
table(tmp$pclass, useNA = "always")
#  compl ofoul taway  defd  drop  <NA> 
#   6372     7   341   313   163    99 

tmp$pclass <- factor(tmp$pclass, levels = c(levels(tmp$pclass), "nopass"))

with(subset(tmp, is.na(pclass)), table(paste(beg_code, end_code)))
# O-CTH O-OV O-CTH O-PU O-CTH O-TO  O-PU O-OV  O-PU O-PU  O-PU O-TO 
#         27          4         37         14          1         16 

## special handling for {O-CTH, O-PU} {O-TO, O-OV}
fill_me <-
  with(tmp, beg_code %in% c('O-CTH', 'O-PU') & end_code %in% c('O-TO', 'O-OV'))
fill_me[is.na(fill_me)] <- FALSE
table(fill_me) # 7237 FALSE 58 TRUE
player_same <- with(tmp, beg_plyr == end_plyr)
table(player_same, useNA = "always") # 6931 FALSE 364 TRUE
tmp$pclass[fill_me & !player_same] <- "compl"
tmp$pclass[fill_me & player_same] <- "nopass"

with(subset(tmp, is.na(pclass)), table(paste(beg_code, end_code)))
# O-CTH O-PU  O-PU O-PU 
#          4          1 

## special handling for {O-CTH, O-PU} O-PU
## so far these seem to usually involve lots of subs off and in
## IRL, raise an alert so we look at these
fill_me <-
  with(tmp, beg_code %in% c('O-CTH', 'O-PU') & end_code == 'O-PU')
fill_me[is.na(fill_me)] <- FALSE
table(fill_me) # 7290 FALSE 5 TRUE
tmp$pclass[fill_me] <- "nopass"

table(tmp$pclass, useNA = "always")
#  compl  ofoul  taway   defd   drop nopass   <NA> 
#   6450      7    341    313    163     21      0 
sum(tmp$pclass == 'compl') / 
  sum(tmp$pclass %in% c('compl', 'ofoul', 'taway', 'defd', 'drop'))
