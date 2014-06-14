## a one-off script to look at rows with game play for both teams

library(plyr)

games <- c("2014-04-12_vanNH-at-pdxST", "2014-04-20_sfoDF-at-vanNH",
           "2014-04-26_vanNH-at-seaRM", "2014-05-10_seaRM-at-vanNH",
           "2014-05-17_vanNH-at-sfoDF", "2014-05-24_pdxST-at-vanNH",
           "2014-05-31_vanNH-at-seaRM", "2014-06-07_seaRM-at-vanNH")
game_file <- file.path("..", "games", games, "04_cleanedGame",
                       paste0(games, "_gameplay-clean.tsv"))
names(game_file) <- games
sDat <- ldply(game_file, function(gg) {
  read.delim(gg,
             colClasses = list(pullNum = 'character',
                               pullCode = 'character',
                               recvNum = 'character',
                               recvCode = 'character'))
}, .id = "game")
str(sDat) # 5026 obs. of  10 variables

is_double_gp <- with(sDat, which(pullNum != "" & recvNum != ""))
length(is_double_gp) # 1!
rows_i_want <- sort(unique(unlist(lapply(is_double_gp,
                                         function(x)
                                           seq(from = x - 2, to = x + 2)))))
rows_i_want <- rows_i_want[rows_i_want > 0 & rows_i_want < nrow(sDat) + 1]
foo <- data.frame(rows_i_want, yo = c(1, diff(rows_i_want)))
foo$start <- foo$yo != 1
foo$clump <- cumsum(foo$start) + 1

oo <- options(width = 300)
(tmp <- dlply(foo, ~ clump, function(x) sDat[x$rows_i_want, ]))
options(oo)

head(tmp)
ldply(tmp, function(x) x[1, c('game', 'point')])

# clump                      game point
# 1      1 2014-04-12_vanNH-at-pdxST    13
# 2      2 2014-04-12_vanNH-at-pdxST    20
# 3      3 2014-04-12_vanNH-at-pdxST    22
# 4      4 2014-04-12_vanNH-at-pdxST    23
# 5      5 2014-04-12_vanNH-at-pdxST    34
# 6      6 2014-04-12_vanNH-at-pdxST    43
# 7      7 2014-04-20_sfoDF-at-vanNH     4
# 8      8 2014-04-20_sfoDF-at-vanNH     7
# 9      9 2014-04-20_sfoDF-at-vanNH    12
# 10    10 2014-04-20_sfoDF-at-vanNH    40
# 11    11 2014-04-26_vanNH-at-seaRM    14
# 12    12 2014-04-26_vanNH-at-seaRM    18
# 13    13 2014-04-26_vanNH-at-seaRM    18
# 14    14 2014-04-26_vanNH-at-seaRM    28
# 15    15 2014-04-26_vanNH-at-seaRM    31
# 16    16 2014-04-26_vanNH-at-seaRM    35
# 17    17 2014-05-10_seaRM-at-vanNH    24
# 18    18 2014-05-10_seaRM-at-vanNH    24
# 19    19 2014-05-10_seaRM-at-vanNH    31
# 20    20 2014-05-10_seaRM-at-vanNH    37
# 21    21 2014-05-10_seaRM-at-vanNH    39
# 22    22 2014-05-17_vanNH-at-sfoDF    13
# 23    23 2014-05-17_vanNH-at-sfoDF    13
# 24    24 2014-05-17_vanNH-at-sfoDF    17
# 25    25 2014-05-17_vanNH-at-sfoDF    22
# 26    26 2014-05-17_vanNH-at-sfoDF    26
# 27    27 2014-05-17_vanNH-at-sfoDF    29
# 28    28 2014-05-17_vanNH-at-sfoDF    30
# 29    29 2014-05-24_pdxST-at-vanNH     3
# 30    30 2014-05-24_pdxST-at-vanNH     8
# 31    31 2014-05-24_pdxST-at-vanNH    14
# 32    32 2014-05-24_pdxST-at-vanNH    28
# 33    33 2014-05-24_pdxST-at-vanNH    33
# 34    34 2014-05-24_pdxST-at-vanNH    38
# 35    35 2014-05-31_vanNH-at-seaRM    22
# 36    36 2014-05-31_vanNH-at-seaRM    23
# 37    37 2014-05-31_vanNH-at-seaRM    41
# 38    38 2014-05-31_vanNH-at-seaRM    43
# 39    39 2014-05-31_vanNH-at-seaRM    52
# 40    40 2014-05-31_vanNH-at-seaRM    55
# 41    41 2014-06-07_seaRM-at-vanNH    10
# 42    42 2014-06-07_seaRM-at-vanNH    22
# 43    43 2014-06-07_seaRM-at-vanNH    25