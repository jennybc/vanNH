#library(stringr)
library(plyr)

games <- list.files(file.path("..", "games"), pattern = "-at-")
pass_files <- list.files(file.path("..", "games", games), recursive = TRUE,
  full.names = TRUE, pattern = "passes.tsv")
identical(seq_along(games), laply(games, function(gg) grep(gg, pass_files)))
names(pass_files) <- games
pass_dat <-
  ldply(pass_files, function(gg) read.delim(gg,
                           colClasses = list(beg_plyr = "character",
                                             innards = "character",
                                             end_plyr = "character")),
                           .id = "game")
str(pass_dat) # 16032 obs. of  15 variables

levels(pass_dat$game)

x <- subset(pass_dat, game == "2014-05-17_wdcCT-at-nykRM")
str(x)
the_plyr <- "wdcCT-99"
y <- subset(x, beg_plyr == the_plyr | end_plyr == the_plyr)
z <-
  with(y,
       data.frame(goals = sum(end_code == 'O-G' & end_plyr == the_plyr,
                              na.rm = TRUE),
                  assists = sum(end_code == 'O-G' & beg_plyr == the_plyr,
                                na.rm = TRUE),
                  throws = sum(beg_plyr == the_plyr & pclass != 'nopass'),
                  completions = sum(beg_plyr == the_plyr & pclass == 'compl'),
                  catches = sum(end_plyr == the_plyr & pclass == 'compl',
                                na.rm = TRUE),
                  def = sum(end_plyr == the_plyr & pclass == 'defd',
                            na.rm = TRUE),
                  drop = sum(beg_plyr == the_plyr & pclass == 'drop')))
z <- mutate(z, points = goals + assists, comp_pct = completions / throws)
vars_how_i_want <- c('points', 'comp_pct', 'goals', 'assists',
                     'throws', 'completions', 'catches', 'def', 'drop')
z <- z[vars_how_i_want]
z
## non-essential stats I could add later:
## BE = bookend; player gets a D then scores goal later in same possession
## double happiness is special case of BE, when it's very next throw
## Callahans, Greatests (how would I even know about Greatests?)
## all the stuff that relies on systematic processing of who's on the line:
## points played (overal, on O, on D), plus/minus