make_pre <- 'make pt_fix'

# googame <- "Official Statistics - 2014 - Week 6 - 5/17 - Nighthawks @ Dogfish"
# game <- "2014-05-17_vanNH-at-sfoDF"
googame <- "Official Statistics - 2014 - Week 7 - 5/24 - Stags @ Nighthawks"
game <- "2014-05-24_pdxST-at-vanNH"

game_args <- paste0("GOOGAME='\"", googame, "\"' GAME=", game)

pt_args <- paste0("POINT=", 32)

(make_command <- paste(make_pre, game_args, pt_args))

system(make_command)


game_ids <-
  c("Official Statistics - 2014 - Week 1 - 4/12 - Nighthawks @ Stags",
    "2014-04-12_vanNH-at-pdxST",
    "Official Statistics - 2014 - Week 2 - 4/20 - Dogfish @ Nighthawks",
    "2014-04-20_sfoDF-at-vanNH",
    "Official Statistics - 2014 - Week 3 - 4/26 - Nighthawks @ Rainmakers",
    "2014-04-26_vanNH-at-seaRM",
    "Official Statistics - 2014 - Week 5 - 5/10 - Rainmakers @ Nighthawks",
    "2014-05-10_seaRM-at-vanNH",
    "Official Statistics - 2014 - Week 6 - 5/17 - Nighthawks @ Dogfish",
    "2014-05-17_vanNH-at-sfoDF",
    "Official Statistics - 2014 - Week 7 - 5/24 - Stags @ Nighthawks",
    "2014-05-24_pdxST-at-vanNH",
    "Official Statistics - 2014 - Week 8 - 5/31 - Nighthawks @ Rainmakers",
    "2014-05-31_vanNH-at-seaRM",
    "Official Statistics - 2014 - Week 9 - 6/7 - Rainmakers @ Nighthawks",
    "2014-06-07_seaRM-at-vanNH")
game_ids <- matrix(game_ids, ncol = 2, byrow = TRUE)


