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
    "2014-06-07_seaRM-at-vanNH",
    "Official Statistics - 2014 - Week 10 - 6/15 - Stags @ Nighthawks",
    "2014-06-15_pdxST-at-vanNH",
    "Official Statistics - 2014 - Week 11 - 6/21 - Nighthawks @ Dogfish",
    "2014-06-21_vanNH-at-sfoDF",
    "Official Statistics - 2014 - Playoffs - 6/28 - Nighthawks @ Stags",
    "2014-06-28_vanNH-at-pdxST",
    "Official Statistics - 2014 - Week 1 - 4/12 - Rainmakers @ Dogfish",
    "2014-04-12_seaRM-at-sfoDF",
    "Official Statistics - 2014 - Week 2 - 4/19 - Dogfish @ Rainmakers",
    "2014-04-19_sfoDF-at-seaRM",
    "Official Statistics - 2014 - Week 3 - 4/26 - Stags @ Dogfish",
    "2014-04-26_pdxST-at-sfoDF",
    "Official Statistics - 2014 - Week 4 - 5/4 - Dogfish @ Rainmakers",  
    "2014-05-04_sfoDF-at-seaRM")
game_ids <- matrix(game_ids, ncol = 2, byrow = TRUE)

#make_pre <- 'make r_bits'
make_pre <- 'make clean_game'
#make_pre <- 'make clean_resolve'
#make_pre <- 'make resolve_game'
make_args <- paste0("GOOGAME='\"", game_ids[,1], "\"' GAME=", game_ids[,2])
make_command <- paste(make_pre, make_args)

for(i in seq_along(make_command)) {
  system(make_command[i])
}

