info_file <- file.path("..", "data", "2014_mlu-game-info.tsv")
game_info <- read.delim(info_file, stringsAsFactors = FALSE)

keeper_games <- intersect(list.files(file.path("..", "games")), game_info$game)

game_info <- game_info[match(keeper_games, game_info$game), ]

#make_pre <- 'make r_bits'
#make_pre <- 'make cat_goog'
#make_pre <- 'make clean_game'
#make_pre <- 'make clean_clean'
#make_pre <- 'make check_clean'
#make_pre <- 'make clean_resolve'
make_pre <- 'make possess_game'
make_args <- paste0("GOOGAME='\"", game_info$gspread_name,
                    "\"' GAME=", game_info$game)
make_command <- paste(make_pre, make_args)

for(i in seq_along(make_command)) {
  system(make_command[i])
}
