## when run in batch mode, provide game identifier on command line
options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  #game <- "2014-04-20_sfoDF-at-vanNH"
  game <- "2014-05-24_pdxST-at-vanNH"
} else {
  game <- options[1]
}

game_dir <- file.path("..", "games", game)

if(file.exists(game_dir)) {
  stop("Trying to initialize game directory at:\n",
       normalizePath(game_dir),
       "\nbut directory already exists. Exiting.")
} else {
  dir.create(game_dir)
}

dirs_to_create <-
  file.path(game_dir, c("01_rawGoogleExtract", "03_concatGoogleExtract",
                        "04_cleanedGame", "05_htmlArchive"))
lapply(dirs_to_create, dir.create)

writeLines("game_status: upcoming",
           file.path(game_dir, paste0(game, "_game-info.yaml")))
