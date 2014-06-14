library(stringr)

fileDat <-
  data.frame(game_files = I(list.files("../games",
                                       full.names = TRUE, recursive = TRUE)))
str(fileDat) # 423 obs.

fileDat$lowDirs <- with(fileDat, basename(dirname(game_files)))
str(fileDat)

fileDat$delete_me <- with(fileDat, )
grepl("cleaned", game_dirs)
