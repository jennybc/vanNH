library(stringr)

fileDat <-
  data.frame(game_files = I(list.files("../games",
                                       full.names = TRUE, recursive = TRUE)))
str(fileDat) # 423 obs.

fileDat$low_dirs <- with(fileDat, basename(dirname(game_files)))
str(fileDat)

fileDat$delete_me <- with(fileDat, grepl("04_cleanedGame", low_dirs) |
                            grepl("05_htmlArchive", low_dirs) |
                            grepl("at-last-point.yaml", low_dirs))

with(fileDat, file.remove(game_files[delete_me]))
