rosters_2014 <- dir(file.path("..", "rosters"), full.names = TRUE)
rosters_2015 <- gsub("2014", "2015", rosters_2014)
cbind(rosters_2014, rosters_2015)
copy_res <- file.copy(from = rosters_2014, to = rosters_2015)
all(copy_res)
