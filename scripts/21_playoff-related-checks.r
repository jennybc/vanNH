library(plyr)

input_dir <- file.path("..", "games", "2014_west")
poss_file <- file.path(input_dir, "2014_west_possessions.rds")
str(poss_dat <- readRDS(poss_file), give.attr = FALSE) # 1268 obs. of 18 vars
str(poss_dat$game) # Factor w/ 11 levels

## get rid of possessions that end with eop
poss_dat <- droplevels(subset(poss_dat, b_code != "eop"))
str(poss_dat) # 1219 obs. of  18 variables:
str(poss_dat$game)

hth_dat <-
  droplevels(subset(poss_dat, grepl("vanNH", game) & grepl("pdxST", game)))
str(hth_dat) # 225 obs. of  18 variables:
str(hth_dat$game) # Factor w/ 3 levels

write.table(hth_dat, "foo.tsv", quote = FALSE, sep = "\t", row.names = FALSE)

count_em_up <- function(code_var, x = poss_dat, cutoff = 0.08) {
  code_freq <- as.data.frame(addmargins(table(x[[code_var]], dnn = code_var)))
  code_freq[[code_var]] <- reorder(code_freq[[code_var]], -1 * code_freq$Freq)
  code_freq <-
    mutate(code_freq, prop = Freq / Freq[nrow(code_freq)],
           pretty_prop = ifelse(prop < cutoff, '', as.character(round(prop, 2))))
  return(code_freq)
}

j_code <- "b_code"
last_code_freq_by_team_and_game <-
  ddply(hth_dat, ~ game + poss_team + who, function(x) count_em_up(j_code, x))
