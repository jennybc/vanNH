library(plyr)
library(ggplot2)

theme_set(theme_bw())
mlu_teams <- read.delim(file.path("..", "data", "mlu-teams.tsv"),
                        stringsAsFactors = FALSE)
mlu_cols <- with(mlu_teams, setNames(color, team))

in_dir <- file.path("..", "games", "2014_all-games")
poss_file <- file.path(in_dir, "2014_possessions.rds")
poss_dat <- readRDS(poss_file)
message(nrow(poss_dat), " rows of possession data found")

poss_table <- as.data.frame(with(poss_dat, table(n_passes, b_code)))
poss_table <- mutate(poss_table,
                     n_passes = as.numeric(as.character(n_passes)))
poss_table <- subset(poss_table, Freq > 0 & b_code != 'eop')
poss_table
str(poss_table)

## barchart of # passes in possession
## facetted by how possession ends: goal, off -, def +
p <- ggplot(poss_table, aes(x = n_passes, y = Freq))
p + geom_bar(stat = "identity", width = 0.5) + facet_grid(b_code ~ .)

## stripplot of # passes in possession
## grouped and boxplotted by how possession ends: goal, off -, def +
p <- ggplot(poss_dat, aes(x = b_code, y = n_passes))
p + geom_boxplot(outlier.colour = NA, width = 0.6) +
  geom_jitter(alpha = 1/5, size = 1) + coord_cartesian(ylim = c(0, 20))
  
poss_table_by_team <-
  as.data.frame(with(poss_dat, table(n_passes, b_code, poss_team)))
poss_table_by_team <-
  droplevels(subset(poss_table_by_team, Freq > 0 & b_code != 'eop'))
poss_table_by_team <- mutate(poss_table_by_team,
                            n_passes = as.numeric(as.character(n_passes)))
poss_table_by_team
str(poss_table_by_team)

## barchart of # passes in possession
## facetted by vanNH/wdcCT * how possession ends: goal, off -, def +
p <- ggplot(poss_table_by_team, aes(x = n_passes, y = Freq, fill = b_code))
p + geom_bar(stat = "identity", width = 0.5) + facet_grid(b_code ~ poss_team) +
  guides(fill = FALSE)
## this plot doesn't really work with 8 teams

## boxplot of number of passes in possession, with teams ordered by median
## number of passes per possession
tmp <- subset(poss_dat, b_code != "eop")
tmp$poss_team <- with(tmp, reorder(poss_team, n_passes, median))
p <- ggplot(tmp,
            aes(x = poss_team, y = n_passes, fill = poss_team))
p + geom_boxplot(outlier.colour = NA, width = 0.6) +
  geom_jitter(alpha = 1/5, size = 1) + scale_fill_manual(values = mlu_cols) +
  xlab("possessing team") + ylab("# passes in possession") +
  coord_cartesian(ylim = c(-1, 23)) +
  guides(fill = FALSE)
