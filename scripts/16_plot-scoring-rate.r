library(ggplot2)
library(plyr)
library(reshape2)

## useful inside reorder(), to invert the resulting factor levels
neglength <- function(x) -1 * length(x)

jWidth <- 5
jHeight <- 4

mlu_cols <-
  c(pdxST = "#4DB870", vanNH = "#CCCCCC", seaRM = "#88A5C3", sfoDF = "#FFAD5C")
theme_set(theme_bw())

input_dir <- file.path("..", "games", "2014_west")
point_file <- file.path(input_dir, "2014_west_points.rds")
str(point_dat <- readRDS(point_file), give.attr = FALSE) # 589 obs. of  17 vars

## learn the date, home team, away team from the game ID
game_dat <- with(point_dat,
                 data.frame(game = levels(game),
                            colsplit(levels(game), "_", c("date", "matchup"))))
game_dat <- with(game_dat,
                 data.frame(game, date,
                            colsplit(matchup, "-at-", c("away", "home"))))
point_dat <- join(point_dat, game_dat[c('game', 'away', 'home')])
str(point_dat) # 589 obs. of  19 vars

## determine the receiving team
point_dat <- ddply(point_dat, ~ game, function(x) {
  x <- droplevels(x)
  the_teams <- levels(x$pull_team)
  data.frame(x, recv_team = the_teams[ifelse(unclass(x$pull_team) == 1, 2, 1)])
})
str(point_dat)

hold_rate <- ddply(point_dat, ~ recv_team, function(x) {
  recv_team <- as.character(x$recv_team)[1]
  y <- as.data.frame(table(x$scor_team, useNA = "always", dnn = "scor_team"))
  y <- mutate(y, who_scores = ifelse(as.character(y$scor_team) == recv_team,
                                     "self", "opp"))
  y$who_scores[is.na(y$who_scores)] <- "nobody"
  z <- aggregate(Freq ~ who_scores, y, sum)
  z <- rbind(z, data.frame(who_scores = "sum", Freq = sum(y$Freq)))
  z$prop <- with(z, Freq / sum(y$Freq))
  z
})
hold_rate <-
  mutate(hold_rate,
         who_scores = factor(who_scores,
                             levels = c('sum', 'self', 'opp', 'nobody')),
         recv_team = reorder(recv_team, prop, function(x) -1 * rev(sort(x))[2]))

p <- ggplot(subset(hold_rate, who_scores != "sum"),
            aes(x = who_scores, y = prop, fill = recv_team))
p <- p + geom_bar(stat = "identity", width = 0.9, position = "dodge") +
  scale_fill_manual(values = mlu_cols) + xlab("who scores?") +
  labs(fill = "receiving team") +
  geom_text(aes(label = sprintf("%1.2f", prop)),
            position = position_dodge(0.9), vjust = -0.2, size = 2.5) +
  theme(legend.position = c(1, 1), legend.justification = c(0.88, 0.9),
        legend.background = element_rect(fill = 0))
p
out_file <- "barchart_who_scores_by_recv_team.png"
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)

hold_rate <- ddply(point_dat, ~ recv_team, function(x) {
  data.frame(hold_rate = with(x, mean(scor_team == recv_team, na.rm = TRUE)))
})



