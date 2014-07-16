library(ggplot2)
library(plyr)

## useful inside reorder(), to invert the resulting factor levels
neglength <- function(x) -1 * length(x)

mlu_cols <-
  c(pdxST = "#4DB870", vanNH = "#CCCCCC", seaRM = "#88A5C3", sfoDF = "#FFAD5C",
    wdcCT = "orange", bosWC = "blue", phlSP = "red", nykRM = "gold")
theme_set(theme_bw())

jWidth <- 5
jHeight <- 4

input_dir <- file.path("..", "games", "2014_all-games")
poss_file <- file.path(input_dir, "2014_possessions.rds")
str(poss_dat <- readRDS(poss_file), give.attr = FALSE) # 2830 obs. of  18 variables:

poss_dat$c_code <-
  revalue(poss_dat$b_code,
          c("G" = "score", "off -" = "turnover", "def +" = "turnover"))

foo <- ddply(subset(poss_dat, c_code != "eop"),
             ~ poss_team, summarize, med_poss_length = median(n_events))
p <- ggplot(subset(poss_dat, c_code != "eop"),
            aes(x = poss_team, y = n_events, fill = poss_team))
p <- p + geom_boxplot(outlier.colour = NA, width = 0.6) +
  geom_jitter(alpha = 1/5, size = 1) + coord_cartesian(ylim = c(0, 23)) +
  scale_fill_manual(values = mlu_cols) + guides(fill = FALSE) +
  xlab("possessing team") + ylab("# events in possession")
p2 <- p + geom_text(data = foo, aes(x = poss_team,
                                   y = med_poss_length, label = med_poss_length),
                   vjust = -0.4)
p2
out_file <- "dot_and_boxplot_events_per_possession_by_poss_team.png"
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p2, width = jWidth, height = jHeight)

q <- p + facet_wrap(~ c_code)
q
out_file <- "dot_and_boxplot_events_per_possession_by_poss_team_and_score.png"
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, q, width = jWidth, height = jHeight)

p <- ggplot(subset(poss_dat, c_code != "eop"),
            aes(x = b_code, y = n_events, fill = poss_team))
p <- p + geom_boxplot(outlier.colour = NA, width = 0.6,
                      position = position_dodge(0.6)) +
  geom_jitter(alpha = 1/5, size = 1) + ylim(0, 20) +
  scale_fill_manual(values = mlu_cols) + 
  xlab("how possession ends") + ylab("# events in possession") +
  labs(fill = "who's on offense?") +
  theme(legend.position = c(0.5, 1), legend.justification = c(0.5, 0.93),
        legend.background = element_rect(fill = 0),
        legend.direction = "horizontal")
p
out_file <- "dot_and_boxplot_events_per_possession_by_poss_team_and_b_code.png"
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)

## look at possession length o_line vs d_line?

## peeking at hucks ... nothing learned or saved from this yet
p <- ggplot(poss_dat, aes(y = n_events, x = poss_team, fill = huck))
p + geom_boxplot() + geom_jitter(alpha = 1/4) + facet_wrap(~ score)

p <- ggplot(poss_dat, aes(x = poss_team, fill = huck))
p + geom_bar() + facet_wrap(~ score)



# out_file <- paste0("barchart_how_possessions_end_",
#                    if(j_code == "b_code") "coarse" else "detailed",
#                    "_by_line_and_poss_team.png")
# out_file <- file.path("..", "web", "figs", out_file)
# ggsave(out_file, p, width = jWidth, height = jHeight)
