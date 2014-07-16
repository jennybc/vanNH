library(ggplot2)
library(plyr)

## useful inside reorder(), to invert the resulting factor levels
neglength <- function(x) -1 * length(x)

jWidth <- 5
jHeight <- 4

mlu_cols <-
  c(pdxST = "#4DB870", vanNH = "#CCCCCC", seaRM = "#88A5C3", sfoDF = "#FFAD5C",
    wdcCT = "orange", bosWC = "blue", phlSP = "red", nykRM = "gold")
theme_set(theme_bw())

## function that counts how often each code occurs, computes as a proportion,
## and also forms a "pretty" proportion suitable for putting on a figure; also
## reorders the factor associated with the codes by the frequencies
count_em_up <- function(code_var, x = poss_dat, cutoff = 0.08) {
  code_freq <- as.data.frame(addmargins(table(x[[code_var]], dnn = code_var)))
  code_freq[[code_var]] <- reorder(code_freq[[code_var]], -1 * code_freq$Freq)
  code_freq <-
    mutate(code_freq, prop = Freq / Freq[nrow(code_freq)],
           pretty_prop = ifelse(prop < cutoff, '', as.character(round(prop, 2))))
  return(code_freq)
}

input_dir <- file.path("..", "games", "2014_all-games")
poss_file <- file.path(input_dir, "2014_possessions.rds")
str(poss_dat <- readRDS(poss_file), give.attr = FALSE) # 2830 obs. of  18 variables:

## make poss_team into a factor
# j_teams <- c('wdcCT', 'bosWC', 'phlSP', 'nykRM',
#              'vanNH', 'pdxST', 'seaRM', 'sfoDF')
j_teams <- c('wdcCT', 'vanNH', 'bosWC', 'pdxST',
             'phlSP', 'seaRM', 'nykRM', 'sfoDF')
poss_dat <- mutate(poss_dat, poss_team = factor(poss_team, levels = j_teams))

## loop over the coarse b_code or more detailed a_code; both codes capture how
## the possession ends

for(j_code in c("b_code", "a_code")) {
  
  #j_code <- "b_code"
  #j_code <- "a_code"
  
if(j_code == "b_code") {
  w1 <- 0.5 # width for marginal barchart re: code only
  at <- 1 # size of x-axis tick labels relative to theme base font size
  s2 <- 4 # text size re: o line vs d line
  s4 <- 1.8 # text size re: poss_team AND o line vs d line
} else {
  w1 <- 0.8 # the marginal barchart re: code only
  at <- 0.6 # size of x-axis tick labels relative to theme base font size
  s2 <- 2 # text size re: o line vs d line
  s4 <- 1.2 # text size re: poss_team AND o line vs d line
}

## how do possessions end?
(last_code_freq <- count_em_up(j_code))
p <- ggplot(last_code_freq, aes_string(x = j_code, y = "Freq")) +
  geom_bar(stat = "identity", width = w1) +
  xlab("how possessions end") + ylab("no. of possessions") +
  geom_text(aes(label = Freq), vjust = -0.2, size = 4) +
  geom_text(aes(label = pretty_prop), vjust = 1.5, size = 4, col = "white") +
  theme(axis.text.x = element_text(size = rel(at)))
p
out_file <- paste0("barchart_how_possessions_end_",
                   if(j_code == "b_code") "coarse" else "detailed",
                   ".png")
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)

## how do possessions end? O line vs D line
last_code_freq_by_line <-
  ddply(poss_dat, ~ who, function(x) count_em_up(j_code, x))
p <- ggplot(last_code_freq_by_line,
            aes_string(x = j_code, y = "Freq", fill = "who")) +
  geom_bar(stat = "identity", width = 0.9) + facet_wrap(~ who) +
  xlab("how possessions end") + ylab("no. of possessions") +
  geom_text(aes(label = Freq), vjust = -0.2, size = s2,
            position = position_dodge(0.9)) +
  geom_text(aes(label = pretty_prop), vjust = 1.5, size = s2, col = "white",
            position = position_dodge(0.9)) +
  theme(legend.position = c(1, 1), legend.justification = c(0.88, 0.85),
        legend.background = element_rect(fill = 0),
        axis.text.x = element_text(size = rel(at))) +
  labs(fill = "who's on offense?")
p
out_file <- paste0("barchart_how_possessions_end_",
                   if(j_code == "b_code") "coarse" else "detailed",
                   "_by_line.png")
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)

## how do possessions end? split out by poss_team
last_code_freq_by_team <-
  ddply(poss_dat, ~ poss_team, function(x) count_em_up(j_code, x))
p <- ggplot(last_code_freq_by_team,
            aes_string(x = j_code, y = "Freq")) +
  geom_bar(stat = "identity", width = 0.9) +
  facet_wrap(~ poss_team, scales = "free", ncol = 2) +
  xlab("how possessions end") + ylab("no. of possessions") +
  geom_text(aes(label = Freq), vjust = -0.2, size = 1.8,
            position = position_dodge(0.9)) +
  geom_text(aes(label = pretty_prop), vjust = 1.5, size = 1.8, col = "white",
            position = position_dodge(0.9)) +
  labs(fill = "who's on offense?") +
  theme(axis.text.x = element_text(size = rel(at)))
p
out_file <- paste0("barchart_how_possessions_end_",
                   if(j_code == "b_code") "coarse" else "detailed",
                   "_by_poss_team.png")
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)

## how do possessions end? O line vs D line AND by poss_team
last_code_freq_by_line_and_team <-
  ddply(poss_dat, ~ poss_team + who, function(x) count_em_up(j_code, x))
p <- ggplot(last_code_freq_by_line_and_team,
            aes_string(x = j_code, y = "Freq", fill = "who")) +
  geom_bar(stat = "identity", width = 0.9, position = "dodge") +
  facet_wrap(~ poss_team, scales="free", ncol = 2) +
  xlab("how possessions end") + ylab("no. of possessions") +
  geom_text(aes(label = Freq), vjust = -0.2, size = s4,
            position = position_dodge(0.9)) +
  geom_text(aes(label = pretty_prop), vjust = 1.5, size = s4, col = "white",
            position = position_dodge(0.9)) +
  labs(fill = "who's on offense?") +
  theme(legend.position = c(1, 1), legend.justification = c(0.88, 0.85),
        legend.background = element_rect(fill = 0),
        #legend.text = element_text(size = rel(0.2)),
        axis.text.x = element_text(size = rel(at)))
## nice to do: make legend less dominant visually dominant
p
out_file <- paste0("barchart_how_possessions_end_",
                   if(j_code == "b_code") "coarse" else "detailed",
                   "_by_line_and_poss_team.png")
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)

} # this ends the loop over b_code, a_code

## aggregate to points: record how many possessions, who scored (if anyone),
## and whether it was a hold or break
jFun <- function(x) {
  n <- nrow(x)
  pull_team <- as.character(x$pull_team[1])
  ## careful to accomodate a foul on the goal catch and to persist even if there
  ## are somehow two codes containing G (alert will be raised elsewhere; this is
  ## neither the time nor the place to clean a game)
  its_a_goal <- which(grepl("L*G", x$pl_code))
  if(length(its_a_goal) > 0) {
    goal_row <- max(its_a_goal)
    scor_team <- x$pl_team[goal_row]
    status <- ifelse(pull_team == scor_team, "break", "hold")
  } else {
    scor_team <- status <- NA
  }
  y <- with(x[n, ], data.frame(period, point, pull_team,
                               scor_team, status, n_poss = max(poss_rel)))
  return(y)
}
point_dat <- ddply(poss_dat, ~ game + point, jFun)
str(point_dat) # 1314 obs. of  7 variables:

## get rid of points that end with no goal
point_dat <- subset(point_dat, !is.na(status))
str(point_dat) # 1197 obs. of  7 variables:

## distribution of number of possessions
n_poss_freq <- ddply(point_dat, ~ n_poss + status, summarize,
                     n = length(point), prop = length(point) / nrow(point_dat))
n_poss_freq <-
  mutate(n_poss_freq,
         pretty_prop = ifelse(prop > 0.01, as.character(round(prop, 2)), ''),
         cum_prop = cumsum(prop),
         pretty_cum_prop = ifelse(cum_prop < 0.98,
                                  as.character(round(cum_prop, 2)), ''))
n_poss_freq     
str(n_poss_freq)

p <- ggplot(n_poss_freq, aes(x = n_poss, y = prop, fill = status))
p + geom_bar(stat = "identity") +
  geom_text(aes(label = pretty_prop), vjust = -0.2, size = 4) +
  scale_x_discrete(breaks = 1:17) +
  ylab("proportion of points scored after exactly x possessions") +
  xlab("x = number of possessions before point ends in a goal") +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = 0)) + labs(fill = "")
ggsave("../web/figs/poss_n_dist_by_status.png")

## now retain status AND scor_team
n_poss_freq <-
  ddply(point_dat, ~ scor_team + status + n_poss, summarize,
        n = length(point))
n_poss_freq
str(n_poss_freq)
n_poss_freq <- ddply(n_poss_freq, ~ scor_team, mutate, team_prop = n / sum(n))
n_poss_freq
str(n_poss_freq)
aggregate(team_prop ~ scor_team, n_poss_freq, sum)

p <- ggplot(n_poss_freq, aes(x = n_poss, y = team_prop, fill = status))
p + geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = (1:5)/10) + 
  scale_x_continuous(breaks = 1:17, limits = c(0, 13)) + 
  facet_wrap(~ scor_team) + 
  ylab("proportion of points scored after exactly x possessions") +
  xlab("x = number of possessions before point ends in a goal") +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = 0)) + labs(fill = "")
ggsave("../web/figs/poss_n_dist_by_scor_team_and_status.png")
