library(plyr)
library(ggplot2)

games <- c("2014-04-12_vanNH-at-pdxST", "2014-04-20_sfoDF-at-vanNH",
           "2014-04-26_vanNH-at-seaRM", "2014-05-10_seaRM-at-vanNH",
           "2014-05-17_vanNH-at-sfoDF", "2014-05-24_pdxST-at-vanNH",
           "2014-05-31_vanNH-at-seaRM", "2014-06-07_seaRM-at-vanNH")
game_file <- file.path("..", "games", games, "07_resolvedGame",
                       paste0(games, "_gameplay-resolved.tsv"))
names(game_file) <- games
gpDat <-
  ldply(game_file, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(gpDat) # 5028 obs. of  8 variables

## set factor levels uniformly for all relevant 'team' variables
# jFun <- function(x, jLevels = jTeams) factor(x, levels = jLevels)
# game_play <-
#   transform(game_play, pl_team = jFun(pl_team), poss_team = jFun(poss_team))

## function to create numbered possessions
## feed it raw poss_team (as a vector) or a matrix/data.fram with poss_team and
## point and optionally game
determine_possession <- function(x) {
  if(is.vector(x)) {
    n <- length(x)
    is_start <- c(TRUE, !(x[2:n] == x[seq_len(n - 1)]))
  } else {
    n <- nrow(x)
    if(is.null(x$game)) {
      is_start <-
        c(TRUE, !(x[2:n, 'poss_team'] == x[seq_len(n - 1), 'poss_team'] &
                    x[2:n, 'point'] == x[seq_len(n - 1), 'point']))
    } else {
      is_start <-
        c(TRUE, !(x[2:n, 'poss_team'] == x[seq_len(n - 1), 'poss_team'] &
                    x[2:n, 'point'] == x[seq_len(n - 1), 'point'] &
                    x[2:n, 'game'] == x[seq_len(n - 1), 'game']))
    }    
    return(cumsum(is_start))
  }
}

## create variables that denote possessions

## not sure I have need of absolute possession variable for the entire season?
# mutate(gpDat,
#          poss_abs = determine_possession(gpDat[c('poss_team', 'point', 'game')]))

## absolute possession variable within game
gpDat <- ddply(gpDat, ~ game, function(x)
  mutate(x, poss_abs = determine_possession(x[c('poss_team', 'point')])))
str(gpDat) # 5028 obs. of  9 variables

## possession variable within point
gpDat <- ddply(gpDat, ~ point + game, function(x)
  data.frame(x,
             poss_rel = determine_possession(x[c('poss_team', 'point')])))
str(gpDat) # 5028 obs. of  10 variables:

## aggregate to individual possessions
jFun <- function(x) {
  n <- nrow(x)
  pull_team <- x$pl_team[1]
  ## careful to accomodate a foul on the goal catch and to persist even if there
  ## are somehow two codes containing G (alert will be raised elsewhere)
  its_a_goal <- which(grepl("L*G", x$pl_code))
  if(length(its_a_goal) > 0) {
    goal_row <- max(its_a_goal)
    scor_team <-  x$pl_team[goal_row]
    status <- ifelse(pull_team == scor_team, "break", "hold")
  } else {
    scor_team <- status <- NA
  }
  y <- with(x[n, ], data.frame(period, point, pull_team = pl_team,
                               scor_team, status, n_poss = max(poss_rel)))
  return(y)
}
poss_dat <- ddply(gpDat, ~ point + game, jFun)
str(poss_dat) # 353 obs. of  7 variables:

## get rid of points that end with no goal
poss_dat <- subset(poss_dat, !is.na(status))
str(poss_dat) # 324 obs. of  7 variables:

## distribution of possession length
table(poss_dat$n_poss)
p <- ggplot(poss_dat, aes(x = n_poss))
p <- p + geom_bar() + scale_x_discrete(breaks = 1:17) +
  ylab("number of points that consist of x possessions") +
  xlab("x = number of possessions before point ends in a goal")
p

## do the counting myself for greater control
poss_freq <- ddply(poss_dat, ~ n_poss, summarize,
                   n = length(point), prop = length(point) / nrow(poss_dat))
poss_freq
str(poss_freq)
p <- ggplot(poss_freq, aes(x = n_poss, y = prop))
p + geom_bar(stat = "identity") + scale_x_discrete(breaks = 1:17) +
  ylab("proportion of points that consist of x possessions") +
  xlab("x = number of possessions before point ends in a goal")

## now retain info separately for break and hold points
poss_freq <- ddply(poss_dat, ~ status + n_poss, summarize,
                   n = length(point), abs_prop = n / nrow(poss_dat))
poss_freq
str(poss_freq)
poss_freq <- ddply(poss_freq, ~ status, mutate, wi_prop = n / sum(n))
p <- ggplot(poss_freq, aes(x = n_poss, y = n))
p + geom_bar(stat = "identity") + facet_grid(. ~ status) + 
  scale_x_discrete(breaks = 1:17) +
  ylab("number of points that consist of x possessions") +
  xlab("x = number of possessions before point ends in a goal")

p <- ggplot(poss_freq, aes(x = n_poss, y = wi_prop))
p + geom_bar(stat = "identity") + facet_grid(. ~ status) + 
  scale_x_discrete(breaks = 1:17) +
  ylab("proportion of points that consist of x possessions") +
  xlab("x = number of possessions before point ends in a goal")

p <- ggplot(poss_freq, aes(x = n_poss, y = abs_prop, fill = status))
p + geom_bar(stat = "identity") + 
  scale_x_discrete(breaks = 1:17) +
  ylab("proportion of points that consist of x possessions") +
  xlab("x = number of possessions before point ends in a goal")

## now retain status AND scor_team
poss_freq <- ddply(poss_dat, ~ scor_team + status + n_poss, summarize,
                   n = length(point))
poss_freq
str(poss_freq)
poss_freq <- ddply(poss_freq, ~ scor_team, mutate, team_prop = n / sum(n))
poss_freq
str(poss_freq)
aggregate(team_prop ~ scor_team, poss_freq, sum)

p <- ggplot(poss_freq, aes(x = n_poss, y = team_prop, fill = status))
p + geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = (1:5)/10) + 
  scale_x_continuous(breaks = 1:17, limits = c(0, 13)) + 
  facet_wrap(~ scor_team) + 
  ylab("proportion of points that consist of x possessions") +
  xlab("x = number of possessions before point ends in a goal") +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = 0)) + labs(fill = "")
ggsave("../web/figs/poss_n_dist_by_scor_team_and_status.png")
