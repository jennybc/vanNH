library(plyr)
library(ggplot2)

games <- c("2014-04-12_vanNH-at-pdxST", "2014-04-20_sfoDF-at-vanNH",
           "2014-04-26_vanNH-at-seaRM", "2014-05-10_seaRM-at-vanNH",
           "2014-05-17_vanNH-at-sfoDF", "2014-05-24_pdxST-at-vanNH",
           "2014-05-31_vanNH-at-seaRM", "2014-06-07_seaRM-at-vanNH",
           "2014-06-15_pdxST-at-vanNH")
game_file <- file.path("..", "games", games, "07_resolvedGame",
                       paste0(games, "_gameplay-resolved.tsv"))
names(game_file) <- games
gpDat <-
  ldply(game_file, function(gg) read.delim(gg, stringsAsFactor = FALSE),
        .id = "game")
str(gpDat) # 5664 obs. of  8 variables

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

## Why would I ever need an absolute possession variable for the entire season?
## I made this possible in case I ever analyze groups of points from different
## games where two "point 9"'s could end up adjacent to each other
# mutate(gpDat,
#       poss_abs = determine_possession(gpDat[c('poss_team', 'point', 'game')]))

## absolute possession variable within game
gpDat <- ddply(gpDat, ~ game, function(x)
  mutate(x, poss_abs = determine_possession(x[c('poss_team', 'point')])))
str(gpDat) # 5664 obs. of  9 variables

## relative possession variable, i.e. within point
gpDat <- ddply(gpDat, ~ point + game, function(x)
  data.frame(x,
             poss_rel = determine_possession(x[c('poss_team', 'point')])))
str(gpDat) # 5664 obs. of  10 variables:

## aggregate to possessions: to gpDat variables, adds the pulling team for the
## associated point, logical indicating if possession ends in a point, scoring
## team
poss_dat <- ddply(gpDat, ~ game + poss_abs, function(x) {
  pull_team <- x$pl_team[1]
  n <- nrow(x)
  score <- which(grepl("L*G", x$pl_code))
  scor_team <- as.character(if(any(score)) x$pl_team[max(score)] else NA)
  data.frame(x[n, ], pull_team, score = any(score), scor_team)
})
str(poss_dat) # 842 obs. of  13 variables:

## sanity checks of poss_dat
ddply(poss_dat, ~ game + scor_team, summarize, score = sum(score))
## yes agrees with actual final scores

## create a new version of the pl_code that is coarser
# d_codes <- c('D', 'HB', 'FB')
# goal_codes <- c('G', 'LG')
# throwaway_codes <- c('*', 'PU', 'L')
poss_dat$a_code <-
  mapvalues(poss_dat$pl_code, # revalue() won't work due to factor level ''
            from = c('D', 'HB', 'FB', 'G', 'LG',   '', 'PU',  'L'),
            to   = c('D',  'D',  'D', 'G',  'G', 'TA', 'TA', 'TA'))
poss_dat$a_code <- with(poss_dat, reorder(a_code, a_code, length))
as.data.frame(table(poss_dat$a_code, dnn = "a_code"))

## reorder and revalue pl_code itself
poss_dat$pl_code <- mapvalues(poss_dat$pl_code, from = '', to = '*')
poss_dat$pl_code <- with(poss_dat, reorder(pl_code, pl_code, length))
as.data.frame(table(poss_dat$pl_code, dnn = "a_code"))

## how do possessions end? naive approach based only on last pl_code, which
## actually works quite well
last_code_freq <- as.data.frame(table(poss_dat$a_code, dnn = "a_code"))
last_code_freq <-
  rbind(last_code_freq,
        data.frame(a_code = "Sum", Freq = sum(last_code_freq$Freq)))
last_code_freq <-
  mutate(last_code_freq, prop = Freq / Freq[nrow(last_code_freq)],
         pretty_prop = as.character(round(prop, 2)))

## bar chart these frequencies
p <- ggplot(subset(last_code_freq, a_code != "Sum"), aes(x = a_code, y = prop))
p + geom_bar(stat = "identity") +
  geom_text(aes(label = pretty_prop), vjust = -0.2, size = 4) + 
  xlab("how a possession ends") + ylab("proportion of possessions")
ggsave("../web/figs/barchart_how_possessions_end.png")

## how do possessions end? split out by poss_team
last_code_freq_by_team <- ddply(poss_dat, ~ poss_team, function(x) {
  tmp <- as.data.frame(table(x$a_code, dnn = "a_code"))
  tmp <- rbind(tmp, data.frame(a_code = "Sum", Freq = sum(tmp$Freq)))
  tmp <- mutate(tmp, prop = Freq / Freq[nrow(tmp)],
                pretty_prop = as.character(round(prop, 2)))
  return(tmp)  
})

p <- ggplot(subset(last_code_freq_by_team, a_code != "Sum"),
            aes(x = a_code, y = prop))
p + geom_bar(stat = "identity") + facet_wrap(~ poss_team) +
  geom_text(aes(label = pretty_prop), vjust = -0.2, size = 4) + 
  xlab("how a possession ends") + ylab("proportion of possessions")
ggsave("../web/figs/barchart_how_possessions_end_by_team.png")



d_ply(poss_dat, ~ pl_code, function(x) {
  match_vars <- c('game', 'period', 'point', 'event')
  gp_rows <-
    join(x[match_vars],
         data.frame(gpDat[match_vars], row = seq_len(nrow(gpDat))))$row
  display_vars <- c('game', 'point', 'event', 'poss_team', 'pl_team',
                    'pl_pnum', 'pl_code')
  for(i in seq_along(gp_rows)) {
    print(gpDat[gp_rows[i] + (-2:2), display_vars])
    cat("\n")
  }
  
})


## aggregate to points: record how many possessions, who scored (if anyone),
## and whether it was a hold or break
jFun <- function(x) {
  n <- nrow(x)
  pull_team <- x$pl_team[1]
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
poss_dat <- ddply(gpDat, ~ point + game, jFun)
str(poss_dat) # 394 obs. of  7 variables:

## get rid of points that end with no goal
poss_dat <- subset(poss_dat, !is.na(status))
str(poss_dat) # 362 obs. of  7 variables:

## distribution of possession length
poss_freq <- ddply(poss_dat, ~ n_poss, summarize,
                   n = length(point), prop = length(point) / nrow(poss_dat))
poss_freq <-
  mutate(poss_freq,
         status = factor(ifelse((poss_freq$n_poss %% 2) == 1,
                                "hold", "break"),
                         levels = c('hold', 'break')),
         pretty_prop = ifelse(prop > 0.01, as.character(round(prop, 2)), ''),
         cum_prop = cumsum(prop),
         pretty_cum_prop = ifelse(cum_prop < 0.98,
                                  as.character(round(cum_prop, 2)), ''))
poss_freq
str(poss_freq)

p <- ggplot(poss_freq, aes(x = n_poss, y = prop, fill = status))
p + geom_bar(stat = "identity") +
  geom_text(aes(label = pretty_prop), vjust = -0.2, size = 4) +
  scale_x_discrete(breaks = 1:17) +
  ylab("proportion of points scored after exactly x possessions") +
  xlab("x = number of possessions before point ends in a goal") +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = 0)) + labs(fill = "")
ggsave("../web/figs/poss_n_dist_by_status.png")

p <- ggplot(poss_freq, aes(x = n_poss, y = cum_prop))
p + geom_bar(stat = "identity") + 
  geom_text(aes(label = pretty_cum_prop), vjust = -0.2, size = 4) +
  scale_x_discrete(breaks = 1:17) + 
  ylab("proportion of points scored in x possessions or less") +
  xlab("x = number of possessions before point ends in a goal")
ggsave("../web/figs/poss_n_CDF_by_status.png")


## now retain info separately for break and hold points
poss_freq <- ddply(poss_dat, ~ status + n_poss, summarize,
                   n = length(point), abs_prop = n / nrow(poss_dat))
poss_freq
str(poss_freq)
poss_freq <- ddply(poss_freq, ~ status, mutate, wi_prop = n / sum(n))
p <- ggplot(poss_freq, aes(x = n_poss, y = n))
p + geom_bar(stat = "identity") + facet_grid(. ~ status) + 
  scale_x_discrete(breaks = 1:17) +
  ylab("number of points scored after exactly x possessions") +
  xlab("x = number of possessions before point ends in a goal")

p <- ggplot(poss_freq, aes(x = n_poss, y = wi_prop))
p + geom_bar(stat = "identity") + facet_grid(. ~ status) + 
  scale_x_discrete(breaks = 1:17) +
  ylab("number of points scored after exactly x possessions") +
  xlab("x = number of possessions before point ends in a goal")

p <- ggplot(poss_freq, aes(x = n_poss, y = abs_prop, fill = status))
p + geom_bar(stat = "identity") + 
  scale_x_discrete(breaks = 1:17) +
  ylab("proportion of points scored after exactly x possessions") +
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
  ylab("proportion of points scored after exactly x possessions") +
  xlab("x = number of possessions before point ends in a goal") +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_rect(fill = 0)) + labs(fill = "")
ggsave("../web/figs/poss_n_dist_by_scor_team_and_status.png")
