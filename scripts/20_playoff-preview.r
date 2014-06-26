library(ggplot2)
library(plyr)

## useful inside reorder(), to invert the resulting factor levels
neglength <- function(x) -1 * length(x)

jWidth <- 5
jHeight <- 4

## I hated the yellow-red Oscar Mayer wiener colors this produced for pdxST +
## vanNH plots
# mlu_cols <-
#   c(pdxST = "#FFCC00", vanNH = "#E64848", seaRM = "#19A347", sfoDF = "#88A5C3")

mlu_cols <-
  c(pdxST = "#4DB870", vanNH = "#CCCCCC", seaRM = "#19A347", sfoDF = "#88A5C3")
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

input_dir <- file.path("..", "games", "2014_west")
poss_file <- file.path(input_dir, "2014_west_possessions.rds")
str(poss_dat <- readRDS(poss_file), give.attr = FALSE) # 1268 obs. of 18 vars
str(poss_dat$game) # Factor w/ 11 levels

poss_dat <-
  droplevels(subset(poss_dat,
                    game %in% grep("vanNH|pdxST", levels(poss_dat$game), value = TRUE)))
str(poss_dat) # 1058 obs. of  18 variables:
str(poss_dat$game)

## get rid of possessions that end with eop
poss_dat <- droplevels(subset(poss_dat, b_code != "eop"))
str(poss_dat) # 1018 obs. of  18 variables:
str(poss_dat$game)

hth_dat <-
  droplevels(subset(poss_dat, grepl("vanNH", game) & grepl("pdxST", game)))
str(hth_dat) # 225 obs. of  18 variables:
str(hth_dat$game) # Factor w/ 3 levels

## loop over the coarse b_code or more detailed a_code; both codes capture how
## the possession ends

for(j_code in c("b_code", "a_code")) {
  
  #j_code <- "b_code"
  #j_code <- "a_code"
  
if(j_code == "b_code") {
  bw1 <- 0.5 # width for barchart re: poss_team
  at <- 1 # size of x-axis tick labels relative to theme base font size
  s1 <- 2.5 # text size re: poss_team
  s2 <- 2.5 # text size re: poss_team AND o line vs d line
  h2 <- 1.3 # hjust re: poss_team AND o line vs d line
  angle <- 0 # geom_text angle re: poss_team AND o line vs d line
} else {
  bw1 <- 0.9 # width for barchart re: poss_team
  at <- 0.6 # size of x-axis tick labels relative to theme base font size
  s1 <- 2.5 # text size re: poss_team
  s2 <- 1.8 # text size re: poss_team AND o line vs d line
  h2 <- 1.3 # hjust re: poss_team AND o line vs d line
  angle <- 0 # geom_text angle re: poss_team AND o line vs d line
}

## how do possessions end? split out by poss_team
last_code_freq_by_team <-
  ddply(poss_dat, ~ poss_team, function(x) count_em_up(j_code, x))
last_code_freq_by_team <-
  droplevels(subset(last_code_freq_by_team, poss_team %in% c("pdxST", "vanNH")))
last_code_freq_by_team[[j_code]] <-
  factor(last_code_freq_by_team[[j_code]],
         levels = rev(levels(last_code_freq_by_team[[j_code]])))
p <- ggplot(subset(last_code_freq_by_team,
                   last_code_freq_by_team[[j_code]] != "Sum"),
            aes_string(x = j_code, y = "prop", fill = "poss_team")) +
  geom_bar(stat = "identity", width = bw1, position = "dodge") +
  xlab("how possessions end") + ylab("proportion of possessions") +
  geom_text(aes(label = pretty_prop), hjust = 1.5, size = s1,
            position = position_dodge(bw1)) +
  scale_fill_manual(values = mlu_cols, guide = guide_legend(reverse = TRUE)) +
  labs(fill = "who's got possession?") +
  theme(legend.position = c(1, 0), legend.justification = c(0.9, 0.1),
        legend.background = element_rect(fill = 0),
        axis.text.x = element_text(size = rel(at))) +
  coord_flip()
p
out_file <- paste0("pp_barchart_how_possessions_end_",
                   if(j_code == "b_code") "coarse" else "detailed",
                   "_by_poss_team.png")
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)

## how do possessions end? O line vs D line AND by poss_team
last_code_freq_by_line_and_team <-
  ddply(poss_dat, ~ poss_team + who, function(x) count_em_up(j_code, x))
last_code_freq_by_line_and_team <-
  droplevels(subset(last_code_freq_by_line_and_team,
                    poss_team %in% c("pdxST", "vanNH")))
last_code_freq_by_line_and_team[[j_code]] <-
  factor(last_code_freq_by_line_and_team[[j_code]],
         levels = rev(levels(last_code_freq_by_line_and_team[[j_code]])))
p <- ggplot(subset(last_code_freq_by_line_and_team,
                   last_code_freq_by_line_and_team[[j_code]] != "Sum"),
            aes_string(x = j_code, y = "prop", fill = "poss_team")) +
  geom_bar(stat = "identity", width = bw1, position = "dodge") +
  facet_grid(who ~ ., scales="free") +
  xlab("how possessions end") + ylab("proportion of possessions") +
  geom_text(aes(label = pretty_prop), hjust = h2, size = s2,
            position = position_dodge(bw1), angle = angle) +
  scale_fill_manual(values = mlu_cols, guide = guide_legend(reverse = TRUE)) +
  labs(fill = "who's got possession?") +
  theme(legend.position = c(1, 0), legend.justification = c(0.9, 0.1),
        legend.background = element_rect(fill = 0),
        axis.text.x = element_text(size = rel(at))) +
  coord_flip()
p
out_file <- paste0("pp_barchart_how_possessions_end_",
                   if(j_code == "b_code") "coarse" else "detailed",
                   "_by_line_and_poss_team.png")
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)

# how do possessions end? split out by poss_team AND game, looking only at
## head-to-head matchups
last_code_freq_by_team_and_game <-
  ddply(hth_dat, ~ game + poss_team, function(x) count_em_up(j_code, x))
last_code_freq_by_team_and_game[[j_code]] <-
  factor(last_code_freq_by_team_and_game[[j_code]],
         levels = levels(last_code_freq_by_team[[j_code]]))
p <- ggplot(subset(last_code_freq_by_team_and_game,
                   last_code_freq_by_team_and_game[[j_code]] != "Sum"),
            aes_string(x = j_code, y = "prop", fill = "poss_team")) +
  geom_bar(stat = "identity", width = bw1, position = "dodge") +
  xlab("how possessions end") + ylab("proportion of possessions") +
  geom_text(aes(label = pretty_prop), hjust = 1.5, size = s1,
            position = position_dodge(bw1)) +
  scale_fill_manual(values = mlu_cols, guide = guide_legend(reverse = TRUE)) +
  labs(fill = "who's got possession?") +
  theme(legend.position = c(1, 0), legend.justification = c(0.9, 0.1),
        legend.background = element_rect(fill = 0),
        strip.text.x = element_text(size = rel(0.7)),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = rel(at)),
        axis.text.x = element_text(size = rel(0.7), angle = 60)) +
  coord_flip() + facet_wrap(~ game) + guides(fill = FALSE)
p
out_file <- paste0("pp_barchart_how_possessions_end_",
                   if(j_code == "b_code") "coarse" else "detailed",
                   "_by_poss_team_and_game.png")
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)
} # this ends the loop over b_code, a_code


## experimenting with game specific looks


#