library(ggplot2)
library(plyr)

## useful inside reorder(), to invert the resulting factor levels
neglength <- function(x) -1 * length(x)

jWidth <- 5
jHeight <- 4

mlu_cols <-
  c(pdxST = "#4DB870", vanNH = "#CCCCCC", seaRM = "#88A5C3", sfoDF = "#FFAD5C")
theme_set(theme_bw())

input_dir <- file.path("..", "games", "2014_west")
point_file <- file.path(input_dir, "2014_west_points.rds")
str(point_dat <- readRDS(point_file), give.attr = FALSE) # 552 obs. of  17 vars

table(point_dat$score) # 503 TRUE 49 FALSE
str(point_dat <- subset(point_dat, score)) # 503 obs. of  17 var

n_poss_freq <- ddply(point_dat, ~ n_poss, function(x) {
  data.frame(n_points = nrow(x))
})
n_poss_freq$cum_points <- cumsum(n_poss_freq$n_points)
tot_points <- sum(n_poss_freq$n_points)
n_poss_freq <-
  mutate(n_poss_freq,
         inv_cum_points = tot_points - c(0, cum_points[-nrow(n_poss_freq)]),
         prop = n_points/tot_points,
         cum_prop = cum_points/tot_points,
         convert_prop = n_points/inv_cum_points)
n_poss_freq

## find a practical max on n_poss for visualization purposes
p <- ggplot(n_poss_freq, aes(x = n_poss, y = cum_prop))
p + ylim(0, 1) + geom_segment(aes(xend = n_poss, yend = 0), size = I(3))

## once we account for 95% of the goals scored, lump the rest together
(n_poss_max <- max(which(n_poss_freq$cum_prop <= 0.95))) # 5 possessions
(catchall_convert_prop <- # weighted avg of convert_prop for the lump
   with(subset(n_poss_freq, n_poss > n_poss_max),
        weighted.mean(convert_prop, n_points))) # 0.4055556

## remake this table, lumping the high poss together
n_poss_freq <- n_poss_freq[c('n_poss', 'n_points')]
n_poss_freq$n_points[n_poss_max + 1] <-
  sum(with(n_poss_freq, n_points[n_poss > n_poss_max]))
n_poss_freq <- subset(n_poss_freq, n_poss <= n_poss_max + 1)
foo <- with(n_poss_freq, paste0(n_poss, rep(c('', "+"), c(n_poss_max, 1))))
n_poss_freq$n_poss_pretty <- with(n_poss_freq, factor(foo, levels = foo))
tot_points <- sum(n_poss_freq$n_points)
n_poss_freq$cum_points <- cumsum(n_poss_freq$n_points)
n_poss_freq <-
  mutate(n_poss_freq,
         inv_cum_points = tot_points - c(0, cum_points[-nrow(n_poss_freq)]),
         prop = n_points/tot_points,
         cum_prop = cum_points/tot_points,
         convert_prop = n_points/inv_cum_points)
n_poss_freq$convert_prop[n_poss_max + 1] <- catchall_convert_prop

## I'll show a confidence interval for convert_prop on the fig
alpha <- 0.05
n_poss_freq$conf_rad <-
  with(n_poss_freq,
       qnorm(1 - alpha/2) * sqrt( (1/inv_cum_points) * convert_prop *
                                    (1 - convert_prop)))

## get overall goal conversion rate
avg_convert_prop <- nrow(point_dat)/sum(point_dat$n_poss)   # 0.4306507
  #with(n_poss_freq, weighted.mean(convert_prop, n_points)) # 0.4370408
anno_text <- paste0("overall goal conversion rate =\n", nrow(point_dat),
                    " goals on ", sum(point_dat$n_poss), " possessions = ",
                    sprintf("%1.2f", avg_convert_prop))

text_size <- rel(3) # for annotations
p <- ggplot(n_poss_freq, aes(x = n_poss, y = convert_prop))
p <- p + 
  geom_hline(yintercept = avg_convert_prop, color = "grey80") +
  geom_errorbar(aes(ymin = convert_prop - conf_rad,
                    ymax = convert_prop + conf_rad), width = 0.15) +
  geom_point(size = 3) + 
  coord_cartesian(xlim = c(0.5, n_poss_max + 1.5),
                  ylim = c(0, 0.65)) +
  xlab("possession number") + ylab("goal conversion rate") +
  with(n_poss_freq,
       scale_x_continuous(breaks = n_poss, labels = n_poss_pretty)) +
  geom_text(aes(x = n_poss, y = 0.2), size = text_size, vjust = 1.2,
            label = with(n_poss_freq,
                         paste0(sprintf("%1.2f", convert_prop), " =\n",
                                n_points, "\nof\n", inv_cum_points))) +
  annotate("text", x = -Inf, y = Inf, label = anno_text,
           hjust = -0.1, vjust = 1.2, size = text_size)
p
out_file <- "dots_with_errbars_goal_conversion_rate_by_poss.png"
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)

text_size <- rel(3) # for annotations
p <- ggplot(n_poss_freq, aes(x = n_poss, y = cum_prop))
p <- p +
  geom_point(size = 3) + geom_line() +
  coord_cartesian(xlim = c(0.5, n_poss_max + 1.5), ylim = c(0, 1.05)) +
  xlab("number of possessions") + ylab("cumulative proportion of points") +
  with(n_poss_freq,
       scale_x_continuous(breaks = n_poss, labels = n_poss_pretty)) +
  geom_text(aes(x = n_poss, y = cum_prop, label = sprintf("%1.2f", cum_prop)),
            size = text_size, hjust = -0.3, vjust = 1.2)
p
out_file <- "lineplot_cum_dist_poss_per_point.png"
out_file <- file.path("..", "web", "figs", out_file)
ggsave(out_file, p, width = jWidth, height = jHeight)
