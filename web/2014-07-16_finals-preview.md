# Fodder for a finals preview article
17 July, 2014  

<a href="index.html">Back to index</a>

### Data

The figures below are based on data from all Western Conference games played in 2014 plus an increasing number of Eastern Conference games. In particular, all games involving the DC Current are already included.





### Scatterplot: completion percentage vs number of throws

The y positions (completion percentage) in the following two scatterplots are randomly jittered a bit to reduce overplotting. Excluding players with less than 5 throws.

![plot of chunk scatterplot_comp_perc_vs_throws](./2014-07-16_finals-preview_files/figure-html/scatterplot_comp_perc_vs_throws1.png) ![plot of chunk scatterplot_comp_perc_vs_throws](./2014-07-16_finals-preview_files/figure-html/scatterplot_comp_perc_vs_throws2.png) 

### Barchart: player's share of team's total season goals

Figure includes players with rank 15 or higher for total season scoring. Due to ties this may not produce exactly 15 players. Players with goal share > 0.05 are labelled.
![plot of chunk barchart_share_of_goals](./2014-07-16_finals-preview_files/figure-html/barchart_share_of_goals.png) 

### Barchart: player's share of team's total season D's
Figure includes players with rank 15 or higher for total season defenses. Due to ties this may not produce exactly 15 players. Players with defense share > 0.06 are labelled.
![plot of chunk barchart_share_of_ds](./2014-07-16_finals-preview_files/figure-html/barchart_share_of_ds.png) 

### Scatterplot: player's share of total season goals vs D's
![plot of chunk share_of_goals_vs_ds](./2014-07-16_finals-preview_files/figure-html/share_of_goals_vs_ds1.png) ![plot of chunk share_of_goals_vs_ds](./2014-07-16_finals-preview_files/figure-html/share_of_goals_vs_ds2.png) 

### Barchart of no. of passes in a possession
vanNH turn it over more and sooner than wdcCT.
![plot of chunk barchart_passes_per_possession](./2014-07-16_finals-preview_files/figure-html/barchart_passes_per_possession.png) 

### Densityplot of no. of passes in a possession
![plot of chunk densityplot_passes_per_possession](./2014-07-16_finals-preview_files/figure-html/densityplot_passes_per_possession.png) 

### Strip-and-boxplot of no. of passes in a possession (whole MLU)
wdcCT resembles pdxST in terms of making more passes than teams like vanNH. Only sfoDF have shorter possessions than vanNH, but luckily vanNH's possessions end well more often.
![plot of chunk strip_and_boxplot_passes_per_possession](./2014-07-16_finals-preview_files/figure-html/strip_and_boxplot_passes_per_possession.png) 

### Strip-and-boxplot of no. of passes in a possession (vanNH and wdcCT)
Head-to-head demonstration of wdcCT making more passes, across all possession outcomes.
![plot of chunk strip_and_boxplot_passes_per_possession_finals](./2014-07-16_finals-preview_files/figure-html/strip_and_boxplot_passes_per_possession_finals.png) 

### How possessions end, high-level.

y = how possessions end   

  * G = goal
  * off - = offense gives it up = throwaway + drop + travel + stall + offensive foul
  * def + = defense directly forces turn = knock down D + interception + hand block + foot block

x = proportion of possessions that end a certain way

![Alt text](figs/fp_barchart_how_possessions_end_coarse_by_poss_team.png)

### How possessions end, high-level and by line.

I use `o_line` to denote a line that was sent out to receive the pull and play offense. I use `d_line` to denote a line that was sent out to pull and play defense. Of course, if there's at least one turnover, an `o_line` plays defense and a `d_line` plays offense. How do possessions end if we split out by which type of line is currently on offense?

*Caveat: I am not (yet) adjusting for the full line changes we often see during timeouts. But that affects a small proportion of possessions.*

In an absolute sense there are *more* possessions by `o_lines` but the distribution of how the possessions end isn't very different at all.

x and y = *same as above*  

![Alt text](figs/fp_barchart_how_possessions_end_coarse_by_line_and_poss_team.png)

### How possessions end, detailed.

We revisit the same figures as above, but with a more detailed look at how possessions end. Here's what the codes mean:

y = how a possession ends  

  * G = goal
  * D = knock down D + interception + hand block + foot block
  * TA = throwaway, i.e. turnover that is neither a drop nor a clear D
  * TD = drop
  * VTT = violation travel turnover
  * VST = violation stall
  * off F = offensive foul
  
x = proportion of possessions that end a certain way  

![Alt text](figs/fp_barchart_how_possessions_end_detailed_by_poss_team.png)

### How possessions end, detailed and by line.

x and y and meaning of `o_line` and `d_line` = *same as above*  

![Alt text](figs/fp_barchart_how_possessions_end_detailed_by_line_and_poss_team.png)
