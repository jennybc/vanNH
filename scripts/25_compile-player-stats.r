#!/usr/bin/Rscript

## this script has two purposes
##   1 command line argument processing -- specifically getting the team(s)
##   2 knitting 25_player-stats-by-team.rmd to HTML 

library(rmarkdown)

options <- commandArgs(trailingOnly = TRUE)

if(length(options) < 1) {
  #team <- "wdcCT"
  #team <- "vanNH"
  target_team <- c('vanNH', 'wdcCT')
} else {
  target_team <- options[1]
}

for(team in target_team) {
  local_md_file <- paste0(team, "_player-stats.md")
  local_html_file <- paste0(team, "_player-stats.html")
  local_helper_files_dir <- paste0(team, "_player-stats_files")
  
  render('25_player-stats-by-team.rmd', output_file = local_html_file,
         quiet = TRUE)
  
  team_stats_dir <- file.path("..", "team-stats")
  if(!file.exists(team_stats_dir)) dir.create(team_stats_dir)
  team_dir <- file.path(team_stats_dir, team)
  if(!file.exists(team_dir)) dir.create(team_dir)
  
  target_md_file <- file.path(team_dir, local_md_file)
  target_html_file <- file.path(team_dir, local_html_file)
  target_helper_files_dir <- file.path(team_dir, local_helper_files_dir)
  
  if(file.exists(target_helper_files_dir)) {
    unlink(target_helper_files_dir, recursive = TRUE)
  }
  
  file.rename(local_html_file, target_html_file)
  file.rename(local_md_file, target_md_file)
  file.rename(local_helper_files_dir, target_helper_files_dir)
  
  team_stats_web_dir <- file.path("..", "web", "team-stats")
  if(!file.exists(team_stats_web_dir)) dir.create(team_stats_web_dir)
  
  web_html_file <- file.path(team_stats_web_dir, local_html_file)
  
  file.copy(target_html_file, web_html_file, overwrite = TRUE)
  
}
