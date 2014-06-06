---
title: "Notes from processing 2014-05-24_pdxST-at-vanNH after Tim edited"
author: "Jenny Bryan"
date: "5 June, 2014"
output: html_document
---

After Tim edited the official Google spreadsheet for this game, I re-processed it to learn implicitly what the official MLU standards must be. for 2014-06-07_seaRM-at-vanNH, I will try to capture according to their conventions, so want to make sure my scripts are ready for that.

First, checked that this repo has no uncommitted changes, so I am in a clean state and can recover if things go horribly wrong.

Then `make clean_raw` to delete all the raw data extracted from the Google spreadsheet, followed by `make eg_all` to re-process all points.

Notes (I only point out a phenomenon once)

  * point02: 98TT --> 98, 88TT --> 88 *no notion of a turnover blamed on the thrower*
  * point03: 7, 24F, 6TT all on separate lines became {7, 24F} on same line, followed by 6 *they want F recorded on same line as the catch/throw*
  * point04: 3 ?s denoting unrecorded player numbers got populated, presumably from video
  * point08: VP? --> ?VP
  * point09: 89 (catching a pull) --> 89pu *always use `pu`, even if it's not literally a pick up*
  * point12: injury --> 42SO, 73SI (on separate lines)
  * point14: 5PU, 5TT (on separate lines) --> 5PU followed by a PU for opposing team *they really have no use for the "turnover throw" designation*
  * point22: a ? denoting unrecorded player number got populated, presumably from video
  * point23: a ? denoting unrecorded player number got populated, presumably from video AND a confusing sequence of double fouls and a ? pickup got replaced with a plain catch/throw for a numbered player
  * point28: clock after point 0:36 --> 0:37
  * point32: a ? denoting unrecorded player number got populated, presumably from video
  * point33: a ? denoting unrecorded player number got populated, presumably from video
  * point34: a player number got inserted, injury --> 24SO, 75SI *note that a F was left on a separate line here, why?*

What will I do about this
  
  * accept the elimination of the `TT` turnover throw; guess we'll stop recording that
  * accept they are committed to recording a F in the cell adjacent to the person who's got the disk; comply with that and handle solely in my own scripts
  * obviously accept all the fixes that addresses missing or incorrect data
  * use `pu` for the person dealing with the pull, regardless of whether it's a pick up or a catch
  * accept the replacements of "injury" by the SO and SI; my scripts will ignore that info for now, since I don't track lines explicitly yet

Then `make clean_cat` to delete all the concatenated gameplay data and the point data, followed by `make cat_goog` to re-do that step.

Notes (I only point out diffs that aren't an expected result of stuff noted above):

  * points 9 and 25: some cells that look empty on the spreadsheet were previously   NA in the concatenated data and are now empty (point 9) or vice versa (point 25)

What will I do about this: nothing

Then `make clean_clean` to delete the cleaned gameplay and point data, followed by `make clean_game` to re-do that step.
  
Notes (I only point out diffs that aren't an expected result of stuff noted above):

  * The newly concatenated data deals with fouls the MLU way, ie. records on same line as associated catch/throw. Recall in this game I always recorded fouls "my way", so my script's handling of this wasn't really tested. Dealing with MLU style records reveals that sometimes my script separates the two events in the wrong order, i.e. the foul ends up before the associated catch/throw.
  
What I have done about this: I fixed my script and redid `make clean_clean` and `make clean_game`. It was a symptom of taking "oTeam" and "dTeam" too literally. This sort of mistake won't be so easy to make once I've switched to better terminology re: pulling team vs receiving team, so that oTeam and dTeam are situational.

Then `make clean_web` to delete live stat page's markdown and html files, followed by `make web` to remake them. All changes are expected consequences of the above.