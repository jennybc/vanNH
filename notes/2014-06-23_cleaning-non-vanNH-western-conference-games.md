---
title: "Notes from cleaning non-vanNH 2014 Western Conference games"
author: "Jenny Bryan"
date: "23 June, 2014"
output: html_document
---

MLU: "Official Statistics - 2014 - Week 1 - 4/12 - Rainmakers @ Dogfish"  
JB: 2014-04-12_seaRM-at-sfoDF  

Open question:

  * I only find 1 D for seaRM-20-simon but MLU reports 2 for this game. (No other discrepancies in player-level assists, goals, Ds found.)

Fixed essentially everywhere:

  * Seattle --> Seattle Rainmakers  
  * San Francisco --> San Francisco Dogfish  
  * GL --> LG  
  * Add `PU` for pick-up after the pull 
  * I note that no times noted for when points end/start! And video does not show, so this cannot be fixed.

MLU: Official Statistics - 2014 - Week 2 - 4/19 - Dogfish @ Rainmakers
JB: 2014-04-19_sfoDF-at-seaRM

Fixed essentially everywhere:

  * Seattle --> Seattle Rainmakers  
  * San Francisco --> San Francisco Dogfish  
 
Points 11, 13, 35, 39: added `PU` to pick up off the pull.

Point 36: 59:49 in video, 5:55 left in period 4, score is 15-17. Why is 24-seaRM marked as picking up after he already has disc and gets fouled? I CORRECTED THIS and simply removed the cell with 24PU.

Point 7: 2:57 left in period 1, score is 3-4. How can 13-seaRM get a D, then immediately get a turnover drop? I ADDED A PU code to 88-sfoDF when they get the disc back and eventually score. VIDEO DOES NOT INCLUDE THIS POINT.

sfoDF-11-Lee: I have 1D, MLU has 2.
  
MLU: "Official Statistics - 2014 - Week 3 - 4/26 - Stags @ Dogfish"
JB: 2014-04-26_pdxST-at-sfoDF

Fixed essentially everywhere:

  * Portland --> Portland Stags  
  * San Francisco --> San Francisco Dogfish  
  * GL, gL, gl --> LG  
  * Add `PU` for pick-up after the pull 

Corrected times where fractions of second were recorded near end of periods 2 and 4.

Period "ot" --> 5

Point 13: 6:18 left in the 2nd, score 5-7. sfoDF pulls, on the 2nd time they get possession, 17Ds 17PU 17 cth huh? I STRAIGHTENED OUT THE GAME PLAY HERE, which results in a new D for pdxST #2.

Point 36: Near 1:00 in video, first point of overtime, score 16-16. seaRM turnover due to 8-seaRM turnover drop or 10-sfoDF D? Shouldn't we choose between those? If 8 had caught it, we would not give 10 the D. So shouldn't this be a drop, since it was catchable despite the tip?

D's for sfoDF-17-Boyd-Meredith. I can only find 1 (point 13) but MLU is reporting 2. pdxST-17 gets a D in point 9 ... is that somehow confusing things?




Official Statistics - 2014 - Week 11 - 6/21 - Nighthawks @ Dogfish
2014-06-21_vanNH-at-sfoDF

Point 12: 9:02 left in period 2, score is 5-5. Many changes of possession ... 89-vanNH gets called for VTT and 23-sfoDF picks up, gets fouled by 98-vanNH then ?8-sfoDF? picks up? How does that work? GAME PLAY WAS GARBLED HERE. I corrected. A vanNH possession was essentially missed and, in particular, vanNH-98 did not foul sfoDF, rather he got an interception D, then threw it away. THIS GIVES WONG A NEW D.

Points 12, 14, 15, 16, 27, 31: recoded a timeout as "?TO" instead of "to".

