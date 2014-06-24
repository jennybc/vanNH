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
  

