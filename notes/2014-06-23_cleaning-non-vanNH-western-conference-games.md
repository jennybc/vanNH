---
title: "Notes from cleaning non-vanNH 2014 Western Conference games"
author: "Jenny Bryan"
date: "23 June, 2014"
output: html_document
---

MLU: "Official Statistics - 2014 - Week 1 - 4/12 - Rainmakers @ Dogfish"  
JB: 2014-04-12_seaRM-at-sfoDF  

Fixed essentially everywhere:

  * Seattle --> Seattle Rainmakers  
  * San Francisco --> San Francisco Dogfish  
  * GL --> LG  
  * Add `PU` for pick-up after the pull 
  * I note that no times noted for when points end/start! And video does not show, so this cannot be fixed.
  
Points 10, 11, 14, 16, 28, 36, 37: added row/cell/event for the pull and populated with `?P`  

Points I watched on video:

  * Point 18: 23:20 on video, score 8-8 at start, in 2nd period, 35 on seaRM ... interception or knock-down D? INTERCEPTION no change to sheet  
  * Point 23: 28:22 on video, score 10-10 at start, first point of 3rd period, 3 on seaRM ... how does he throw to himself? HE DOES NOT There is first a foul on him as thrower, recorded in same row, then his throw gets D'd. I think the stat keeper thought a second cell holding 3 was necessary but I don't thin it is. REMOVED that row.
  * Point 29: 35:21 on video. Make sure 15 on seaRM gets knock down D then 20 picks up. CONFIRMED.
  * Point 33: 83 on sfoDF ... interception or knock-down D? CONFIRMED interception D around 42:40.
  * Point 35: 44:35 on video, first point of period 4 score 15 16. 20 on seaRM ... interception or knock-down D? INTERCEPTION D confirmed.
  * Point 36: 48:30 on video, period 4, score 16-16. 9 on seaRM ... interception or knock-down D? INTERCEPTION D confirmed. Although he sets the disc down on the ground at some point?!?
  * Point 37: there's a foul and a D on same throw; check my script does something sensible  
