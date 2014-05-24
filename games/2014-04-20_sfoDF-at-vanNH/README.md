---
title: "2014-04-20_sfoDF-at-vanNH"
author: "Jenny Bryan"
date: "23 May, 2014"
output: html_document
---

I need to address the jersey switching in this game. At gametime, I handled by editing the roster. But since then, of course, the roster has switched back to normal and so, when I re-process the game, the wrong people are associated with relevant events.

The fix I'd prefer is to create the intermediate `02_fixedGoogleExtract`-type of directory, full of point files in this case, and do a global search and replace. Handle by retrospectively recording the number usually associated with the players who played (vs. the jersey they were wearing that night).

The jersey switching was recorded in a9624c8a6c1013914688c777c194fa3f42a99c64.

  * hayduk, normally 49, wore 18 (usually worn by menzies)
  * dandurand, normally ?7?, wore 6 (usually worn by gailits)