vanNH
=====

In-house statistics for the Vancouver Nighthawks of Major League Ultimate

<http://www.stat.ubc.ca/~jenny/notOcto/vanNH/>

To do

  * check systematically for creative entries in clock before/after, i.e. when people only report the second or suddenly report tenths of a second, etc.
  * 2014-04-19_phlSP-at-nykRM point 36: the second event is coded `TD` instead of `PU`; the player attempted to catch the pull but dropped it ...that's the first case of this; I need to take a deliberate look at what all my scripts do with that
  * 2014-05-03_bosWC-at-nykRM point 39: the second event is coded `TO` instead of `PU`; apparently there was a timeout called even before a pickup;  I need to take a deliberate look at what all my scripts do with that
  * take comprehensive look at what's character vs factor
  * sort out the offline access for Google spreadsheet
  * add a comment column *have asked MLU to bless this but no response*
  * get a handle on which offensive fouls cause posession to change and which ones don't

Makefile notes *not updated in a long while ...*
  * __assuming the `GOOGAME` and `GAME` have been set up in the Makefile and are not being passed constantly via command line__
  * `make clean_raw` removes all data extracted from Google spreadsheet, sitting in `01_rawGoogleExtract`
  * `make eg_pt POINT=7` extracts raw data from the Google worksheet for point 7
  * `make eg_all` extracts raw data from all worksheets, all points
  * `make yo POINT=3` adds Google data for point 3 and reruns all the R stuff
  * `make clean_cat` cleans out the raw, concatenated Google data, sitting in `03_concatGoogleExtract`
  * `make cat_goog` concatenates the raw Google data across all points
  * `make clean_clean` cleans out the clean, processed game data, sitting in `04_cleanedGame`
  * `make clean_game` processes the game
  * `make web` bakes the "now playing" style game webpage
  * `make web_copy` copies the "now playing" webpage into `web`
  * `make set_web` copies the "now playing" webpage to `vanNH_nowPlaying.html`; ideally this would be just symlinking but I'm having trouble with that and can't troubleshoot now
  * `make compile_index` compiles `web/index.md`; should only need to be re-done when new game is added to `index.md`
  * `make r_bits` runs all the R scripts
  * `make sync` syncs `web/*` to `notOcto/vanNH` subdirectory of my stat website  

Notes

  * To pass my Python script a Google spreadsheet name with all sorts of awful spaces and punctuation in it, this will work, i.e. surround it with single quotes
  
        ./01_extract-data-from-google-spreadsheet.py -gg 'Official Statistics - 2014 - Week 5 - 5/10 - Rainmakers @ Nighthawks' -g 2014-05-10_seaRM-at-vanNH

  * To use my Makefile target to do same, surround it with single AND double quotes

        make eg_all GOOGAME='"Official Statistics - 2014 - Week 5 - 5/10 - Rainmakers @ Nighthawks"' GAME=2014-05-10_seaRM-at-vanNH

  * Note: neither of the above is necessary during live game work because I set a default value for `GOOGAME` and `GAME` inside the Makefile now

  * It is evil to record info for offense and defense on the same row of the spreadsheet. MLU likes that for defensive fouls, so I handle that with my own cleaning script.
  
  * Stay aware `0` vs `00` re seattle rainmakers. Always use `00`. Helpful lines for *ad hoc* checking for either in game play at the end of cleaning and expanding:
        game_play[with(game_play, grepl("00", pullNum) | grepl("00", recvNum)), ]
        game_play[with(game_play, grepl("\\b0\\b", pullNum) | grepl("\\b0\\b", recvNum)), ]

  
  
Helpful for Python command line argument processing:
http://www.cyberciti.biz/faq/python-command-line-arguments-argv-example/

https://docs.python.org/dev/library/argparse.html

https://docs.python.org/dev/howto/argparse.html

Python working directory stuff:
http://stackoverflow.com/questions/5137497/find-current-directory-and-files-directory

File I/O:
https://docs.python.org/2/tutorial/inputoutput.html#reading-and-writing-files

Knitting from command line or Makefile:
http://stackoverflow.com/questions/10943695/what-is-the-knitr-equivalent-of-r-cmd-sweave-myfile-rnw
http://stackoverflow.com/questions/10646665/how-to-convert-r-markdown-to-html-i-e-what-does-knit-html-do-in-rstudio-0-9?lq=1
http://stackoverflow.com/questions/17533359/how-can-i-use-command-line-arguments-with-knitr-source-file