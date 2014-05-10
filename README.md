vanNH
=====

In-house statistics for the Vancouver Nighthawks of Major League Ultimate

Things I learned prepping for 2014-05-10_seaRM-at-vanNH

  * To pass my Python script a Google spreadsheet name with all sorts of awful spaces and punctuation in it, this will work, i.e. surround it with single quotes
  
        ./01_extract-data-from-google-spreadsheet.py -gg 'Official Statistics - 2014 - Week 5 - 5/10 - Rainmakers @ Nighthawks' -g 2014-05-10_seaRM-at-vanNH

  * To use my Makefile target to do same, surround it with single AND double quotes

        make eg_all GOOGAME='"Official Statistics - 2014 - Week 5 - 5/10 - Rainmakers @ Nighthawks"' GAME=2014-05-10_seaRM-at-vanNH

  * Note: neither of those is necessary during live game work because I set a default value for `GOOGAME` and `GAME` inside the Makefile now

To do

  * share link to [live game page](http://www.stat.ubc.ca/~jenny/notOcto/vanNH/vanNH_nowPlaying.html) with the right people
  * figure out why test game doesn't "work" until I've got ~ 3 points; of course, as I prep for 2014-05-10_seaRM-at-vanNH, I can't reproduce this problem
  * make it easy for me to insert a message into the live game page, such as "fake data, prepping for game" or "final"
  * players print ugly when their number is not found in the roster; fix that
  * serve up all the games I've ever done, instead of just the current game
  * Makefile stuff
    - __assuming the `GOOGAME` and `GAME` have been set up in the Makefile and are not being passed constantly via command line__
    - `make yo POINT=3` adds Google data for point 3 and reruns all the R stuff
    - `make clean_goog` cleans out the `rawGoogleExtract` directory
    - `make eg_pt POINT=7` processes worksheet = point 7
    - `make eg_all` processes all worksheets
    - `make r_bits` runs all the R scripts
    - `make clean_cat` cleans out the raw, concatenated Google info
    - `make cat_goog` concatenates the raw Google info
    - `make clean_proc` cleans out the clean, processed game data
    - `make proc_goog` processes the game
    - `make web` bakes the webpage

Notes

  * It is evil to record info for offense and defense on the same row of the spreadsheet. Eliminate that at the source whenever possible.
  * I am really not set up to record hand blocks (HB) and foot blocks (FB) right now. Rather I'm expecting plain D's (D).
  * Test if I can write a note in column 3 without breaking anything; would be a good place to record, e.g. a hand block or foot block for later processing.
  * Test with dummy Google Spreadsheet. How to create pull down for pulling team?

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