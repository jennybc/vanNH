vanNH
=====

In-house statistics for the Vancouver Nighthawks of Major League Ultimate

To do

  * Create today's spreadsheet: real one and practice (with date off by one?)
  * dogfish roster
  * test pipeline
    - figure out why test game doesn't "work" until I've got ~ 3 points
    - use Transmit or Make to push to web
    - share link with the right people
  * Makefile stuff
    - `make yo GAME=2014-04-12_vanNH-at-pdxST POINT=3` adds Google data for point 3 and reruns all the R stuff
    - `make clean_goog GAME=2014-04-12_vanNH-at-pdxST` cleans out the `rawGoogleExtract` directory
    - `make eg_pt GAME=2014-04-12_vanNH-at-pdxST POINT=7` processes worksheet = point 7
    - `make eg_all GAME=2014-04-12_vanNH-at-pdxST` processes all worksheets
    - `make r_bits GAME=2014-04-12_vanNH-at-pdxST` runs all the R scripts
    - `make clean_cat GAME=2014-04-12_vanNH-at-pdxST` cleans out the raw, concatenated Google info
    - `make cat_goog GAME=2014-04-12_vanNH-at-pdxST` concatenates the raw Google info
    - `make clean_proc GAME=2014-04-12_vanNH-at-pdxST` cleans out the clean, processed game data
    - `make proc_goog GAME=2014-04-12_vanNH-at-pdxST` processes the game
    - `make web` bakes the webpage (currently hard-wired for game)

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