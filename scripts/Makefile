all: liveStats sync

liveStats: vanNH_nowPlaying.html

clean_goog:
	rm -rf ../games/$(GAME)/rawGoogleExtract/*

eg_all: 01_extract-data-from-google-spreadsheet.py
	./01_extract-data-from-google-spreadsheet.py -g $(GAME)

eg_pt: 01_extract-data-from-google-spreadsheet.py
	./01_extract-data-from-google-spreadsheet.py -g $(GAME) -p $(POINT)

clean_cat:
	rm -rf ../games/$(GAME)/concatGoogleExtract/*

cat_goog: 02_concatentate-google-extract.r
	Rscript ./02_concatentate-google-extract.r $(GAME)

clean_proc:
	rm -rf ../games/$(GAME)/cleanGoogleExtract/*

proc_goog: 03_process-game-data.r
	Rscript ./03_process-game-data.r $(GAME)

web: 06_vanNH-nowPlaying.rmd
	Rscript -e "library(knitr); knit2html('06_vanNH-nowPlaying.rmd')"

r_bits: clean_cat clean_proc cat_goog proc_goog web

yo: eg_pt clean_cat clean_proc cat_goog proc_goog web

sync:
	rsync -av --exclude-from=/Users/jenny/.rsync/exclude vanNH_nowPlaying.html jenny@stat.ubc.ca:public_html/notOcto/vanNH/

clean:
	rm -rf *.md *.html *~ figure/
