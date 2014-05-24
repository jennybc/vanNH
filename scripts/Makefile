#GOOGAME ?= "Official Statistics - 2014 - Week 2 - 4/20 - Dogfish @ Nighthawks"
#GAME ?= 2014-04-20_sfoDF-at-vanNH

GOOGAME ?= "Official Statistics - 2014 - Week 5 - 5/10 - Rainmakers @ Nighthawks"
GAME ?= 2014-05-10_seaRM-at-vanNH

all: liveStats sync

liveStats: vanNH_nowPlaying.html

clean_raw:
	rm -rf ../games/$(GAME)/01_rawGoogleExtract/*

eg_all: 01_extract-data-from-google-spreadsheet.py
	./01_extract-data-from-google-spreadsheet.py -gg $(GOOGAME) -g $(GAME)

eg_pt: 01_extract-data-from-google-spreadsheet.py
	./01_extract-data-from-google-spreadsheet.py -gg $(GOOGAME) -g $(GAME) -p $(POINT)

clean_cat:
	rm -rf ../games/$(GAME)/03_concatGoogleExtract/*

cat_goog: 02_concatentate-google-extract.r
	Rscript ./02_concatentate-google-extract.r $(GAME)

clean_clean:
	rm -rf ../games/$(GAME)/04_cleanedGame/*

clean_game: 03_process-game-data.r
	Rscript ./03_process-game-data.r $(GAME)

web: 06_vanNH-nowPlaying.rmd
	./05_compile-HTML.r $(GAME)

web_copy:
	cp ../games/$(GAME)/05_htmlArchive/$(GAME)_live-stats.html ../web/

## the resulting symlink does not work on the server; gets this error:
## Forbidden
## You don't have permission to access /~jenny/notOcto/vanNH/vanNH_nowPlaying.html on this server.
##set_web:
##	ln -s ../web/$(GAME)_live-stats.html ../web/vanNH_nowPlaying.html

set_web:
	cd ../web; cp $(GAME)_live-stats.html vanNH_nowPlaying.html

compile_index: 
	cd ../web; Rscript -e "library(markdown); markdownToHTML('index.md')"

r_bits: clean_cat clean_clean cat_goog clean_game web

yo: eg_pt clean_cat clean_clean cat_goog clean_game web web_copy sync

sync:
	cd ../web; rsync -av --exclude-from=/Users/jenny/.rsync/exclude ./ jenny@stat.ubc.ca:public_html/notOcto/vanNH/

clean:
	rm -rf *.md *.html *~ figure/
