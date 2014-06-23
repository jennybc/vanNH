#GOOGAME ?= "Official Statistics - 2014 - Week 1 - 4/12 - Nighthawks @ Stags"
#GAME ?= 2014-04-12_vanNH-at-pdxST

#GOOGAME ?= "Official Statistics - 2014 - Week 2 - 4/20 - Dogfish @ Nighthawks"
#GAME ?= 2014-04-20_sfoDF-at-vanNH

#GOOGAME ?= "Official Statistics - 2014 - Week 3 - 4/26 - Nighthawks @ Rainmakers"
#GAME ?= 2014-04-26_vanNH-at-seaRM

#GOOGAME ?= "Official Statistics - 2014 - Week 5 - 5/10 - Rainmakers @ Nighthawks"
#GAME ?= 2014-05-10_seaRM-at-vanNH

#GOOGAME ?= "Official Statistics - 2014 - Week 6 - 5/17 - Nighthawks @ Dogfish"
#GAME ?= 2014-05-17_vanNH-at-sfoDF

#GOOGAME ?= "Official Statistics - 2014 - Week 7 - 5/24 - Stags @ Nighthawks"
#GAME ?= 2014-05-24_pdxST-at-vanNH

#GOOGAME ?= "Official Statistics - 2014 - Week 8 - 5/31 - Nighthawks @ Rainmakers"
#GAME ?= 2014-05-31_vanNH-at-seaRM

#GOOGAME ?= "Official Statistics - 2014 - Week 9 - 6/7 - Rainmakers @ Nighthawks"
#GAME ?= 2014-06-07_seaRM-at-vanNH

#GOOGAME ?= "Official Statistics - 2014 - Week 10 - 6/15 - Stags @ Nighthawks"
#GAME ?= 2014-06-15_pdxST-at-vanNH

GOOGAME ?= "Official Statistics - 2014 - Week 1 - 4/12 - Rainmakers @ Dogfish"
GAME ?= 2014-04-12_seaRM-at-sfoDF

#GOOGAME ?= "test"
#GAME ?= 2014-12-31_pdxST-at-vanNH

sitrep:
	echo $(GOOGAME) $(GAME) 

init:
	Rscript ./00_init-game.r $(GAME)
	
delete:
	rm -rf ../games/$(GAME)/*
	rm ../web/$(GAME)_live-stats.html

clean_raw:
	rm -rf ../games/$(GAME)/01_rawGoogleExtract/*

eg_all: 01_extract-data-from-google-spreadsheet.py
	./01_extract-data-from-google-spreadsheet.py -gg $(GOOGAME) -g $(GAME)

eg_pt: 01_extract-data-from-google-spreadsheet.py
	./01_extract-data-from-google-spreadsheet.py -gg $(GOOGAME) -g $(GAME) -p $(POINT)

clean_cat:
	rm -rf ../games/$(GAME)/03_concatGoogleExtract/*

cat_goog: 03_concatentate-google-extract.r
	Rscript ./03_concatentate-google-extract.r $(GAME)

clean_clean:
	rm -rf ../games/$(GAME)/05_cleanedGame/*

clean_game: 05_clean-and-expand-game-play.r
	Rscript ./05_clean-and-expand-game-play.r $(GAME)

clean_resolve:
	rm -rf ../games/$(GAME)/07_resolvedGame/*

resolve_game: 07_resolve-game-play.r
	Rscript ./07_resolve-game-play.r $(GAME)

clean_web:
	rm -rf ../games/$(GAME)/09_htmlArchive/*

web: 
	./08_compile-HTML.r $(GAME)

web_copy:
	cp ../games/$(GAME)/09_htmlArchive/$(GAME)_live-stats.html ../web/

## the resulting symlink does not work on the server; gets this error:
## Forbidden
## You don't have permission to access /~jenny/notOcto/vanNH/vanNH_nowPlaying.html on this server.
##set_web:
##	ln -s ../web/$(GAME)_live-stats.html ../web/vanNH_nowPlaying.html

set_web:
	cd ../web; cp $(GAME)_live-stats.html vanNH_nowPlaying.html

# this works if not using Preview version of RStudio, new rmarkdown package, very recent version of Pandoc
#index.html: ../web/index.md 
#	cd ../web; Rscript -e "library(markdown); markdownToHTML('index.md', output = 'index.html', stylesheet = file.path(path.expand('~/'), 'resources/css/jasonm23-markdown-css-themes', 'markdown7.css'))"

index.html: ../web/index.rmd ../games/2014*/*at-last-point.yaml
	cd ../web; Rscript -e "rmarkdown::render('index.rmd', quiet = TRUE)"

r_bits: clean_cat clean_clean clean_resolve cat_goog clean_game resolve_game web web_copy

pt_fix: eg_pt clean_cat clean_clean clean_resolve cat_goog clean_game resolve_game web web_copy

yo: eg_pt clean_cat clean_clean clean_resolve cat_goog clean_game resolve_game web web_copy set_web sync

sync:
	cd ../web; rsync -av --exclude-from=/Users/jenny/.rsync/exclude ./ jenny@stat.ubc.ca:public_html/notOcto/vanNH/

clean:
	rm -rf *.md *.html *~ figure/
