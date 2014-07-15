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

#GOOGAME ?= "Official Statistics - 2014 - Week 11 - 6/21 - Nighthawks @ Dogfish"
#GAME ?= 2014-06-21_vanNH-at-sfoDF

#GOOGAME ?= "Official Statistics - 2014 - Playoffs - 6/28 - Nighthawks @ Stags"
#GAME ?= 2014-06-28_vanNH-at-pdxST

#GOOGAME ?= "Official Statistics - 2014 - Week 1 - 4/12 - Rainmakers @ Dogfish"
#GAME ?= 2014-04-12_seaRM-at-sfoDF

#GOOGAME ?= "Official Statistics - 2014 - Week 2 - 4/19 - Dogfish @ Rainmakers"
#GAME ?= 2014-04-19_sfoDF-at-seaRM

#GOOGAME ?= "Official Statistics - 2014 - Week 3 - 4/26 - Stags @ Dogfish"
#GAME ?= 2014-04-26_pdxST-at-sfoDF

#GOOGAME ?= "Official Statistics - 2014 - Week 4 - 5/4 - Dogfish @ Rainmakers"  
#GAME ?= 2014-05-04_sfoDF-at-seaRM

GOOGAME ?= "Official Statistics - 2014 - Week 4 - 5/3 - Dogfish @ Stags"  
GAME ?= 2014-05-03_sfoDF-at-pdxST

#GOOGAME ?= "Official Statistics - 2014 - Week 6 - 5/17 - Rainmakers @ Stags"  
#GAME ?= 2014-05-17_seaRM-at-pdxST

#GOOGAME ?= "Official Statistics - 2014 - Week 8 - 5/31 - Dogfish @ Stags"  
#GAME ?= 2014-05-31_sfoDF-at-pdxST

#GOOGAME ?= "Official Statistics - 2014 - Week 9 - 6/7 - Stags @ Dogfish"  
#GAME ?= 2014-06-07_pdxST-at-sfoDF

#GOOGAME ?= "Official Statistics - 2014 - Week 10 - 6/14 - Stags @ Rainmakers"  
#GAME ?= 2014-06-14_pdxST-at-seaRM

#GOOGAME ?= "Official Statistics - 2014 - Week 11 - 6/21 - Rainmakers @ Stags"  
#GAME ?= 2014-06-21_seaRM-at-pdxST

#GOOGAME ?= "Official Statistics - 2014 - Week 1 - 4/12 - Current @ Spinners"  
#GAME ?= 2014-04-12_wdcCT-at-phlSP

#GOOGAME ?= "Official Statistics - 2014 - Week 2 - 4/19 - Whitecaps @ Current"  
#GAME ?= 2014-04-19_bosWC-at-wdcCT

#GOOGAME ?= "Official Statistics - 2014 - Week 3 - 4/26 - Current @ Whitecaps"  
#GAME ?= 2014-04-26_wdcCT-at-bosWC

#GOOGAME ?= "Official Statistics - 2014 - Week 4 - 5/4 - Spinners @ Current"  
#GAME ?= 2014-05-04_phlSP-at-wdcCT

#GOOGAME ?= "Official Statistics - 2014 - Week 5 - 5/10 - Rumble @ Current"  
#GAME ?= 2014-05-10_nykRM-at-wdcCT

#GOOGAME ?= "Official Statistics - 2014 - Week 6 - 5/17 - Current @ Rumble"  
#GAME ?= 2014-05-17_wdcCT-at-nykRM

#GOOGAME ?= "Official Statistics - 2014 - Week 8 - 5/31 - Spinners @ Current"  
#GAME ?= 2014-05-31_phlSP-at-wdcCT

#GOOGAME ?= "Official Statistics - 2014 - Week 9 - 6/7 - Current @ Whitecaps"  
#GAME ?= 2014-06-07_wdcCT-at-bosWC

#GOOGAME ?= "Official Statistics - 2014 - Week 10 - 6/14 - Rumble @ Current"  
#GAME ?= 2014-06-14_nykRM-at-wdcCT

#GOOGAME ?= "Official Statistics - 2014 - Week 11 - 6/21 - Current @ Spinners"  
#GAME ?= 2014-06-21_wdcCT-at-phlSP

#GOOGAME ?= "Official Statistics - 2014 - Playoffs - 6/28 - Whitecaps @ Current"  
#GAME ?= 2014-06-28_bosWC-at-wdcCT

## just in case GAME has some trailing whitespace ...
GAME := $(strip $(GAME))

## create a macro to render (i.e. spin) an R script and tidy up after
define tidy-spin
      @echo Spinning $< ...
      Rscript -e 'rmarkdown::render("$<", clean = FALSE)'
      rm $(basename $<).spin.Rmd
      rm $(basename $<).spin.R
      rm $(basename $<).knit.md
      mv $(basename $<).utf8.md $(basename $<).md
endef

sitrep:
	@echo $(GOOGAME) $(GAME) 

init:
	Rscript ./00_init-game.r $(GAME)
	
delete:
	rm -rf ../games/$(GAME)/*
	rm ../web/$(GAME)_live-stats.html

clean_raw:
	rm -rf ../games/$(GAME)/01_raw-google/*

eg_all: 01_extract-data-from-google-spreadsheet.py
	./01_extract-data-from-google-spreadsheet.py -gg $(GOOGAME) -g $(GAME)

eg_pt: 01_extract-data-from-google-spreadsheet.py
	./01_extract-data-from-google-spreadsheet.py -gg $(GOOGAME) -g $(GAME) -p $(POINT)

clean_cat:
	rm -rf ../games/$(GAME)/03_concat-google/*

cat_goog: 03_concatentate-google-extract.r
	@Rscript ./03_concatentate-google-extract.r $(GAME)

clean_clean:
	rm -rf ../games/$(GAME)/04_clean-game/*

clean_game: 04_clean-and-expand-game-play.r
	@Rscript ./04_clean-and-expand-game-play.r $(GAME) 2>&1 | tee ../games/$(GAME)/04_clean-game/$(GAME)_gameplay-clean-log.txt

check_clean: 05_sanity-check-game-play.r
	@Rscript ./05_sanity-check-game-play.r $(GAME) 2>&1 | tee ../games/$(GAME)/04_clean-game/$(GAME)_gameplay-checks-log.txt

clean_possess:
	rm -rf ../games/$(GAME)/06_possess-game/*

possess_game: 06_resolve-possessions.r
	@Rscript ./06_resolve-possessions.r $(GAME) 2>&1 | tee ../games/$(GAME)/06_possess-game/$(GAME)_gameplay-possessions-log.txt

clean_pass:
	rm -rf ../games/$(GAME)/07_pass-game/*

pass_game: 07_resolve-passes.r
	@Rscript ./07_resolve-passes.r $(GAME) 2>&1 | tee ../games/$(GAME)/07_pass-game/$(GAME)_passes-log.txt

clean_web:
	rm -rf ../games/$(GAME)/09_html/*

web: 
	@./08_compile-HTML.r $(GAME) 2>&1 | tee ../games/$(GAME)/09_html/$(GAME)_nowPlaying-log.txt

web_copy:
	cp ../games/$(GAME)/09_html/$(GAME)_live-stats.html ../web/

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

marshal: 11_marshal-game-play-and-aggregate.r ../games/2014*/07_resolvedGame/*_gameplay-resolved.tsv
	$(tidy-spin)

index.html: ../web/index.rmd ../games/2014*/*at-last-point.yaml
	cd ../web; Rscript -e "rmarkdown::render('index.rmd', quiet = TRUE)"

r_bits: clean_cat clean_clean clean_possess clean_pass cat_goog clean_game check_clean possess_game pass_game web web_copy

pt_fix: eg_pt clean_cat clean_clean clean_possess clean_pass cat_goog clean_game check_clean possess_game pass_game web web_copy

yo: eg_pt clean_cat clean_clean clean_possess cat_goog clean_game check_clean possess_game pass_game web web_copy set_web sync

sync:
	cd ../web; rsync -av --exclude-from=/Users/jenny/.rsync/exclude ./ jenny@stat.ubc.ca:public_html/notOcto/vanNH/

clean:
	rm -rf *.md *.html *~ figure/
