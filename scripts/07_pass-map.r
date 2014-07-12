## constructs a map from pass data to these categories
##   * completed
##   * defensed
##   * dropped
##   * violated (VST or VTT)
##   * offensive foul causing turnover
##   * thrown away
##   * not a pass (example: a timeout that "ends" a pass)

compl <- expand.grid(pclass = "compl",
                     beg_code = c('O-CTH', 'O-PU'),
                     end_code = c('O-CTH', 'O-G'))

ofoul <- expand.grid(pclass = "ofoul",
                     beg_code = c('O-CTH', 'O-PU'),
                     end_code = 'O-F')

taway <- expand.grid(pclass = "taway",
                     beg_code = c('O-CTH', 'O-PU'),
                     end_code = NA)

defd <- expand.grid(pclass = "defd",
                    beg_code = c('O-CTH', 'O-PU'),
                    end_code = 'D-D')

drop <- expand.grid(pclass = "drop",
                    beg_code = c('O-CTH', 'O-PU'),
                    end_code = 'O-TD')

pass_map <- rbind(compl, ofoul, taway, defd, drop)

rm(compl, ofoul, taway, defd, drop)
