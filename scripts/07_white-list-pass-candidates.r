## constructs a white list for vetting pass data

## not all of these are valid passes!
## this just captures valid game play I have seen

## if game play comes that is not found on this white list, I want to take a
## hard look at it

no_brainers <-
  expand.grid(beg_code = c('O-CTH', 'O-PU'),
              end_code = c('O-CTH', 'O-G', 'D-D', 'O-TD', 'O-TO', 'O-F'),
              innards = c('', 'D-F', 'O-VP', 'O-F', 'D-VP'))

throwaways <- expand.grid(beg_code = c('O-CTH', 'O-PU'),
                          end_code = NA,
                          innards = c('', 'D-F'))

subs <- expand.grid(beg_code = 'O-CTH',
                    end_code = c('O-CTH', 'O-PU'),
                    innards = c('O-SO,O-SI', 'D-SO,D-SI',
                                'D-F,O-SO,O-SI', 'D-F,D-SO,D-SI'))

timeouts <- expand.grid(beg_code = c('O-CTH', 'O-PU'),
                        end_code = 'O-PU',
                        innards = 'O-TO')

misc_cth_pu <-
  data.frame(beg_code = 'O-CTH',
             end_code = 'O-PU',
             innards = c('D-F,O-TO', 'D-VP,O-TO', 'O-TO,D-F', 'O-VP'))

misc_pu_pu <-
  data.frame(beg_code = 'O-PU',
             end_code = 'O-PU',
             innards = 'D-F,O-SO,D-SO,O-SI,D-SI')

ov <- expand.grid(beg_code = c('O-CTH', 'O-PU'),
                  end_code = 'O-OV',
                  innards = c('', 'D-F'))

white_list <- rbind(no_brainers, throwaways, subs, timeouts,
                    misc_cth_pu, misc_pu_pu, ov)

rm(no_brainers, throwaways, subs, timeouts,
   misc_cth_pu, misc_pu_pu, ov)
