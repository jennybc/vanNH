resolve_passes <- function(x) {
  ## detect data with more than one possession indicated by poss_abs | poss_rel
  if(!is.null(x$poss_abs)) {
    poss <- "poss_abs"
  } else if (!is.null(x$poss_rel)) {
    poss <- "poss_rel"
  } else {
    poss <- NULL
  }
  if(!is.null(poss)) {
    if(length(unique(x[[poss]])) > 1) {
      message("  ALERT point ", x$point[1], " has more than one possession")
      message("  resolving passes only for the last possession provided")
      x <- x[x[[poss]] == max(x[[poss]]), ]
    }
  }
  ## detect data with more than one possession indicated via poss_team
  if(length(unique(x$poss_team)) > 1) {
    message("  ALERT detected >1 poss_team when resolving passes for ",
            "a possession in point ", x$point[1])
    message("  no passes will be returned for the affected possession")
    return(NULL)
  }
  
  require(plyr)
  x <- mutate(x,
              is_alpha = pl_code %in% c("CTH", "PU"),
              is_omega = pl_code %in% c("CTH", "D", "G", "TD", "OV", "TO"),
              e_code = paste(who, pl_code, sep = "-"))
  ## offensive foul *can* be omega ... but not always ... handled below
  n <- nrow(x)
  ## trim off the top: pulls, subs out or in, timeouts
  alpha_row <- min(which(!(x$pl_code %in% c('P', 'SO', 'SI', 'TO'))))
  ## trim off the bottom: anything after a goal
  ## at time I wrote this, sole example was a defensive foul on successful goal
  is_a_goal <- x$pl_code == 'G'
  omega_row <- if(any(is_a_goal)) max(which(is_a_goal)) else n
  y <- x[alpha_row:omega_row, ]
  n <- nrow(y)
  if(y$e_code[n] == 'O-F') y$is_omega[n] <- TRUE
  
  if(n < 1)
    stop("no game play available for pass detection")
     
  tmp <- vector(mode = "list", length = n)
  i <- 1
  while(i <= n) {
    if(y$is_alpha[i]) {
      suppressWarnings(next_omega <- min(which(seq_len(n) > i & y$is_omega)))
      suppressWarnings(next_alpha <- min(which(seq_len(n) > i & y$is_alpha)))
      desc <- switch(which.min(c(next_omega, next_alpha, n)), "ao", "aa", "an")
      end_row <- min(next_omega, next_alpha, n)
      innards <- if(end_row - i > 1) {
        paste(y$e_code[(i + 1):(end_row - 1)], collapse = ",")
      } else ''
      tmp[[i]] <-
        with(y,
             data.frame(beg_event = event[i], end_event = event[end_row],
                        desc = desc, n_inn = end_row - i - 1, 
                        beg_code = e_code[i], #beg_pnum = I(pl_pnum[i]),
                        beg_plyr = I(paste(pl_team[i], pl_pnum[i], sep = "-")),
                        innards = I(innards),
                        end_code =if(desc == "an")NA_character_ else e_code[end_row],
                        #end_pnum = I(pl_pnum[end_row]),
                        end_plyr = I(paste(pl_team[end_row], pl_pnum[end_row], sep = "-"))))
      if(i == end_row) {
        break
      } else {
          i <- end_row
          next
        }
    } else {
      i <- i + 1
      next
    }
  }
  return(do.call("rbind", tmp))
}
