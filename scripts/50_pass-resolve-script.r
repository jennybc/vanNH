resolve_passes <- function(x) {
  n <- nrow(x)
  ## trim off the top: pulls, subs out or in, timeouts
  alpha_row <- min(which(!(x$pl_code %in% c('P', 'SO', 'SI', 'TO'))))
  ## trim off the bottom: anything after a goal
  ## at time I wrote this, sole example was a defensive foul on successful goal
  is_a_goal <- x$pl_code == 'G'
  omega_row <- if(any(is_a_goal)) max(which(is_a_goal)) else n
  y <- x[alpha_row:omega_row, ]
  n <- nrow(y)
  
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
                beg_code = y$e_code[i], beg_pnum = pl_pnum[i],
                innards = innards,
                end_code =if(desc == "an")NA_character_ else y$e_code[end_row],
                end_pnum = pl_pnum[end_row]))
      if(i == end_row) {
        break
        } else {
          i <- end_row
          next
        }
    } else {
      #tmp[[i]] <- "huh?"
      if(i == end_row) {
        break
      }
    }
  }
  return(data.frame(do.call("rbind", tmp)))
}
