examples.diagnose = function() {
  tra = diagnose_transitions(g)
}

#' Take a look at the computed transitions for each state
#' using separate data frames
diagnose_transitions = function(g) {
  li = lapply(1:NROW(g$sdf), function(row) {
    restore.point("dfhfh")

    tm = g$sdf$trans.mat[[row]]
    if (NROW(tm)==0) return(NULL)
    bind_cols(g$sdf$a.grid[[row]], as_tibble(tm))
  })
  names(li) = g$sdf$x
  li
}
