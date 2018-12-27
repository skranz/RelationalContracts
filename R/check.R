check.rel.colnames = function(df, cols, msg) {
  check.rel(all(colnames(df)==cols), msg)
}

check.rel.has.colscolnames = function(df, cols, msg) {
  check.rel(all(colnames(df)==cols), msg)
}


check.rel = function(cond, msg) {
  if (!cond) stop(msg,call. = FALSE)
}
