# Helper function to find differences between two equilibria

compare.eq = function(eq1,eq2=g$eq,g, verbose=TRUE) {
  restore.point("compare.eq")
  is.multi.stage = has.col(eq1,"s.ae")

  payoff.cols = c("r1","r2","U","v1","v2")
  pm1 = as.matrix(eq1[,payoff.cols])
  pm2 = as.matrix(eq2[,payoff.cols])

  p.diff = pm2-pm1
  p.diff.rows = which(rowSums(abs(p.diff))>0)
  if (length(p.diff.rows)==0) {
    cat("\nAll payoffs are identical")
  } else {
    cat(paste0("\nPayoffs differ in rows ", paste0(p.diff.rows, collapse=", ")))
  }

  action.cols = c("s.ae","s.a1","s.a2","ae", "a1","a2")
  action.cols = intersect(action.cols, colnames(eq1))
  action.cols = intersect(action.cols, colnames(eq2))
  a.diff = as.matrix(eq2[,action.cols]-eq1[,action.cols])
  a.diff.rows = which(rowSums(abs(a.diff))>0)

  if (length(a.diff.rows)==0) {
    cat("\nAll actions are identical.")
  } else {
    cat(paste0("\nActions differ in rows ", paste0(a.diff.rows, collapse=", ")))
  }
  rows = sort(unique(c(p.diff.rows, a.diff.rows)))
  res = cbind(eq1[,"x",drop=FALSE],p.diff, a.diff)[rows,]
  invisible(res)
}
