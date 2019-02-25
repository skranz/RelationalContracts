# Helper function to find differences between two equilibria

compare.eq = function(eq1,eq2=g[["eq"]],g, verbose=TRUE) {
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

eq.li.compare = function(g,eq.li, split.col = "t") {
  if (is.data.frame(eq.li)) {
    li = rev(split(eq.li, eq.li[[split.col]]))
  } else {
    li = eq.li
  }
  if (length(li)<=1) return(NULL)
  prev.eq = li[[1]]
  stationary.eq.distribution(g, prev.eq)
  for (row in 2:length(li)) {
    eq = li[[row]]


  }

}

find.eq.li.action.repetitions = function(eq.li){
  restore.point("eq.li.action.repetitions")
  if (is.data.frame(eq.li)) {
    eq = eq.li
    acols = c("s.ae","s.a1","s.a2","ae", "a1","a2")
    acols = intersect(acols, colnames(eq))
    ids = unlist(lapply(rev(unique(eq$t)), function(t) {
      df = eq[eq$t==t,]
      paste0(paste.df.cols(df, acols, sep=","), collapse="|")
    }))

  } else {
    eq = eq.li[[1]]
    acols = c("s.ae","s.a1","s.a2","ae", "a1","a2")
    acols = intersect(acols, colnames(eq))
    ids = unlist(lapply(eq.li, function(eq) {
      restore.point("shdfrfrg")
      paste0(paste.df.cols(eq, acols, sep=","), collapse="|")
    }))
  }
  uni = unique(ids)
  groups = match(ids, uni)
  groups
}
