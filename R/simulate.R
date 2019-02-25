simulate.eq = function(g,periods=100,x0=g$sdf$x[[1]], eq=g[["eq"]]) {
  restore.point("simulate.eq")

  sdf = g$sdf
  xrows = rep(0,periods)
  xrow = match(x0,sdf$x)

  xvec = 1:NROW(sdf)
  names(xvec) = sdf$x

  ae.col = "ae"
  #cat(" ",names(trans)[ind])
  t = 1
  xrows[t] = xrow
  while(t < periods) {
    t = t+1

    trans.mat = sdf$trans.mat[[xrow]]
    if (NROW(trans.mat)>0) {
      ae = eq[xrow,ae.col]
      trans = trans.mat[ae,]
      ind = sample.int(length(trans),1,prob = trans)
      xrow = xvec[names(trans)[ind]]

      #if (x.df$x1A[[xrow]]<2) {
      #  restore.point("shfdhfrfr")
      #  stop()
      #}

      #cat(" ",names(trans)[ind])
    }
    xrows[t] = xrow
  }
  res = cbind(quick_df(.t=1:periods),g$x.df[xrows,,drop=FALSE])
  res
}
