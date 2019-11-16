sum.same.transitions = function(trans.df, action.cols=colnames()) {

}


compute.eq.trans.mat = function(g, ae = eq$ae, eq=g[["eq"]]) {
  restore.point("compute.eq.trans.mat")
  if (!is.null(g$ax.trans)) {
    ax = eq.a.to.ax(g,a=ae)
    g$ax.trans[ax,,drop=FALSE]
  } else {
    nx = NROW(g$sdf)
    mat = matrix(0,nx,nx)
    colnames(mat) = g$sdf$x
    for (xrow in 1:NROW(g$sdf)) {
      tm = g$sdf$trans.mat[[xrow]]
      if (NROW(tm)==0) {
        mat[xrow, colnames(tm)] = 1
      } else {
        mat[xrow, colnames(tm)] = tm[ae[xrow],]
      }
    }
    mat
  }

}



stationary.eq.distribution = function(g, eq=g[["eq"]], tol = 1e-10, start=rep(1/NROW(g$sdf), NROW(g$sdf)), iterations=200, ae = eq$ae) {
  restore.point("stationary.eq.distribution")
  mat = compute.eq.trans.mat(g, eq=eq, ae=ae)
  res = start
  for (i in 1:iterations) {
    nres = res %*% mat
    change = max(abs(res-nres))
    res = nres
  }
  res = as.vector(res)
  attr(res,"change") <- change
  res
}


