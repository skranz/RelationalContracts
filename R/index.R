xrow.a.to.ax = function(xrow,a, lag.cumsum.na=g$sdf$lag.cumsum.na, g=NULL) {
  lag.cumsum.na[xrow]+a
}

eq.a.to.ax = function(g,a) {
  g$sdf$lag.cumsum.na+a
}
