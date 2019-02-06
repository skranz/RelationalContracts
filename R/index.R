xrow.a.to.ax = function(xrow,a, lag.cumsum.na=g$sdf$lag.cumsum.na, g=NULL) {
  lag.cumsum.na[xrow]+a
}

eq.a.to.ax = function(g,a) {
  a+g$sdf$lag.cumsum.na
}

eq.ax.to.a = function(g,ax) {
  ax-g$sdf$lag.cumsum.na
}
