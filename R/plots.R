plot.spe.payoff.set = function (g,x=eq$x[1],t=1,  eq=g[[eq.field]], eq.field = "spe",xlim=NULL, ylim=NULL, add=FALSE, colors=c("#377EB8","#E41A1C", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),plot.r=TRUE, alpha=0.5, black.border=TRUE, add.state.label=TRUE, add.xlim=NULL, add.ylim=NULL, extend.lim.perc=0.05, ...) {
  restore.point("plot.spe.payoff.set")
  old.par = par()
  on.exit(suppressWarnings(par(old.par)))
  par(mar = c(4, 4, 1, 1))

  dat = eq
  if (!is.null(x)) {
    rows = match(x, dat$x)
    dat = dat[rows,]
  }
  if (t %in% colnames(dat) & !is.null(t)) {
    rows = match(t, dat$t)
    dat = dat[rows,]
  }

  dat$u1.max = dat$U-dat$v2
  dat$u2.max = dat$U-dat$v1

  if (is.null(xlim)) {
    xlim = range(c(dat$v1, dat$u1.max, add.xlim))
    xlim = extend.range(xlim, extend.lim.perc)
  }
  if (is.null(ylim)) {
    ylim = range(c(dat$v2, dat$u2.max, add.ylim))
    ylim = extend.range(ylim, extend.lim.perc)
  }


  plot(xlim, ylim, col = "white", type = "s", xlim = xlim,
        ylim = ylim, xlab = "u1", ylab = "u2")




  for (row in seq_len(NROW(dat))) {
    col = colors[row]
    fill = adjustcolor(col,alpha.f=alpha)
    #border="blue"; fill="blue"
    polygon(
      x=c(dat$v1[row],dat$u1.max[row],dat$v1[row]),
      y=c(dat$v2[row],dat$v2[row], dat$u2.max[row]),
      border=if (black.border) "black" else col,
      col=fill
    )
    if (plot.r) {
      points(dat$r1[row],dat$r2[row], pch=19, col=adjustcolor("black",alpha.f=0.8))
    }
  }
  if (plot.r) {
    points(dat$r1,dat$r2, pch=19, col=adjustcolor("black",alpha.f=0.8))
  }
  if (add.state.label) {
    text(
      x=(0.75*dat$v1+0.25*dat$u1.max),
      y=(0.75*dat$v2+0.25*dat$u2.max),
      labels = dat$x
    )
  }
}

extend.range = function(range, perc=0.05) {
  range[1] = range[1] - perc*abs(diff(range))
  range[2] = range[2] + perc*abs(diff(range))
  range

}
