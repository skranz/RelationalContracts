plot.rne.payoff.set = function(g, x=eq$x[1], eq = g[["rne"]],...) {
  plot_eq_payoff_set(g=g,x=x, eq=eq,...)
}

#' Show a base R plot of equilibrium payoff set
#'
#' @param g The game object for which an equilibrium has been solved
#' @param x A character vector of the state(s) for which the (continuation) equilibrium payoff set shall be shown. By default only the first stage.
#' @param eq An equilibrium object. By default the last solved equilibrium.
#' @param plot.r Shall negotiation payoffs be shown as a point on the Pareto-frontier (default = TRUE)
#' @param xlim as in \code{\link{plot.default}}
#' @param ylim as in \code{\link{plot.default}}
#' @param add as in \code{\link{plot.default}} Setting \code{add=FALSE} can be useful to compare payoff sets of different games.
#' @param alpha opacity of the fill color
plot_eq_payoff_set = function (g,x=eq$x[1],t=1,  eq=if(use.vr) get_eq(g, add.vr=TRUE) else g[["eq"]], xlim=NULL, ylim=NULL, add=FALSE,plot.r=TRUE, alpha=0.8, black.border=TRUE, add.state.label=is.null(labels), labels=NULL, colors=c("#377EB8","#E41A1C", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"), add.xlim=NULL, add.ylim=NULL, extend.lim.perc=0.05,use.vr = FALSE, ...) {
  restore.point("plot_eq_payoff_set")
  #old.par = par()
  #on.exit(suppressWarnings(par(old.par)))
  if (!add) {
    par(mar = c(4, 4, 1, 1))
  }
  dat = eq
  if (!is.null(x)) {
    rows = match(x, dat$x)
    dat = dat[rows,]
  }
  if (t %in% colnames(dat) & !is.null(t)) {
    rows = match(t, dat$t)
    dat = dat[rows,]
  }

  if (use.vr) {
    dat$v1 = dat$vr1
    dat$v2 = dat$vr2
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


  if (!add) {
    plot(xlim, ylim, col = "white", type = "s", xlim = xlim,
        ylim = ylim, xlab = "u1", ylab = "u2")
  }

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
  if (add.state.label & is.null(labels))
    labels = dat$x
  if (!is.null(labels)) {
    text(
      x=(0.75*dat$v1+0.25*dat$u1.max),
      y=(0.75*dat$v2+0.25*dat$u2.max),
      labels = labels
    )
  }
}

#' Use ggplotly to show an animation of the payoff sets of a list of equilibria
#' @export
animate_eq_li = function(g, eq.li, x=g$sdf$x[1],...) {
  animate_capped_rne_history(g, eq.li=eq.li, x=x,...)
}

#' Use ggplotly to show an animation of the payoff sets of a capped RNE going from t=T to t=1
#' @export
animate_capped_rne_history = function(g,x=g$sdf$x[1], hist = g$eq.history, colors=c("#377EB8","#E41A1C", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"), alpha=0.4, add.state.label=TRUE, add.grid=FALSE, add.diag=FALSE, add.plot=NULL, eq.li=NULL) {
  add.plot = substitute(add.plot)

  restore.point("animate_capped_rne_history")

  if (!is.null(eq.li)) {
    hist = bind_rows(eq.li)
    hist$t = rep(rev(seq_along(eq.li)),each=NROW(g$sdf))
  }

  if (NROW(hist)==0) {
    stop("No results for periods t>1 have been saved. Call rel_capped_rne with the option save.history=TRUE.")
  }



  if (!is.null(x)) {
    dat = hist[hist$x %in% x,]
  } else {
    dat = hist
    x = rev(g$sdf$x)
  }
  if (NROW(dat)==0) {
    stop("None of your selected states exists.")
  }
  dat$x.ord = match(dat$x,x)

  poly = rbind(
    transmute(dat, point=1,x=x,x.ord=x.ord, t=t, px=v1,py=v2,r1=r1,r2=r2),
    transmute(dat, point=2,x=x,x.ord=x.ord, t=t, px=U-v2,py=v2,r1=r1,r2=r2),
    transmute(dat, point=3,x=x,x.ord=x.ord,t=t, px=v1,py=U-v1,r1=r1,r2=r2)
  ) %>% arrange(desc(t),x.ord,point)

  poly$x = factor(poly$x,levels=x)

  x.ind = match(poly$x, g$sdf$x)
  poly$fill = colors[x.ind]
  if (length(alpha)>1) {
    poly$alpha = alpha[x.ind]
  } else {
    poly$alpha = alpha
  }
  poly$frame = max(poly$t)-poly$t

  poly$state = poly$x
  library(ggplot2)
  library(plotly)

  suppressWarnings({
    gg = ggplot(poly, aes(x = px, y = py, fill=x, alpha=alpha, frame=frame))
    gg = gg+
        geom_point(aes(x=r1,y=r2,frame=frame), color="black", show.legend = FALSE) +
        geom_polygon(color="black") +
        scale_alpha_continuous(guide=FALSE) +
        xlab("u1") + ylab("u2") + theme_bw()
    if (!add.grid) {
      gg = gg + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    }
    if (!is.null(add.plot)) {
      code = paste0("gg + ", deparse1(add.plot))
      gg = eval(parse(text = code))
    }

    if (add.diag) {
      gg = gg + geom_abline(intercept = 0, slope=1, colour="black", alpha=0.4)
    }
  })
  ggplotly(gg) %>%
    config(displayModeBar = F) %>%
    animation_slider(
      currentvalue = list(prefix = "Periods before Cap ", font = list(color="black", size="0.6em"))
    )

}

extend.range = function(range, perc=0.05) {
  range[1] = range[1] - perc*abs(diff(range))
  range[2] = range[2] + perc*abs(diff(range))
  range

}
