
starting_small = function() {

  A.fun = function(x1,x2,x.seq,...) {
    restore.point("A.fun")
    list(
      A1=list(to_x1=x.seq[x.seq>=x1]),
      A2=list(to_x2=x.seq[x.seq>=x2])
    )
  }


  static.A.fun = function(x1,x2,e.seq=c(0,1),...) {
    restore.point("static.A.fun")
    list(
      A1=list(h1=unique(c(0,x2))),
      A2=list(e=e.seq, h2=unique(c(0,x1)))
    )
  }
  static.pi.fun = function(ax.df, cost=1/2,...) {
    restore.point("pi.fun")
    mutate(ax.df,
           pi1 = e - h2,
           pi2 = -cost*e^2 - h1
    )
  }

  trans.fun = function(ax.df,x.seq, ...) {
    restore.point("trans.fun")
    ax.df %>%
      select(xs=x, to_x1, to_x2) %>%
      unique %>%
      mutate(xd = paste0(to_x1,"_", to_x2), prob=1)
  }
  x.seq = seq(0,0.5, by=0.05)
  x.df = as_tibble(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)
  g = rel_game("Starting Small Game") %>%
    rel_param(cost = 1/2, x.seq=x.seq, e.seq=seq(0,1,by=0.01)) %>%
    rel_states(x.df,A.fun=A.fun,static.A.fun=static.A.fun, pi1=0, pi2=0, static.pi.fun=static.pi.fun, trans.fun=trans.fun) %>%
    rel_compile() %>%
    rel_solve_repgames()

  g = g %>%  rel_capped_rne(T=1000, adjusted.delta = 0.25, rho=0.7)
  eq = capped = get_eq(g)
  eq$ae.e

  #rne = rel_rne_from_capped(g,iterations=20)

  # State transitions on the equilibrium path
  eq_diagram(g, just.eq.chain = TRUE)

  eq_diagram(g, show.own.loop = TRUE, just.eq.chain = TRUE, label.fun = function(rne,...)  paste0(rne$x1, " ", rne$x2))

  rep = get_repgames_results(g) %>%
    mutate(r_lab = paste0(round(r1,2)," ", round(r2,2)), e = ae.e)
  library(ggplot2)
  ggplot(rep, aes(x=x1,y=x2, fill=e)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))

}

