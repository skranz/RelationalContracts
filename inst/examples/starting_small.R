
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
  vec.static.pi.fun = function(ax.df, cost=1/2,...) {
    restore.point("vec.pi.fun")
    mutate(ax.df,
           pi1 = e - h2,
           pi2 = -cost*e^2 - h1
    )
  }

  vec.trans.fun = function(ax.df,x.seq, ...) {
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
    rel_states(x.df,A.fun=A.fun,static.A.fun=static.A.fun, pi1=0, pi2=0, vec.static.pi.fun=vec.static.pi.fun, vec.trans.fun=vec.trans.fun) %>%
    rel_compile() %>%
    rel_solve_repgames()

  g = g %>%  rel_capped_rne(T=1000, adjusted.delta = 0.25, rho=0.7, save.history = FALSE, use.cpp = TRUE, add.stationary = FALSE, save.details = FALSE,tie.breaking = "equal_r")
  eq = capped = get.eq(g)
  eq$ae.e

  #rne = rel_rne_from_capped(g,iterations=20)

  # State transitions on the equilibrium path
  rne.diagram(g, show.own.loop = TRUE, just.eq.chain = TRUE)
  rne.diagram(g, show.own.loop = !TRUE, just.eq.chain = !TRUE,only.eq.edges = TRUE)
  rne.diagram(g, show.own.loop = TRUE, just.eq.chain = TRUE,x0 = "0.3_0")

  rne.diagram(g, show.own.loop = TRUE, just.eq.chain = TRUE, label.fun = function(rne,...)  paste0(rne$x1, " ", rne$x2))
  res = rne.diagram(g, show.own.loop = TRUE, just.eq.chain = TRUE, return.df=TRUE)

  rep = get.repgames.results(g) %>%
    mutate(r_lab = paste0(round(r1,2)," ", round(r2,2)), e = ae.e)
  library(ggplot2)
  ggplot(rep, aes(x=x1,y=x2, fill=e)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))


  # Compute change in r when increasing vulnerability
  rep = mutate(rep,
               next_x1 = x1+0.1,
               next_x2 = x2+0.1
  )
  rep = left_join(rep, transmute(rep, next_x1=x1,x2=x2, next_r1=r1), by=c("next_x1","x2"))
  rep = left_join(rep, transmute(rep, next_x2=x2,x1=x1, next_r2=r2), by=c("next_x2","x1"))
  rep = mutate(rep,
               r1_change = next_r1-r1,
               r2_change = next_r2-r2,
               dd_r_change = round(r1_change-r2_change,12),
               r_lab = paste0(round(r1_change,3),"_", round(r2_change,3))
  )
  ggplot(rep, aes(x=x1,y=x2, fill=dd_r_change)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))




  # Effort level
  eq$r_lab = paste0(eq$e)
  library(ggplot2)
  ggplot(eq, aes(x=x1,y=x2, fill=e)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))


  # Transitions
  eq = mutate(eq,
    x1_gain = to_x1-x1,
    x2_gain = to_x2-x2
  )
  eq$r_lab = paste0(round(eq$r1,2), " ", round(eq$r2,2))
  library(ggplot2)
  ggplot(eq, aes(x=x1,y=x2, fill=to_x1)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))


}

