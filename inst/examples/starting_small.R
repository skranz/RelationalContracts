starting_small_variant2 = function() {

  A.fun = function(x1,x2,x.seq,e.seq=c(0,1),...) {
    restore.point("A.fun")
    list(
      A1=list(to_x1=unique(c(x1, min(c(x.seq[x.seq>x1],max(x.seq))), max(x.seq))),
        h1=unique(c(0,x2))),
      A2=list(to_x2=unique(c(x2, min(c(x.seq[x.seq>x2],max(x.seq))), max(x.seq))),
        e=e.seq, h2=unique(c(0,x1)))
    )
  }


  vec.pi.fun = function(ax.df, cost=1/3,...) {
    restore.point("vec.pi.fun")
    mutate(ax.df,
      pi1 = e - h2,
      pi2 = cost*e^3 - h1
    )
  }

  vec.trans.fun = function(ax.df,x.seq, ...) {
    restore.point("trans.fun")
    ax.df %>%
      select(xs=x, to_x1, to_x2) %>%
      unique %>%
      mutate(xd = paste0(to_x1,"_", to_x2), prob=1)
  }
  #x.seq = c(0,0.01, 0.05,seq(0.1,1,by=0.1))
  x.seq = c(0, 0.01,0.02, 0.05, 0.1, 0.2, 0.4, 1)
  #x.seq = seq(0,1,by=0.05)
  #x.seq = 0
  x.df = as_tibble(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)
  g = rel_game("Starting Small Game") %>%
    rel_param(cost = 1/3, x.seq=x.seq, e.seq=seq(0,1,by=0.1)) %>%
    rel_states(x.df,A.fun=A.fun, vec.pi.fun=vec.pi.fun, vec.trans.fun=vec.trans.fun) %>%
    rel_compile()


  g = g %>%  rel_capped_rne(T=100, delta=0.3, rho=0, save.history = FALSE, use.cpp = TRUE, add.stationary = FALSE, save.details = FALSE,tie.breaking = "equal_r")
  eq = capped = get.eq(g)


  # State transitions on the equilibrium path
  rne.diagram(g, show.own.loop = TRUE, just.eq.chain = TRUE)

  # Effort level
  eq$r_lab = paste0(eq$ae.e1, " ", eq$ae.e2)
  eq$e = eq$ae.e1+eq$ae.e2
  library(ggplot2)
  ggplot(filter(eq, !(x1>0 &x1<0.1),!(x2>0 & x2<0.1)), aes(x=x1,y=x2, fill=e)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))


}



starting_small_variant1 = function() {

  A.fun = function(x1,x2,x.seq,...) {
    restore.point("A.fun")
    list(
      A1=list(to_x1=x.seq[x.seq>=x1],e1=c(0,1),h1=c(0,x2)),
      A2=list(to_x2=x.seq[x.seq>=x2],e2=c(0,1),h2=c(0,x1))
    )
  }


  vec.pi.fun = function(ax.df, cost,...) {
    restore.point("vec.pi.fun")
    mutate(ax.df,
      pi1 = -e1*cost+e2-h2,
      pi2 = -e2*cost+e1-h1
    )
  }

  vec.trans.fun = function(ax.df,x.seq, ...) {
    restore.point("trans.fun")
    ax.df %>%
      select(xs=x, to_x1, to_x2) %>%
      unique %>%
      mutate(xd = paste0(to_x1,"_", to_x2), prob=1)
  }
  x.seq = c(0,0.01, 0.05,seq(0.1,1,by=0.1))
  x.df = as_tibble(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)
  g = rel_game("Starting Small Game") %>%
    rel_param(cost = 0.8, x.seq=x.seq) %>%
    rel_states(x.df,A.fun=A.fun, vec.pi.fun=vec.pi.fun, vec.trans.fun=vec.trans.fun) %>%
    rel_compile()


  g = g %>%  rel_capped_rne(T=100, delta=0.9, rho=0.7, save.history = FALSE, use.cpp = TRUE, add.stationary = FALSE, save.details = FALSE,tie.breaking = "equal_r")
  eq = capped = get.eq(g)

  # State transitions on the equilibrium path
  rne.diagram(g, show.own.loop = TRUE, just.eq.chain = TRUE)

  # Effort level
  eq$r_lab = paste0(eq$ae.e1, " ", eq$ae.e2)
  eq$e = eq$ae.e1+eq$ae.e2
  library(ggplot2)
  ggplot(filter(eq, !(x1>0 &x1<0.1),!(x2>0 & x2<0.1)), aes(x=x1,y=x2, fill=e)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))


}

