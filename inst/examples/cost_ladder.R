examples.cost.ladder.cournot = function() {
  # A Bertrand duopoly
  # with endogenous marginal cost
  pi.fun = function(ax.df, i.cost=10,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = -i.cost*i1,
      pi2 = -i.cost*i2
    )
  }
  static.pi.fun = function(ax.df,x.max,a=x.max, b=1,...) {
    restore.point("pi.fun")
    res = mutate(ax.df,
      Q = q1+q2,
      p = a-b*Q,
      pi1 = (p-x1)*q1,
      pi2 = (p-x2)*q2
    )
    attr(res,"extra.col") = TRUE
    # dat = res %>% filter(x1==5, x2==5)
    res
  }

  trans.fun = function(ax.df,x.max,dep.prob=0,alpha=1,x.min=0, ...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()
    ax.df = mutate(ax.df,
      i1.prob = alpha*i1 / (1+alpha*i1),
      i2.prob = alpha*i2 / (1+alpha*i2)
    )
    dp = dep.prob
    trans = independent.transitions(ax.df,
      trans_var("nx1",default=x1,lower=0, upper=x.max,
        trans_val(x1-1, (1-dp)*i1.prob),
        trans_val(x1+1, dp*(1-i1.prob))
      ),
      trans_var("nx2",default=x2,lower=0, upper=x.max,
        trans_val(x2-1, (1-dp)*i2.prob),
        trans_val(x2+1, dp*(1-i2.prob))
      )
    )
    trans = mutate(trans,
        xd = paste0(nx1,"_",nx2),
        xs=x
      ) %>%
      select(xs,xd,i1,i2,prob)
    trans
  }



  x.min=0; x.max = 5; a = x.max
  i.seq=c(0,0.5,1,2)
  q.seq=seq(0,a, length=41)
  x.seq = seq(x.max,x.min, by=-1)
  x.df = as_data_frame(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)
  g = rel_game("Cournot with Cost Ladder") %>%
    rel_param(x.min=x.min,x.max=x.max,dep.prob=0.1,a=x.max,b=1, i.cost=1.5,alpha=1) %>%
    rel_states(x.df,A1=list(i1=i.seq),A2=list(i2=i.seq),static.A1 = list(q1=q.seq), static.A2 = list(q2=q.seq),pi.fun=pi.fun, trans.fun=trans.fun, static.pi.fun = static.pi.fun, x.T = "0_0") %>%
    rel_compile()


  g = rel_mpe(g, delta=0.9)
  mpe = get_mpe(g)
  mpe$r_lab = paste0(
    "u ",round(mpe$u1,2)," ", round(mpe$u2,2),
    "\nq ",round(mpe$q1,1)," ", round(mpe$q2,1),
    "\ni ",round(mpe$i1,1)," ", round(mpe$i2,1)
  )
  library(ggplot2)
  ggplot(mpe, aes(x=x1,y=x2, fill=stat.prob)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))



  g = g %>%  rel_capped_rne(T=50, delta=0.7, rho=0.8, save.history = !FALSE, use.cpp = TRUE, add.stationary = TRUE, save.details = !TRUE)
  eq = get_eq(g)

  rho.seq = c(1,0.9,0.5,0)
  sim = bind_rows(lapply(rho.seq, function(rho) {
    cat("\nrho = ",rho)
    g = g %>%  rel_capped_rne(T=50, delta=0.9, rho=rho, save.history = FALSE, use.cpp = TRUE, add.stationary = TRUE)
    eq = get_eq(g)
    eq$rho = rho

    eq
  }))
  sim = mutate(sim, r_lab = paste0(
    "r ",round(r1,2)," ", round(r2,2),
    "\nq ",round(q1,2)," ", round(q2,2),
    "\ni ",round(i1,1)," ", round(i2,1)
  ))
  ggplot(sim, aes(x=x1,y=x2, fill=stationary)) + geom_raster(interpolate=FALSE) + geom_text(aes(label=r_lab), color="white", alpha=0.5, size=2.5, label.padding=unit(0.1,"lines")) + facet_wrap(~rho)


  library(plotly)
  library(ggplot2)

  gg = ggplot(sim, aes(x=x1,y=x2, fill=stationary, frame=rho)) + geom_raster(interpolate=FALSE) + geom_text(aes(label=r_lab), color="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))
  ggplotly(gg) %>%
    config(displayModeBar = F) %>%
    animation_slider(
      currentvalue = list(prefix = "rho ", font = list(color="black", size="0.6em"))
    ) %>%
    animation_opts(frame=1000, transition=0)



  eq$r_lab = paste0(
    "r ",round(eq$r1,2)," ", round(eq$r2,2),
    "\nq ",round(eq$q1,2)," ", round(eq$q2,2),
    "\ni ",round(eq$i1)," ", round(eq$i2)
  )

  ggplot(eq, aes(x=x1,y=x2, fill=stationary)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))

  animate_capped_rne_history(g,x = c("5_5","0_0","5_1"))


  eq_diagram(g, just.eq.chain = !TRUE)

  sdf = g$gs$sdf
  cbind(g$sdf$a.grid[[1]],pi1= g$sdf$pi1[[1]],pi2=g$sdf$pi2[[1]], g$sdf$trans.mat[[1]])
  res = cbind(sdf$a.grid[[1]],pi1= sdf$pi1[[1]],pi2=sdf$pi2[[1]])


  det = get_rne_details(g, x="5_5")
}

examples.cost.ladder.bertrand = function() {
  # A Bertrand duopoly
  # with endogenous marginal cost
  pi.fun = function(ax.df, i.cost=10,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = -i.cost*i1,
      pi2 = -i.cost*i2
    )
  }
  static.pi.fun = function(ax.df,x.max,a=x.max, b=1,...) {
    restore.point("pi.fun")
    res = mutate(ax.df,
      p = pmin(p1,p2),
      Q = pmax(0,a-b*p),
      q1 = case_when(
        p1 < p2 ~ Q,
        p1 == p2 ~ Q/2,
        p1 > p2 ~ 0
      ),
      q2 = Q-q1,
      pi1 = (p1-x1)*q1,
      pi2 = (p2-x2)*q2
    )
    attr(res,"extra.col") = TRUE
    # dat = res %>% filter(x1==5, x2==5)
    res
  }

  trans.fun = function(ax.df,x.max,dep.prob=0,alpha=1,x.min=0, ...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()
    ax.df = mutate(ax.df,
      i1.prob = alpha*i1 / (1+alpha*i1),
      i2.prob = alpha*i2 / (1+alpha*i2)
    )
    dp = dep.prob
    trans = independent.transitions(ax.df,
      trans_var("nx1",default=x1,lower=0, upper=x.max,
        trans_val(x1-1, (1-dp)*i1.prob),
        trans_val(x1+1, dp*(1-i1.prob))
      ),
      trans_var("nx2",default=x2,lower=0, upper=x.max,
        trans_val(x2-1, (1-dp)*i2.prob),
        trans_val(x2+1, dp*(1-i2.prob))
      )
    )
    trans = mutate(trans,
        xd = paste0(nx1,"_",nx2),
        xs=x
      ) %>%
      select(xs,xd,i1,i2,prob)
    trans
  }



  x.min=0; x.max = 5; a = x.max
  i.seq=c(0,1,2,4,8)
  p.seq=seq(0,a, length=21)
  x.seq = seq(x.max,x.min, by=-1)
  x.df = as_data_frame(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)
  g = rel_game("Bertrand with Cost Ladder") %>%
    rel_param(x.min=x.min,x.max=x.max,dep.prob=0.1,a=x.max,b=1, i.cost=0.1,alpha=1) %>%
    rel_states(x.df,A1=list(i1=i.seq),A2=list(i2=i.seq),static.A1 = list(p1=p.seq), static.A2 = list(p2=p.seq),pi.fun=pi.fun, trans.fun=trans.fun, static.pi.fun = static.pi.fun, x.T = "0_0") %>%
    rel_compile()


  g = rel_mpe(g, delta=0.7)
  mpe = get_mpe(g)
  mpe$r_lab = paste0(
    "u ",round(mpe$u1)," ", round(mpe$u2),
    "\np ",round(mpe$p1)," ", round(mpe$p2),
    "\nq ",round(mpe$q1)," ", round(mpe$q2),
    "\ni ",round(mpe$i1)," ", round(mpe$i2)
  )
  library(ggplot2)
  ggplot(mpe, aes(x=x1,y=x2, fill=stat.prob)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))



  g = g %>%  rel_capped_rne(T=50, delta=0.7, rho=0, save.history = !FALSE, use.cpp = TRUE, add.stationary = TRUE, save.details = !TRUE)
  eq = get_eq(g)


  eq$r_lab = paste0(
    "r ",round(eq$r1,2)," ", round(eq$r2,2),
    "\np ",round(eq$p1)," ", round(eq$p2),
    "\nq ",round(eq$q1)," ", round(eq$q2),
    "\ni ",round(eq$i1)," ", round(eq$i2)
  )
  ggplot(eq, aes(x=x1,y=x2, fill=stationary)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))

  animate_capped_rne_history(g,x = c("5_5","0_0","5_1"))


  eq_diagram(g, just.eq.chain = !TRUE)

  sdf = g$gs$sdf
  cbind(g$sdf$a.grid[[1]],pi1= g$sdf$pi1[[1]],pi2=g$sdf$pi2[[1]], g$sdf$trans.mat[[1]])
  res = cbind(sdf$a.grid[[1]],pi1= sdf$pi1[[1]],pi2=sdf$pi2[[1]])


  det = get_rne_details(g, x="5_5")
}


