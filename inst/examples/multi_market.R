examples.cost.ladder.cournot.multimarket = function() {
  # A Cournot duopoly in 2 markets
  # with endogenous marginal cost
  vec.pi.fun = function(ax.df, i.cost=10,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = -i.cost*i1,
      pi2 = -i.cost*i2
    )
  }
  vec.static.pi.fun = function(ax.df,x.max,a=x.max, b=1,...) {
    restore.point("pi.fun")
    res = mutate(ax.df,
      Q = q1+q2,
      p = a-b*Q,
      x1 = ifelse(market=="A",x1A,x1B),
      x2 = ifelse(market=="A",x2A,x2B),
      pi1 = (p-x1)*q1,
      pi2 = (p-x2)*q2
    )
    attr(res,"extra.col") = TRUE
    # dat = res %>% filter(x1==5, x2==5)
    res
  }

  vec.trans.fun = function(ax.df,x.max,dep.prob=0,alpha=1,x.min=0, ...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()
    ax.df = mutate(ax.df,
      i1.prob = alpha*i1 / (1+alpha*i1),
      i2.prob = alpha*i2 / (1+alpha*i2)
    )
    axA = filter(ax.df, market=="A")
    axB = filter(ax.df, market=="B")
    dp = dep.prob
    transA = independent.transitions(axA,
      trans_var("nx1",default=x1A,lower=0, upper=x.max,
        trans_val(x1A-1, (1-dp)*i1.prob),
        trans_val(x1A, 1-((1-dp)*i1.prob+dp*(1-i1.prob))),
        trans_val(x1A+1, dp*(1-i1.prob))
      ),
      trans_var("nx2",default=x2A,lower=0, upper=x.max,
        trans_val(x2A-1, (1-dp)*i2.prob),
        trans_val(x2A, 1-((1-dp)*i2.prob+dp*(1-i2.prob))),
        trans_val(x2A+1, dp*(1-i2.prob))
      ),
      # Next market is chosen randomly
      trans_var("nm",
        trans_val("A", 0.5),
        trans_val("B",0.5)
      )
    )
    transA = mutate(transA,
        xd = paste0(nm,"_",nx1,"_",nx2,"_",x1B,"_",x2B),
        xs=x
      ) %>%
      select(xs,xd,i1,i2,prob)

    transB = independent.transitions(axB,
      trans_var("nx1",default=x1B,lower=0, upper=x.max,
        trans_val(x1B-1, (1-dp)*i1.prob),
        trans_val(x1B, 1-((1-dp)*i1.prob+dp*(1-i1.prob))),
        trans_val(x1B+1, dp*(1-i1.prob))
      ),
      trans_var("nx2",default=x2B,lower=0, upper=x.max,
        trans_val(x2B-1, (1-dp)*i2.prob),
        trans_val(x2B, 1-((1-dp)*i2.prob+dp*(1-i2.prob))),
        trans_val(x2B+1, dp*(1-i2.prob))
      ),
      # Next market is chosen randomly
      trans_var("nm",
        trans_val("A", 0.5),
        trans_val("B",0.5)
      )
    )
    transB = mutate(transB,
        xd = paste0(nm,"_",x1A,"_",x2A,"_",nx1,"_",nx2),
        xs=x
      ) %>%
      select(xs,xd,i1,i2,prob)

    rbind(transA,transB)

  }



  x.min=0; x.max = 3; a = x.max
  i.seq=c(0,0.5,1,2)
  q.seq=seq(0,a, length=11)
  x.seq = seq(x.max,x.min, by=-1)
  x.df = as_data_frame(expand.grid(
    x1A=x.seq,x2A=x.seq,
    x1B=x.seq,x2B=x.seq,
    market=c("A","B"))
  )
  x.df$x = paste0(x.df$market,"_",x.df$x1A,"_", x.df$x2A,"_",x.df$x1B,"_", x.df$x2B)
  g = rel_game("Multimarket Cournot with Cost Ladder") %>%
    rel_param(x.min=x.min,x.max=x.max,dep.prob=0.05,a=x.max,b=1, i.cost=0.5,alpha=1) %>%
    rel_states(x.df,A1=list(i1=i.seq),A2=list(i2=i.seq),static.A1 = list(q1=q.seq), static.A2 = list(q2=q.seq),vec.pi.fun=vec.pi.fun, vec.trans.fun=vec.trans.fun, vec.static.pi.fun = vec.static.pi.fun, x.T = "A_0_0_0_0") %>%
    rel_compile()


  g = rel_mpe(g, delta=0.9)
  mpe = get.mpe(g)
  plot.top.states(mpe)

  g = g %>%  rel_capped_rne(T=50, delta=0.9, rho=0.5, save.history = FALSE, use.cpp = TRUE, add.stationary = TRUE, save.details = FALSE)

  reps = 20
  eq.li = vector("list",reps)

  eq = get.eq(g)
  eq.li[[1]] = eq
  #plot.top.states(eq)

  g = rel_rne_from_capped(g)
  eq = get.eq(g)
  eq.li[[2]] = eq

  diff = compare.eq(eq.li[[1]], eq.li[[2]],g)

  for (rep in 3:reps) {
    eq =get.eq(g)
    eq$rep = rep
    eq.li[[rep]] = eq
    g = rel_is_eq_rne(g)
  }
  animate.eq.li(g, eq.li,x="B_1_0_0_1")



  sim = simulate.eq(g,1000,x0 = "A_2_2_2_2")
  sort(table(sim$x),decreasing = TRUE)


  top10 = eq %>% filter(market=="A") %>% top_n(6, stationary.prob)



  eqA = eq %>% group_by(x1A,x2A) %>%
    summarize(stationary.prob = sum(stationary.prob),market="A") %>%
    ungroup() %>% rename(x1=x1A,x2=x2A)
  eqB = eq %>% group_by(x1B,x2B) %>%
    summarize(stationary.prob = sum(stationary.prob),market="B") %>%
    ungroup() %>% rename(x1=x1B,x2=x2B)

  eqAB = rbind(eqA,eqB)
  ggplot(eqAB, aes(x=x1,y=x2, fill=stationary.prob)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=round(stationary.prob,2)), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines")) + facet_wrap(~market)



  rho.seq = c(1,0.9,0.5,0)
  sim = bind_rows(lapply(rho.seq, function(rho) {
    cat("\nrho = ",rho)
    g = g %>%  rel_capped_rne(T=50, delta=0.9, rho=rho, save.history = FALSE, use.cpp = TRUE, add.stationary = TRUE)
    eq = get.eq(g)
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

  animate.capped.rne.history(g,x = c("5_5","0_0","5_1"))


  rne.diagram(g, just.eq.chain = !TRUE)

  sdf = g$gs$sdf
  cbind(g$sdf$a.grid[[1]],pi1= g$sdf$pi1[[1]],pi2=g$sdf$pi2[[1]], g$sdf$trans.mat[[1]])
  res = cbind(sdf$a.grid[[1]],pi1= sdf$pi1[[1]],pi2=sdf$pi2[[1]])


  det = get.rne.details(g, x="5_5")
}

plot.top.states = function(eq,ntop=4) {
  library(ggplot2)
  tp = make.top.prob(eq,ntop)
  ggplot(tp, aes(x=x1,y=x2, fill=stat.prob)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=round(stat.prob,2)), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines")) + facet_grid(top~market)

}

make.top.prob = function(eq, ntop=4) {
  dat = eq %>% group_by(x1A,x2A,x1B,x2B) %>%
    mutate(stat.prob = sum(stationary.prob)) %>%
    ungroup() %>%
    filter(market=="A")

  top.x = arrange(top_n(dat, ntop, stat.prob), desc(stat.prob))$x
  top.x = top.x[1:ntop]

  res = bind_rows(lapply(seq_along(top.x), function(top) {
    x = top.x[top]
    d = dat
    d$stat.prob[d$x != x] = 0
    d$top = top

    dA = d %>% group_by(x1A,x2A) %>% summarize(stat.prob=max(stat.prob)) %>%
      ungroup() %>% mutate(market="A") %>% rename(x1=x1A,x2=x2A)
    dB = d %>% group_by(x1B,x2B) %>% summarize(stat.prob=max(stat.prob)) %>%
      ungroup() %>% mutate(market="B") %>% rename(x1=x1B,x2=x2B)
    rbind(dA,dB) %>% mutate(top=top)
  }))
  res

}
