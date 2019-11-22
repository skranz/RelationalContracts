examples.multimarket.cost.ladder.cournot = function() {
  pbounded = function(min,x,max) {
    pmax(min,pmin(x,max))
  }

  # Action space for each state
  A.fun = function(ap,...) {
    restore.point("A.fun")
    list(
      A1 = list(i1=c(0,if (ap==1) 1)),
      A2 = list(i2=c(0,if (ap==2) 1))
    )
  }

  # A cournot duopoly
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
      x1 = ifelse(market=="A",x1A,x1B),
      x2 = ifelse(market=="A",x2A,x2B),
      pi1 = (p-x1)*q1,
      pi2 = (p-x2)*q2
    )
    attr(res,"extra.col") = TRUE
    # dat = res %>% filter(x1==5, x2==5)
    res
  }

  trans.fun = function(ax.df,x.max,dep.prob=0,i.prob=1,alpha=1,x.min=0, ...) {
    restore.point("trans.fun")
    tr = ax.df %>% irv_joint_dist(
      irv("new_ap",irv_val(1, 0.5), irv_val(2, 0.5)),
      irv("new_market",irv_val("A", 0.5), irv_val("B", 0.5)),
      irv("suc_1",default=0,
        irv_val(1, i1*i.prob)
      ),
      irv("suc_2",default=0,
        irv_val(1, i2*i.prob)
      ),
      irv("dep_1",default=0,
        irv_val(1, dep.prob)
      ),
      irv("dep_2",default=0,
        irv_val(1, dep.prob)
      )
    ) %>%
    filter(prob > 0)


    tr = tr %>%
      mutate(
        nx1A = ifelse(market=="A",pbounded(0,x1A-suc_1+dep_1,x.max),x1A),
        nx2A = ifelse(market=="A",pbounded(0,x2A-suc_2+dep_2,x.max),x2A),
        nx1B = ifelse(market=="A",pbounded(0,x1A-suc_1+dep_1,x.max),x1A),
        nx2B = ifelse(market=="A",pbounded(0,x2B-suc_2+dep_2,x.max),x2B),
        xd = paste0(new_ap,new_market," ",nx1A,"_", nx2A, " ",nx1B,"_", nx2B),
        xs=x
      ) %>%
      group_by(xs,xd, i1,i2) %>%
      summarize(prob=sum(prob))

  }


  x.min=0; x.max = 2; a = x.max
  q.seq=seq(0,a, length=21)
  x.seq = seq(x.max,x.min, by=-1)
  library(tidyr)
  x.df = expand_grid(ap=c(1:2),market=c("A","B"),x1A=x.seq,x2A=x.seq,x1B=x.seq,x2B=x.seq) %>%
    mutate(
      x = paste0(ap,market," ",x1A,"_", x2A, " ",x1B,"_", x2B),
      xgroup = paste0(market," ",x1A,"_", x2A, " ",x1B,"_", x2B)
    )

  g = rel_game("Multimarket Cournot with Cost Ladder") %>%
    rel_param(x.min=x.min,x.max=x.max,dep.prob=0,a=x.max,b=1, i.cost=5,i.prob = 1) %>%
    rel_states(x.df, A.fun = A.fun,static.A1 = list(q1=q.seq), static.A2 = list(q2=q.seq),pi.fun=pi.fun, trans.fun=trans.fun, static.pi.fun = static.pi.fun)

  g = g %>%  rel_compile()

  g = g %>%  rel_T_rne(T=1000, delta=0.9, rho=1, save.details = !TRUE) %>%
    rel_state_probs(x0="equal")

  eq = get_eq(g)

  geq = eq_combine_xgroup(g) %>%
    left_join(select(x.df, xgroup,market, x1A,x2A,x1B,x2B) %>% unique(), by="xgroup")

  plot.top.states(geq)


  eqA = geq %>% group_by(x1A,x2A) %>%
    summarize(prob = sum(state.prob),market="A") %>%
    ungroup() %>% rename(x1=x1A,x2=x2A)
  eqB = geq %>% group_by(x1B,x2B) %>%
    summarize(prob = sum(state.prob),market="B") %>%
    ungroup() %>% rename(x1=x1B,x2=x2B)

  eqAB = rbind(eqA,eqB)
  ggplot(eqAB, aes(x=x1,y=x2, fill=prob)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=round(prob,2)), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines")) + facet_wrap(~market)


}


examples.cost.ladder.cournot.multimarket = function() {

  # A Cournot duopoly in 2 markets
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
      x1 = ifelse(market=="A",x1A,x1B),
      x2 = ifelse(market=="A",x2A,x2B),
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



  x.min=0; x.max = 2; a = x.max
  i.seq=c(0,0.5,1,2,4)
  #i.seq=c(0,0.5,1)
  q.seq=seq(0,a, length=41)
  x.seq = seq(x.max,x.min, by=-1)
  x.df = as_data_frame(expand.grid(
    x1A=x.seq,x2A=x.seq,
    x1B=x.seq,x2B=x.seq,
    market=c("A","B"))
  )
  x.df$x = paste0(x.df$market,"_",x.df$x1A,"_", x.df$x2A,"_",x.df$x1B,"_", x.df$x2B)
  g = rel_game("Multimarket Cournot with Cost Ladder") %>%
    rel_param(x.min=x.min,x.max=x.max,dep.prob=0.1,a=x.max,b=1, i.cost=1,alpha=1) %>%
    rel_states(x.df,A1=list(i1=i.seq),A2=list(i2=i.seq),static.A1 = list(q1=q.seq), static.A2 = list(q2=q.seq),pi.fun=pi.fun, trans.fun=trans.fun, static.pi.fun = static.pi.fun, x.T = "A_0_0_0_0") %>%
    rel_compile()

  g = g %>%  rel_capped_rne(T=100, delta=0.9, rho=0.5, save.history = FALSE, use.cpp = TRUE, add.stationary = TRUE, save.details = FALSE)

  g = rel_rne_from_eq_actions(g, iterations=20, save.eq.li=TRUE)
  find.eq.li.action.repetitions(g$eq.li)


  g = rel_mpe(g, delta=0.9)
  mpe = get_mpe(g)
  plot.top.states(mpe)


  eq = get_eq(g)
  eq.li[[2]] = eq

  diff = compare_eq(eq.li[[1]], eq.li[[2]],g)

  for (rep in 3:reps) {
    eq =get_eq(g)
    eq$rep = rep
    eq.li[[rep]] = eq
    g = rel_is_eq_rne(g)
  }
  animate_eq_li(g, eq.li,x="B_1_0_0_1")



  sim = simulate.eq(g,1000,x0 = "A_2_2_2_2")
  sort(table(sim$x),decreasing = TRUE)


  top10 = eq %>% filter(market=="A") %>% top_n(6, stationary.prob)

}

plot.top.states = function(eq,ntop=4) {
  library(ggplot2)
  tp = make.top.prob(eq,ntop)
  ggplot(tp, aes(x=x1,y=x2, fill=prob)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=round(prob,2)), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines")) + facet_grid(top~market)

}

make.top.prob = function(geq, ntop=4) {
  dat = geq %>% group_by(x1A,x2A,x1B,x2B) %>%
    mutate(prob = sum(state.prob), x=xgroup) %>%
    ungroup() %>%
    filter(market=="A")

  top.x = arrange(top_n(dat, ntop, prob), desc(prob))$x
  top.x = top.x[1:ntop]

  res = bind_rows(lapply(seq_along(top.x), function(top) {
    x = top.x[top]
    d = dat
    d$stat.prob[d$x != x] = 0
    d$top = top

    dA = d %>% group_by(x1A,x2A) %>% summarize(prob=max(prob)) %>%
      ungroup() %>% mutate(market="A") %>% rename(x1=x1A,x2=x2A)
    dB = d %>% group_by(x1B,x2B) %>% summarize(prob=max(prob)) %>%
      ungroup() %>% mutate(market="B") %>% rename(x1=x1B,x2=x2B)
    rbind(dA,dB) %>% mutate(top=top)
  }))
  res

}
