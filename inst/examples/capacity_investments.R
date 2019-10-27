examples.capacity.bertrand = function() {
  # A Bertrand duopoly
  # with endogenous soft capacity constraints
  #
  # Model from the Appendix of
  #
  # David Besanko and Ulrich Doraszelski (2004),
  # Capacity Dynamics and Endogenous Asymmetries in Firm Size,
  # RAND Journal of Economics, Vol. 35, No. 1, pp. 23-49.
  #
  # Linear demand functions
  #
  # Q = a - b*p
  #
  # The cheaper firm gets all demand. At the same price, demand is split.
  #
  # Soft capacity constraints: Cost functions
  #
  # c_i = (1/(1+eta)) * (q_1 / x_1)^eta *q
  #
  # Specifications from Besanko & Doraszelski
  #
  # a = 40
  # b = 10
  # eta = 10
  # x in seq(5,45,by=5)

  A.fun = function(i.seq=c(0,1),...) {
    restore.point("A.fun")
    list(
      A1=list(i1=i.seq),
      A2=list(i2=i.seq)
    )
  }
  static.A.fun = function(x1,x2,p.seq,...) {
    restore.point("static.A.fun")
    list(
      A1=list(p1=p.seq),
      A2=list(p2=p.seq)
    )
  }

  pi.fun = function(ax.df, i.cost=10*x.step, x.step=1,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = -i.cost*i1,
      pi2 = -i.cost*i2
    )
  }


  static.pi.fun = function(ax.df, a=40,b=10,gamma=0,eta=10,max.mc=2*a,...) {
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
      C1 = pmin(max.mc*q1,(1/(1+eta)) * (q1 / x1)^eta *q1),
      C2 = pmin(max.mc*q2,(1/(1+eta)) * (q2 / x2)^eta *q2),
      mc1 = C1-pmin(max.mc*(q1-1),(1/(1+eta)) * ((q1-1) / x1)^eta *(q1-1)),
      mc2 = C2-pmin(max.mc*(q2-1),(1/(1+eta)) * ((q2-1) / x1)^eta *(q2-1)),
      pi1 = p1*q1-C1,
      pi2 = p2*q2-C2
    )
    attr(res,"extra.col") = TRUE
    # dat = res %>% filter(x1==5, x2==5)
    res
  }

  trans.fun = function(ax.df,x.step,x.max,dep.prob=0,alpha=1,x.min=0,full.dep=FALSE, ...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()
    ax.df = mutate(ax.df,
      i1.prob = alpha*i1 / (1+alpha*i1),
      i2.prob = alpha*i2 / (1+alpha*i2)
    )
    dp = dep.prob

    if (full.dep) {
      # We assume the possibility of full depreciation
      trans = independent.transitions(ax.df,
        trans_var("nx1",default=x1,lower=0, upper=x.max,
          trans_val(x1+x.step, (1-dp)*i1.prob),
          trans_val(x1, (1-dp)*(1-i1.prob)),
          trans_val(x.min+x.step, dp*i1.prob),
          trans_val(x.min, dp*(1-i1.prob))
        ),
        trans_var("nx2",default=x2,lower=0, upper=x.max,
          trans_val(x2+x.step, (1-dp)*i2.prob),
          trans_val(x2, (1-dp)*(1-i2.prob)),
          trans_val(x.min+x.step, dp*i2.prob),
          trans_val(x.min, dp*(1-i2.prob))
        )
      )
    # Only one unit depreciates
    } else {
      # We assume the possibility of full depreciation
      trans = independent.transitions(ax.df,
        trans_var("nx1",default=x1,lower=x.min, upper=x.max,
          trans_val(x1+x.step, (1-dp)*i1.prob),
          trans_val(x1-x.step, dp*(1-i1.prob))
        ),
        trans_var("nx2",default=x2,lower=x.min, upper=x.max,
          trans_val(x2+x.step, (1-dp)*i2.prob),
          trans_val(x2-x.step, dp*(1-i2.prob))
        )
      )

    }
    trans = mutate(trans,
        xd = paste0(nx1,"_",nx2),
        xs=x
      ) %>%
      select(xs,xd,i1,i2,prob)
    trans
  }

  # Small game to test software
  x.min=5; x.max = 45; x.step = 5
  x.seq = seq(x.min,x.max, by=x.step)
  x.df = as_data_frame(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)
  g = rel_game("Bertrand with Investment") %>%
    rel_param(x.min=x.min,x.max=x.max,x.step=x.step,dep.prob=0.2,a=50,b=1, i.cost=10, i.seq=c(0,1), alpha=1, eta=10, p.seq=c(seq(10,40,length=31),1000)) %>%
    rel_states(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun, static.pi.fun = static.pi.fun, static.A.fun = static.A.fun) %>%
    rel_compile()
  g = g %>%  rel_capped_rne(T=400, delta=0.9, rho=0.8, save.history = !FALSE, use.cpp = TRUE, add.stationary = TRUE, save.details = !TRUE,tie.breaking = "max_r1")
  (capped = get_eq(g))
  find.eq.li.action.repetitions(g$eq.history)

  g = rel_rne_from_eq_actions(g,iterations = 20, save.eq.li = TRUE)
  find.eq.li.action.repetitions(g$eq.li)





  x.min=5; x.max = 45; x.step = 5
  x.seq = seq(x.min,x.max, by=x.step)
  x.df = as_data_frame(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)
  g = rel_game("Bertrand with Investment") %>%
    rel_param(x.min=x.min,x.max=x.max,x.step=x.step,dep.prob=0.2,a=50,b=1, i.cost=10, i.seq=c(0,1), alpha=1, eta=10, p.seq=c(seq(0,40,length=41),1000)) %>%
    rel_states(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun, static.pi.fun = static.pi.fun, static.A.fun = static.A.fun) %>%
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
  ggplot(mpe, aes(x=x1,y=x2, fill=stationary.prob)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))



  g = g %>%  rel_capped_rne(T=50, delta=0.9, rho=0.5, save.history = !FALSE, use.cpp = TRUE, add.stationary = TRUE, save.details = !TRUE,tie.breaking = "max_r1")
  eq = get_eq(g)
  g = rel_rne_from_capped(g)

  eq$r_lab = paste0(round(eq$r1)," ", round(eq$r2),"\n", eq$ae.lab)
  ggplot(eq, aes(x=x1,y=x2, fill=stationary)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))

  animate_capped_rne_history(g,x = c("5_5","45_45","25_25","45_5"))


  eq_diagram(g, just.eq.chain = !TRUE)

  sdf = g$gs$sdf
  cbind(g$sdf$a.grid[[1]],pi1= g$sdf$pi1[[1]],pi2=g$sdf$pi2[[1]], g$sdf$trans.mat[[1]])
  res = cbind(sdf$a.grid[[1]],pi1= sdf$pi1[[1]],pi2=sdf$pi2[[1]])


  det = get_rne_details(g, x="5_5")
}



examples.capacity.differentiated.bertrand = function() {
  # A differentiated product Bertrand game
  # with endogenous soft capacity constraints
  #
  # Model from the Appendix of
  #
  # David Besanko and Ulrich Doraszelski (2004),
  # Capacity Dynamics and Endogenous Asymmetries in Firm Size,
  # RAND Journal of Economics, Vol. 35, No. 1, pp. 23-49.
  #
  # Differentiated linear demand functions
  #
  # q_1 = (1/(1-gamma^2)) * (a*(1-gamma) - b*p_1 + gamma*b*p_2)
  # q_2 = (1/(1-gamma^2)) * (a*(1-gamma) - b*p_2 + gamma*b*p_1)
  #
  # Soft capacity constraints: Cost functions
  #
  # c_i = (1/(1+eta)) * (q_1 / x_1)^eta *q
  #
  # Specifications from Besanka & Doraszelski
  #
  # gamma in (0, 0.7, 0.9)
  # a = 40
  # b = 10
  #
  # eta = 10
  # x in seq(5,45,by=5)

  A.fun = function(i.seq=c(0,1),...) {
    restore.point("A.fun")
    list(
      A1=list(i1=i.seq),
      A2=list(i2=i.seq)
    )
  }
  static.A.fun = function(x1,x2,p.seq,...) {
    restore.point("static.A.fun")
    list(
      A1=list(p1=p.seq),
      A2=list(p2=p.seq)
    )
  }
  extra.info = function(ax.df, a=40,b=10,gamma=0,eta=10,max.C = 1e7) {

  }

  pi.fun = function(ax.df, i.cost=10*x.step, x.step=1,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = -i.cost*i1,
      pi2 = -i.cost*i2
    )
  }


  static.pi.fun = function(ax.df, a=40,b=10,gamma=0,eta=10,max.C = 1e7,...) {
    restore.point("pi.fun")
    res = mutate(ax.df,
      q1 = pmax(0,(1/(1-gamma^2)) * (a*(1-gamma) - b*p1 + gamma*b*p2)),
      q2 = pmax(0,(1/(1-gamma^2)) * (a*(1-gamma) - b*p2 + gamma*b*p1)),
      C1 = pmin(max.C,(1/(1+eta)) * (q1 / x1)^eta *q1),
      C2 = pmin(max.C,(1/(1+eta)) * (q2 / x2)^eta *q2),
      pi1 = p1*q1-C1,
      pi2 = p2*q2-C2
    )
    attr(res,"extra.col") = TRUE
    # dat = res %>% filter(x1==5, x2==5)
    res
  }

  trans.fun = function(ax.df,x.step,x.max,dep.prob=0,alpha=1,x.min=0,...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()
    ax.df = mutate(ax.df,
      i1.prob = alpha*i1 / (1+alpha*i1),
      i2.prob = alpha*i2 / (1+alpha*i2)
    )
    dp = dep.prob
    # We assume the possibility of full depreciation
    trans = independent.transitions(ax.df,
      trans_var("nx1",default=x1,lower=0, upper=x.max,
        trans_val(x1+x.step, (1-dp)*i1.prob),
        trans_val(x1, (1-dp)*(1-i1.prob)),
        trans_val(x.min+x.step, dp*i1.prob),
        trans_val(x.min, dp*(1-i1.prob))
      ),
      trans_var("nx2",default=x2,lower=0, upper=x.max,
        trans_val(x2+x.step, (1-dp)*i2.prob),
        trans_val(x2, (1-dp)*(1-i2.prob)),
        trans_val(x.min+x.step, dp*i2.prob),
        trans_val(x.min, dp*(1-i2.prob))
      )
    )
    trans = mutate(trans,
        xd = paste0(nx1,"_",nx2),
        xs=x
      ) %>%
      select(xs,xd,i1,i2,prob)
    trans
  }


  x.min=5; x.max = 45; x.step = 10
  x.seq = seq(x.min,x.max, by=x.step)
  x.df = as_data_frame(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)
  g = rel_game("Bertrand with Investment") %>%
    rel_param(x.min=x.min,x.max=x.max,x.step=x.step,dep.prob=0.05,a=40,b=1, i.cost=40, i.seq=c(0,1), alpha=1, eta=10, gamma=0.9, p.seq=seq(18,40,length=21)) %>%
    rel_states(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun, static.pi.fun = static.pi.fun, static.A.fun = static.A.fun) %>%
    rel_compile()

  g = g %>%  rel_capped_rne(T=20, delta=0.9, rho=0.9, save.history = FALSE, use.cpp = FALSE, add.stationary = TRUE, save.details = TRUE)
  eq = get_eq(g)

  sdf = g$gs$sdf
  cbind(g$sdf$a.grid[[1]],pi1= g$sdf$pi1[[1]],pi2=g$sdf$pi2[[1]], g$sdf$trans.mat[[1]])
  res = cbind(sdf$a.grid[[1]],pi1= sdf$pi1[[1]],pi2=sdf$pi2[[1]])

  eq$r_lab = paste0(round(eq$r1)," ", round(eq$r2),"\n", eq$ae.lab)
  ggplot(eq, aes(x=x1,y=x2, fill=stationary)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))
  eq_diagram(g, just.eq.chain = !TRUE)

  det = get_rne_details(g, x="5_5")
}

examples.capacity.cournot = function() {
  # A Cournot Game with Capacity Building

  A.fun = function(i.seq=c(0,1),...) {
    restore.point("A.fun")
    list(
      A1=list(i1=i.seq),
      A2=list(i2=i.seq)
    )
  }
  static.A.fun = function(x1,x2,q.step=1,...) {
    restore.point("A.fun")
    list(
      A1=list(q1=seq(0,x1,by=q.step)),
      A2=list(q2=seq(0,x2,by=q.step))
    )
  }


  pi.fun = function(ax.df, i.cost=10*x.step, x.step=1,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = -i.cost*i1,
      pi2 = -i.cost*i2
    )
  }
  static.pi.fun = function(ax.df, c1=0, c2=0,a=10,b=1,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = (a-b*(q1+q2)-c1)*q1,
      pi2 = (a-b*(q1+q2)-c2)*q2
    )
  }

  trans.fun = function(ax.df,x.step,x.max,dep.prob=0,...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()
    ax.df = mutate(ax.df,
      i1.prob = i1 / (1+i1),
      i2.prob = i2 / (1+i2)
    )
    dp = dep.prob
    trans = independent_transitions(ax.df,
      trans_var("nx1",default=x1,lower=0, upper=x.max,
        trans_val(x1+x.step, (1-dp)*i1.prob),
        trans_val(x1, (1-dp)*(1-i1.prob)),
        trans_val(x.step, dp*i1.prob),
        trans_val(0, dp*(1-i1.prob))
      ),
      trans_var("nx2",default=x2,lower=0, upper=x.max,
        trans_val(x2+x.step, (1-dp)*i2.prob),
        trans_val(x2, (1-dp)*(1-i2.prob)),
        trans_val(x.step, dp*i2.prob),
        trans_val(0, dp*(1-i2.prob))
      )
    )
    trans = mutate(trans,
        xd = paste0(nx1,"_",nx2),
        xs=x
      ) %>%
      select(xs,xd,i1,i2,prob)
    trans
  }


  x.max = 100; x.step = 20
  x.seq = seq(0,x.max, by=x.step)
  x.df = as_tibble(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)

  g = rel_game("Cournot with Investment") %>%
    rel_param(c1=0,c2=0,x.step=x.step, x.max=x.max,dep.prob=0.05,a=100, i.cost=50, i.seq=c(0,1,2,5)) %>%
    rel_states(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun, static.pi.fun = static.pi.fun, static.A.fun = static.A.fun) %>%
    rel_compile()

  g = g %>%  rel_capped_rne(T=1000, delta=0.9, rho=1, add.stationary = TRUE)


  eq = g$eq
  eq$r_lab = paste0(round(eq$r1)," ", round(eq$r2),"\n", eq$ae.lab)
  library(ggplot2)
  ggplot(eq, aes(x=x1,y=x2, fill=stationary.prob)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))
  eq_diagram(g, just.eq.chain = TRUE)

  det = get_rne_details(g, x="100_0")
}

examples.capacity.cournot.det = function() {
  # A Cournot Game with Capacity Building

  A.fun = function(...) {
    restore.point("A.fun")
    list(
      A1=list(i1=c(0,1)),
      A2=list(i2=c(0,1))
    )
  }
  static.A.fun = function(x1,x2,q.step=1,...) {
    restore.point("static.A.fun")
    list(
      A1=list(q1=seq(0,x1,by=q.step)),
      A2=list(q2=seq(0,x2,by=q.step))
    )
  }


  pi.fun = function(ax.df, i.cost=10*x.step, x.step=1,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = -i.cost*i1,
      pi2 = -i.cost*i2
    )
  }
  static.pi.fun = function(ax.df, c1=0, c2=0,a=10,b=1,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = (a-b*(q1+q2)-c1)*q1,
      pi2 = (a-b*(q1+q2)-c2)*q2
    )
  }

  trans.fun = function(ax.df,x.step,x.max,dep.prob=0,...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()


    trans = ax.df %>%
      mutate(
        nx1 = pmin(x1+i1*x.step,x.max),
        nx2 = pmin(x2+i2*x.step,x.max),
        xd = paste0(nx1,"_",nx2),
        xs=x,
        prob = 1
      ) %>%
      select(xs,xd,i1,i2,prob) %>%
      unique()
    trans
  }

  x.max = 100; x.step = 20
  x.seq = seq(0,x.max, by=x.step)
  x.df = as_tibble(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)

  g = rel_game("Cournot with Investment") %>%
    rel_param(c1=0,c2=0,x.step=x.step, x.max=x.max,dep.prob=0.05,a=100, i.cost=50, i.seq=c(0,1,2,5)) %>%
    rel_states(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun, static.pi.fun = static.pi.fun, static.A.fun = static.A.fun) %>%
    rel_compile()

  g = g %>%  rel_capped_rne(T=1000, delta=0.9, rho=1, add.stationary = TRUE)


  eq = g$eq
  eq$r_lab = paste0(round(eq$r1)," ", round(eq$r2),"\n", eq$ae.lab)
  library(ggplot2)
  ggplot(eq, aes(x=x1,y=x2, fill=stationary.prob)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))
  eq_diagram(g, just.eq.chain = TRUE)

  det = get_rne_details(g, x="100_0")
}
