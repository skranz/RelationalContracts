pd.markov = function() {
  pi1 = matrix(c(1,2,-1,0),2,2)
  pi2 = t(pi1)
  g = rel_game("Repated PD") %>%
    # Initial State
    rel_state("x", A1=c("C","D"),A2=c("C","D"),pi1=pi1, pi2=pi2) %>%
    rel_after_cap_payoffs(U=3,v1=0, v2=0) %>%
    rel_compile() %>%
    rel_capped_rne(T=20, delta=0.9,rho=1, save.history = TRUE)




  g = rel_game("PD with C and D state") %>%
    # Initial State
    rel_state(c("xC"), A1=c("C","D"),A2=c("C","D"),pi1=pi1, pi2=pi2,x.T="xTC") %>%
    rel_state(c("xD"), A1=c("C","D"),A2=c("C","D"),pi1=pi1, pi2=pi2,x.T="xTD") %>%
    rel_transition("xC","xD",a1="D") %>%
    rel_transition("xC","xD",a2="D") %>%
    rel_transition("xD","xC",a1="C",a2="C") %>%
    rel_after_cap_payoffs("xTC",U=5,v1.trunc=0, v2.trunc=0) %>%
    rel_after_cap_payoffs("xTD",U=0,v1.trunc=0, v2.trunc=0) %>%
    rel_compile() %>%
    rel_capped_rne(T=20, delta=0.9,rho=1, save.history = TRUE)

  (rne = g$rne)
  capped.rne.history.animation(g)

  g = rel_game("Cournot") %>%
    # Initial State
    rel_state(c("xC"), A1=c("C","D"),A2=c("C","D"),pi1=pi1, pi2=pi2,x.T="xTC") %>%
    rel_state(c("xD"), A1=c("C","D"),A2=c("C","D"),pi1=pi1, pi2=pi2,x.T="xTD") %>%
    rel_transition("xC","xD",a1="D") %>%
    rel_transition("xC","xD",a2="D") %>%
    rel_transition("xD","xC",a1="C",a2="C") %>%
    rel_after_cap_payoffs("xTC",U=5,v1.trunc=0, v2.trunc=0) %>%
    rel_after_cap_payoffs("xTD",U=0,v1.trunc=0, v2.trunc=0) %>%
    rel_compile() %>%
    rel_capped_rne(T=20, delta=0.9,rho=1, save.history = TRUE)



  sdf = g$sdf

  sdf$a.grid
  sdf$pi2
  sdf$trans.mat
}

counot.example = function() {
  library(RelationalContracts)

  # Quantity choice
  static.A.fun = function(k1,k2, q.step,...) {
    restore.point("A.fun")
    list(
      A1=list(q1=seq(0,k1,by=q.step)),
      A2=list(q2=seq(0,k2,by=q.step))
    )
  }

  # Capacity investments
  A.fun = function(k1,k2,stage,k.max,...) {
    restore.point("A.fun")
    A1 = list(i1=c("",if (k1<k.max) "i"))
    A2 = list(i2=c("",if (k2<k.max) "i"))
    list(A1=A1,A2=A2)
  }

  vec.static.pi.fun = function(ax.df,c1,c2,...) {
    restore.point("vec.static.pi.fun")
    ax.df %>%
      transmute(
        x=x,
        P = pmax(0,100-q1-q2),
        pi1= (P-c1)*q1,
        pi2= (P-c2)*q2
      )
  }

  vec.trans.fun = function(ax.df, k.step,...) {
    restore.point("vec.trans.fun")
    ax.df %>%
      mutate(
        k1d = k1+(i1=="i")*k.step,
        k2d = k2+(i2=="i")*k.step,
      ) %>%
      transmute(xs=x,xd=paste0(k1d, " ",k2d),i1=i1,i2=i2, prob=1)
  }


  q.step = 1
  k.step = 20
  k.max = 60
  k.seq = seq(0,k.max, by=k.step)
  x.df = expand_grid(k1=k.seq,k2=k.seq) %>%
    mutate(x= paste0(k1," ", k2))
  x.T = x.df$x[NROW(x.df)]


  g = rel_game("Cournot with Investments") %>%
    rel_param(k.max=k.max,k.step=k.step, q.step=q.step,c1=0,c2=0,i_cost=0) %>%
    rel_states(x.df,
      # Static effort stage
      static.A.fun=static.A.fun,
      vec.static.pi.fun = vec.static.pi.fun,
      # Dynamic relationship intensification stage
      A.fun = A.fun,
      pi1 =~ (i1=="i")*i_cost, pi2=~ (i2=="i")*i_cost,
      vec.trans.fun=vec.trans.fun
    )

  g = rel_compile(g)
  options(warn=2)
  g = rel_capped_rne(g,T=50, adjusted.delta=0.5, rho=0.2,save.history = !TRUE)
  rne.diagram(g)

  hist = g$rne.history
  (rne=g$rne)
  animate.capped.rne.history(g,x = c("60 0", "60 60", "20 20","0 0"), add.diag = FALSE)



}

