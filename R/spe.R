examples.rel.spe = function() {

  e = e.seq = seq(0,1, by=0.01); xL=0; xH=1;
  g = rel_game("Vulnerability Paradox") %>%
    rel_param(delta=0.2, rho=0, c=0.5, xL=xL,xH=xH) %>%
    # Initial State
    rel_state("xL", A1=list(move=c("stay","vul")),A2=list(e=e.seq)) %>%
    rel_payoff("xL",pi1=~e, pi2=~ -c*e*e*(e>=0)) %>%
    #rel_transition("xL","xH",move="vul") %>%
    # High vulnerability
    rel_state("xH", A1=NULL,A2=list(e=unique(c(-xH,e.seq)))) %>%
    rel_payoff("xH",pi1=~e, pi2=~ -c*e*e*(e>=0)) %>%
    rel_compile()
  g = rel_spe(g)
  (spe = g$spe)

  plot.spe.payoff.set(g,x=c("xH","xL"), alpha=0.8)

  plot.spe.payoff.set(g,x=c("xL"), alpha=0.8)

  g = rel_game("Two-State PD") %>%
    rel_param(delta=0.99, rho=0) %>%
    # Initial State
    rel_state("x1", A1=c("C1","D1"),A2=c("D2","C2"),
      pi1=~0.8*((a1=="C1")+(a2=="C2"))-(a1=="C1"),
      pi2=~0.8*((a1=="C1")+(a2=="C2"))-(a2=="C2")
    ) %>%
    rel_transition("x1","x2",a1="C1", a2="C2") %>%
    # High vulnerability
    rel_state("x2", A1=c("C1","D1"),A2=c("D2","C2"),
      pi1=~0.6*((a1=="C1")+(a2=="C2"))-(a1=="C1"),
      pi2=~0.6*((a1=="C1")+(a2=="C2"))-(a2=="C2")
    ) %>%
    rel_compile()

  g = rel_spe(g)
  (spe = g$spe)

  plot.spe.payoff.set(g,x=c("x1","x2"))

  g = rel_rne(g)
  (rne = g$rne)


  g = rel_capped_spe(g,T=10)
  (spe = g$spe)

  # solve game with transfers
}


#' Solve for the set of SPE payoffs in every state and for optimal simple equilibria
#'
#' This function is just an interface to the dyngame package
rel_spe = function(g, delta=g$param$delta,new.dyngame=FALSE, verbose=FALSE, plots=FALSE) {
  restore.point("rel_spe")
  g$param$delta = delta
  if (!g$is_compiled) g = rel_compile(g)

  if (is.null(g[["dyngame"]]) | new.dyngame)
    g$dyngame = make.rel.dyngame(g)

  g$dyngame.sol = solve.game(g$dyngame,delta=delta, plots = plots, verbose = verbose)
  g$spe = dyngame.sol.to.rel.sol(g)
  cat("\n")
  g
}

#' Solve for the set of SPE payoffs in every state and for optimal simple equilibria
#'
#' This function is just an interface to the dyngame package
rel_capped_spe = function(g,T, delta=g$param$delta,...) {
  restore.point("rel_capped_spe")
  g$param$delta = delta
  old_rho = g$param$rho

  g = rel_capped_rne(g,T=T, delta=delta,rho = 0, res.field="spe")
  g$rho = old_rho
  g
}


dyngame.sol.to.rel.sol = function(g,sol=g$dyngame.sol, m=g$dyngame) {
  restore.point("dyngame.sol.to.rel.sol")

  res = as_data_frame(sol$sol.mat)

  ax.to.rel.a = function(m,ax) {
    xa = v.ind.to.rowcol(m$ind.ax.by.x, ax)[,2]
  }

  res$ae = ax.to.rel.a(m,res$ae)
  res$a1 = ax.to.rel.a(m,res$a1)
  res$a2 = ax.to.rel.a(m,res$a2)
  res$x = g$sdf$x[res$x]
  res$r1 = res$v1 + g$param$beta1*(res$U-res$v1-res$v2)
  res$r2 = res$U - res$r1

  res = res[, c("x","r1","r2","U","v1","v2","ae","a1","a2")]

  for (row in seq_len(NROW(res))) {
    res$ae.lab = left_join(select(res,x,a=ae), g$a.labs.df, by=c("x","a"))$lab
    res$a1.lab = left_join(select(res,x,a=a1), g$a.labs.df, by=c("x","a"))$lab
    res$a2.lab = left_join(select(res,x,a=a2), g$a.labs.df, by=c("x","a"))$lab
  }
  res

}



make.rel.dyngame = function(g, symmetric=FALSE) {
  restore.point("make.rel.dyngame")
  library(dyngame)

  sdf = g$sdf

  g$pi.df = bind_rows(lapply(1:NROW(sdf), function(row) {
    cbind(
      quick_df(xv=row, a=1:NROW(sdf$pi1[[row]])),
      expand.grid(av2=1:sdf$na2[[row]],av1=1:sdf$na1[[row]]),
      pi1=sdf$pi1[[row]], pi2=sdf$pi2[[row]]
    )
  }))

  avm.to.a = function() {}

  # xv will be the row number of the state
  act.fun = function(xv,...) {
    restore.point("act.fun")
    #cat("\n xv=",xv)
    na1 = sdf$na1[xv]; na2 =sdf$na2[xv]
    list(
      val = list(a1=1:na1, a2=1:na2),
      lab = 1:(na1*na2),
      i = 1:2
    )
  }

  # Payoff function. Must be vectorized over avm, xvm
  g.fun = function(avm,xvm,...) {
    restore.point("g.fun");

    df = quick_df(xv=xvm[,1], av1=avm[,1],av2=avm[,2])

    rows = match.by.cols(df, g$pi.df, cols=c("xv","av1","av2"))
		cbind(g$pi.df[rows,"pi1"],g$pi.df[rows,"pi2"])
  }

  xd.cols = 1:NROW(sdf)
  names(xd.cols) = as.character(sdf$x)

  # State transitions
  # For a matrix of action profiles and states specifies
  # the matrix of state transitions
  # Will be called separetly for each x.group
  tau.fun = function(avm,xvm,m=NULL) {
    restore.point("tau.fun")
    rownames(avm)=rownames(xvm)=NULL

    row = xvm[1]
    trans.mat = sdf$trans.mat[[ row ]]
    tau = matrix(0,NROW(avm),NROW(g$sdf))

    if (is.null(trans.mat)) {
      tau[,row] = 1
    } else {
      dest = xd.cols[colnames(trans.mat)]
      tau[,dest] = trans.mat
    }
    tau
  }

  # States can be grouped into sets of states with same
  # action sets.
  # In our game each state has a different action set
  x.group = function(xvm,...) {
    1:NROW(xvm)
  }

  # return required information for the dynamic game
  game = list(
    n=2,
    delta=g$param$delta,
    xv.val = list(1:NROW(sdf)), # states
    act.fun=act.fun,
    g.fun=g.fun,
    tau.fun=tau.fun,
    x.group=x.group
  )
  m = init.game(game, no.labels = TRUE,symmetric = symmetric)
  m
}
