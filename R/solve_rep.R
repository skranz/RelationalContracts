#' Solves for all states the repeated game assuming the state is fixed
#'
#' @returns Returns a game object that contains a field 'rep.games.df'.
#' This data frame contains the relevant information to compute
#' equilibrium payoffs and equilibria for all
#' discount factors for all states.
rel_solve_all_repgames = function(g, overwrite=FALSE) {
  restore.point("rel_solve_all_repgames")
  if (!overwrite & !is.null(g$rep.games.df))
    return(g)

  if (isTRUE(g$is.multi.stage))
    return(solve.all.rep.multistage(g, overwrite=overwrite))


  sdf = g$sdf
  li = lapply(1:NROW(sdf), function(row) {
    if (is.null(sdf$rep[[row]])) {
      solve.x.repgame(g,row=row)
    } else {
      sdf$rep[[row]]
    }
  })
  g$rep.games.df = bind_rows(li)
  g
}

#' Get the results of all repeated games assuming the state is fixed
#'
#' Returns for all discount factors the optimal simple strategy profiles
#' maximum joint payoffs and punishment profiles
get.repgames.results = function(g, action.details=TRUE) {
  if (is.null(g$rep.games.df)) {
    stop("Please first call rel_solve_all_repgames")
  }
  res = g$rep.games.df
  if (action.details) {
    res = add.action.details(g,res,c("ae","a1","a2"))
  }
  res
}

# Solve the repeated game that would stay forever in state x
solve.x.repgame = function(g,x=NULL,row=NULL) {
  restore.point("solve.x.repgame")

  if (!is.null(row)) {
    x=g$sdf$x[row]
  } else {
    row = which(g$sdf$x==x)
  }

  state = g$sdf[row,]

  g1 = t(matrix(state$pi1[[1]], state$na2,state$na1))
  g2 = t(matrix(state$pi2[[1]], state$na2,state$na1))
  lab.ai = make.state.lab.ai(state)

  rg = repgame::init.game(n=2, g1=g1,g2=g2,lab.ai = lab.ai)
  sol = repgame::solve.game(rg)

  beta1 = g$param$beta1
  beta2 = 1-beta1

  opt.mat = sol$opt.mat

  U = opt.mat[,"Ue"]
  v1_rep = opt.mat[,"v1"]
  v2_rep = opt.mat[,"v2"]
  r1 = v1_rep + beta1 * (U-v1_rep-v2_rep)
  r2 = v2_rep + beta2 * (U-v1_rep-v2_rep)


  rg$lab.a
  res = quick_df(
    x=x,
    delta_min=opt.mat[,"delta"],
    delta_max=c(opt.mat[,"delta"][-1],1),
    r1 = r1,
    r2 = r2,
    U = U,
    v1_rep = v1_rep,
    v2_rep = v2_rep,
    strat.lab = rownames(opt.mat),
    ae = opt.mat[,"ae"],
    a1 = opt.mat[,"a1"],
    a2 = opt.mat[,"a2"]
  )
  res
}


