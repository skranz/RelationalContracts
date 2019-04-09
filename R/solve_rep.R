#' Solves for all specified states the repeated game assuming the state is fixed
#'
#' @returns Returns a game object that contains a field 'rep.games.df'.
#' This data frame contains the relevant information to compute
#' equilibrium payoffs and equilibria for all
#' discount factors for all states.
rel_solve_repgames = function(g,x=g$sdf$x, overwrite=FALSE, rows=match(x, g$sdf$x), use.repgame.package=FALSE) {
  restore.point("rel_solve_repgames")
  if (!isTRUE(g$is_compiled)) stop("Please first call rel_compile")


  is.multi.stage = isTRUE(g$is.multi.stage)
  added = 0
  for (row in rows) {
    if (is.null(g$sdf$rep[[row]]) | overwrite) {
      added = added+1
      if (is.multi.stage) {
        g$sdf$rep[[row]] = solve.x.rep.multistage(g,row=row)
      } else {
        if (!use.repgame.package) {
          if (is.null(g[["rep.li"]]))
            g$rep.li = compute.rep.game.action.lists(g$sdf)
          g$sdf$rep[[row]] = solve.x.repgame(g,row=row)
        } else {
          g$sdf$rep[[row]] = solve.x.repgame.external(g,row=row)
        }
      }
    }
  }
  if (added >0)
    g$rep.games.df = bind_rows(g$sdf$rep)
  g
}


#' Get the results of all solved repeated games assuming the state is fixed
#'
#' Returns for all discount factors the optimal simple strategy profiles
#' maximum joint payoffs and punishment profiles
get.repgames.results = function(g, action.details=TRUE, delta=g$param$delta, rho=g$param$rho) {
  if (is.null(g$rep.games.df)) {
    stop("Please first call rel_solve_repgames")
  }
  res = g$rep.games.df
  res = left_join(g$x.df, res,by="x")
  if (action.details) {
    res = add.action.details(g,res,c("ae","a1","a2"))
  }
  if (!is.null(delta)) {
    adj.delta = delta*(1-rho)
    res = filter(res, adj.delta >= delta_min, adj.delta < delta_max)
  }
  res
}



# Solving a repeated game with perfect monitoring
# use own algorithm: no repgame package
solve.x.repgame = function(g,x=NULL,row=NULL, tol=1e-10, beta1=g$param$beta1, make.strat.lab=FALSE) {
  restore.point("solve.x.rep")

  if (!is.null(row)) {
    x=g$sdf$x[row]
  } else {
    row = which(g$sdf$x==x)
  }
  # Action lists containing ae.df, a1.df, a2.df
  d.li = g$rep.li[[row]]

  # Number of list elements
  d.len = sapply(d.li, NROW)

  # Starting pos in each list
  a.pos = c(1,1,1)

  # Number of maximally considered action profile combinations
  ncomb = 1+sum(d.len-1)

  res.r = matrix(NA_real_,ncomb,7)
  res.i = matrix(NA_integer_,ncomb,3)

  colnames(res.r) = c("delta_min","delta_max","r1","r2","U","v1_rep","v2_rep")
  colnames(res.i) = c("ae","a1","a2")

  cur.comb = 0
  lowest.delta = Inf

  # Go through all combinations of possible action profiles
  while(all(a.pos<=d.len)) {
    ae = d.li$ae.df[a.pos[1],]
    a1 = d.li$a1.df[a.pos[2],]
    a2 = d.li$a2.df[a.pos[3],]

    Md = ae["G"] - a1["c1"] - a2["c2"]

    # We have a stage game NE
    if (Md==0) {
      cur.comb = cur.comb+1
      v1 =  a1["c1"]
      v2 =  a2["c2"]
      U =  ae["G"]
      res.r[cur.comb,] = c(
        0, # delta_min
        lowest.delta, # delta_max
        v1+beta1*(U-v1-v2), #r1
        v2+(1-beta1)*(U-v1-v2), #r2
        U,
        v1,
        v2
      )
      res.i[cur.comb,] = c(
        ae=ae[".a"],
        a1=a1[".a"],
        a2=a2[".a"]
      )
      lowest.delta = 0
      break
    }

    # Critical interest rates
    r.ae = Md / (ae["L"])
    r.a1 = Md / (a1["L"])
    r.a2 = Md / (a2["L"])

    r.crit = c(r.ae,r.a1,r.a2)
    delta.crit = 1 / (1+r.crit)
    max.delta.crit = max(delta.crit)

    # Combination does reduce delta
    if (max.delta.crit < lowest.delta) {
      cur.comb = cur.comb+1

      v1 =  a1["c1"]
      v2 =  a2["c2"]
      U =  ae["G"]
      res.r[cur.comb,] = c(
        max.delta.crit, # delta_min
        lowest.delta, # delta_max
        v1+beta1*(U-v1-v2), #r1
        v2+(1-beta1)*(U-v1-v2), #r2
        U,
        v1,
        v2
      )
      res.i[cur.comb,] = as.integer(c(
        ae=ae[".a"],
        a1=a1[".a"],
        a2=a2[".a"]
      ))
      lowest.delta = max.delta.crit
    }

    replace = which(delta.crit >= lowest.delta)
    a.pos[replace] = a.pos[replace]+1

  }

  res = cbind(
    quick_df(x=rep(x, cur.comb)),
    as.data.frame(res.r[cur.comb:1,,drop=FALSE]),
    as.data.frame(res.i[cur.comb:1,,drop=FALSE])
  )

  if (make.strat.lab) {
    d.labs = g$a.labs.df$lab[g$a.labs.df$x==x]
    res$strat.lab = paste0("(",d.labs[res$ae],") (",d.labs[res$a1],") (",d.labs[res$a2],")")
  }

  res
}



# Solve the repeated game that would stay forever in state x
# use the repgame package
solve.x.repgame.external = function(g,x=NULL,row=NULL) {
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

  library(repgame)
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


