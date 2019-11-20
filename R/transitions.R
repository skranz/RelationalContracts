examples.rel_state_probs = function() {
  g = rel_game() %>%
    rel_state("x1",pi1=0, pi2=0) %>%
    rel_state("x2",pi1=0, pi2=0) %>%
    rel_transition("x1","x2",prob=0.5) %>%
    rel_transition("x2","x1",prob=0.5) %>%
    rel_spe() %>%
    rel_state_probs(x0="first")

  get_eq(g) %>%
    select(x, state.prob)
  diagnose_transitions(g)

}

#' Compute the long run probability distribution over states if an
#' equilibrium is played for many periods.
#'
#' Adds a column \code{state.prob} to the computed equilibrium data frame,
#' which you can retrieve by calling \code{get_eq}.
#'
#' If the equilibrium strategy induces a unique stationary distribution over
#' the states, this distribution should typically be found (or at least approximated). Otherwise the result can depend on the parameters.
#'
#' The initial distribution of states is determined by the parameters
#' \code{x0} or \code{start.prob}. We then multiply
#' the current probabilities susequently \code{n.burn.in} times with
#' the transitition matrix on the equilibrium path.
#' This yields the probability distribution over states assuming
#' the game is played for \code{n.burn.in} periods.
#'
#' We then continue the process for \code{n.averaging} rounds, and return
#' the mean of the state probability vectors over these number of rounds.
#'
#' If between two rounds in the burn-in phase the transitition probabilities
#' of no state pair change by more than the parameter \code{tol}, we
#' immediately stop and use the resulting probabilit vector.
#'
#' Note that for T-RNE or capped RNE we always take the transition
#' probabilities of the first period, i.e. we don't increase the t in
#' the actual state definition.
#'
#' @param g the game object for which an equilibrium has been solved
#' @param x0 the initial state, by default the first state. If \code{initial} is not specified we asssume the game starts with probability 1 in the initial state. We have 3 reserved keywords: \code{x0="equal"} means all states are equally likely, \code{x0="first"} means the game starts in the first state, \code{x0="first.group"} means all states of the first \code{xgroup} are equally likely.
#'
#' @param start.prob an optional vector of probabilities that specifies for each state the probability that the game starts in that state. Overwrites "x0" unless kept NULL.
#' @param n.burn.in Number of rounds before probabilities are averaged.
#' @param n.averaging Number of rounds for which probabilities are averaged.
#' @param tol Tolerance such that computation stops already in burn-in phase if transition probabilities change not by more than tol.
#' @value A game object whose equilibrium data frames have the additional column \code{state.prob}
rel_state_probs = function(g, x0=c("equal","first","first.group")[1], start.prob = NULL, n.burn.in=100, n.averaging=100, tol=1e-13, eq.field = "eq") {
  restore.point("rel_state_probs")

  nx = NROW(g$sdf)
  if (is.null(start.prob)) {
    if (identical(x0,"equal")) {
      start.prob = rep(1/nx,nx)
    } else {
      if (identical(x0,"first")) {
        xrow = 1
      } else if (identical(x0,"first.group")) {
        xgroup = g$x.df$xgroup[1]
        xrow = which(g$x.df$xgroup == xgroup)
      } else {
        xrow = which(g$sdf$x %in% x0)
        if (length(xrow)==0)
          stop(paste0("Could not find state ",paste0(x0, collapse=", "),"."))
      }
      start.prob = rep(0, nx)
      start.prob[xrow] = 1 / length(xrow)
    }
  }
  eq = g[[eq.field]]
  mat = compute.eq.trans.mat(g, eq=eq, ae=eq$ae)
  res = start.prob
  change = NA
  for (i in seq_len(n.burn.in)) {
    nres = res %*% mat
    change = max(abs(res-nres))
    if (change < tol)
      break
    res = nres
  }

  if (change >= tol) {
    avg = res / n.averaging
    for (i in seq_len(n.averaging-1)) {
      res = res %*% mat
      avg = avg + res / n.averaging
    }
    res = avg
  }
  res = as.vector(res)

  g$state.prob.change = change
  same.eqs = find.same.eqs(g, eq)
  eq$state.prob = res
  for (ef in same.eqs) {
    g[[ef]] = eq
  }
  g
}


find.same.eqs = function(g, eq=g[["eq"]]) {
  same = NULL
  candidates = c("eq","mpe","rne","spe")
  is.same = sapply(candidates, function(cand) identical(g[[cand]],eq))
  candidates[is.same]
}


compute.eq.trans.mat = function(g, ae = eq$ae, eq=g[["eq"]]) {
  restore.point("compute.eq.trans.mat")
  if (!is.null(g$ax.trans)) {
    ax = eq.a.to.ax(g,a=ae)
    g$ax.trans[ax,,drop=FALSE]
  } else {
    nx = NROW(g$sdf)
    mat = matrix(0,nx,nx)
    colnames(mat) = g$sdf$x
    for (xrow in 1:NROW(g$sdf)) {
      tm = g$sdf$trans.mat[[xrow]]
      if (NROW(tm)==0) {
        mat[xrow, colnames(tm)] = 1
      } else {
        mat[xrow, colnames(tm)] = tm[ae[xrow],]
      }
    }
    mat
  }

}

