# Solve MPE

mpe.example = function() {
  x=c("x1","x2")
  g = rel_game("Two State") %>%
    rel_states(x,
      A1=list(m1=c(1,2)),
      A2=list(m2=c(1,2)),
      pi1 = 0,
      pi2 = 0,
      static.A1 = list(e=c(0,1)),
      static.pi1 =~ -1/2*e + (x=="x2")+1,
      static.pi2 =~ e + (x=="x2")+1
    ) %>%
    rel_transition("x1","x2",m1=2, m2=2, prob=1) %>%
    rel_transition("x1","x2",m1=1, m2=2, prob=0.2) %>%
    rel_transition("x1","x2",m1=2, m2=1, prob=0.2) %>%
    rel_compile() %>%
    rel_mpe(delta=0.9)

  cbind(g$sdf$a.grid[[1]],g$sdf$trans.mat[[1]])

  (mpe = get_mpe(g))
  mpe = g
}

get_mpe = function(g, extra.cols="ae", eq=g$mpe) {
  get_eq(g, extra.cols=extra.cols, eq=eq)
}

#' Tries to find a MPE by computing iteratively best replies
#'
#' Returns a game object that contains the mpe.
#' Use the function get_mpe to retrieve a data frame that describes the MPE.
#'
#' @param g the game
#' @param delta the discount factor
#' @param max.iter maximum number of iterations
#' @param tol we finish if payoffs in a subsequent iteration don't change by more than tol
#' @param ax optionaly an initially guess of the action profiles. A vector of size nx describing action profiles as ax indices
rel_mpe = function(g, delta=g$param$delta, static.eq=NULL, max.iter = 100, tol=1e-8, ax=NULL, add.stationary = TRUE) {
  restore.point("rel_mpe")

  g$param$delta = delta
  if (is.null(g$ax.trans))
    g = prepare.for.spe(g)

  multi.stage = isTRUE(g$is.multi.stage)

  if (multi.stage & is.null(static.eq))
    static.eq = static.nash.eq(g)


  sdf = g$sdf
  nx = NROW(sdf)
  if (!is.null(static.eq)) {
    static.u = cbind(static.eq$u1,static.eq$u2)
  } else {
    static.u = matrix(0,nx,2)
  }
  if (is.null(ax))
    ax = sdf$lag.cumsum.na+sdf$na.vec

  ax.trans = g$ax.trans
  ax.grid = g$ax.grid
  u.old = u = matrix(-Inf, nx, 2)
  iter = 0
  ax.xrow = g$ax.pi$xrow


  pi.mat=cbind(g$ax.pi$pi1,g$ax.pi$pi2)
  i = 1
  while (TRUE) {
    iter = iter+1
    T = as.matrix(ax.trans[ax, ])

    u[,i] = solve(diag(nx) - delta * T, (1 - delta) *
        (pi.mat[ax,i]+static.u[,i]))
    if (approxeq(u, u.old, tol = tol)) {
        break
    }
    q_i = (1-delta)*(pi.mat[,i] + static.u[ax.xrow,i])+
      delta * as.vector(ax.trans %*% u[,i])

    if (i==1) {
      ax = c_pl1_best_reply_ax(q_i,ax,sdf$na1,sdf$na2)
    } else {
      ax = c_pl2_best_reply_ax(q_i,ax,sdf$na1,sdf$na2)
    }
    i = (i %% 2) + 1
    u.old = u
    if (iter > max.iter) {
      warning(paste0("Reached limit of ", max.iter, " iteration and no MPE was found."))
      break
    }
  }

  if (!multi.stage) {
    mpe = bind_cols(g$x.df,quick_df(u1=u[,1],u2=u[,2],ae=ax-g$sdf$lag.cumsum.na))
  } else {
    mpe = bind_cols(g$x.df,quick_df(u1=u[,1],u2=u[,2],s.ae = static.eq$s.ae, ae=ax-g$sdf$lag.cumsum.na))
  }
  if (add.stationary) {
    mpe$stationary.prob = stationary.eq.distribution(g,mpe, ae = mpe$ae)
  }
  g$mpe = mpe
  g
}

static.nash.eq = function(g, gs=g$gs, xrows=seq_len(NROW(g$sdf)), ax.select=NULL, verbose=TRUE) {
  restore.point("static.nash.eq")

  sdf = gs$sdf
  xrow = 1
  pi1.li = sdf$pi1
  pi2.li = sdf$pi2
  na1.li = sdf$na1
  na2.li = sdf$na2

  n = length(xrows)
  res.a = rep(NA_integer_,n)
  res.u1 = rep(NA_real_,n)
  res.u2 = rep(NA_real_,n)

  xrow = 2
  counter = 0
  for (xrow in xrows) {
    counter = counter+1
    pi1 =pi1.li[[xrow]]
    pi2 = pi2.li[[xrow]]
    na1 = na1.li[[xrow]]
    na2 = na2.li[[xrow]]

    br1 = find.best.reply.payoffs(i=1,pi1,na1,na2)
    br2 = find.best.reply.payoffs(i=2,pi2,na1,na2)

    #eps1 = br1-pi1
    #eps2 = br2-pi2
    #plot(eps1, type="l", col="blue")
    #lines(eps2, type="l", col="red")


    nash = which((pi1 == br1) & (pi2==br2))
    #cbind(x=sdf$x[[xrow]],sdf$a.grid[[xrow]][nash,],pi1=pi1[nash],pi2= pi2[nash])


    # Just Pick the first Nash eqilibrium
    if (length(nash)>0) {
      if (length(nash)>1) {

        if (!is.null(ax.select)) {
          ax = nash + sdf$lag.cumsum.na[xrow]
          nash.ind = which.max(ax.select[ax])
        } else {
          # Choose NE with highest joint payoff by default
          # Break ties by preferring more similar payoffs
          #restore.point("hkhfdshfuirhei")
          ord = order(-(pi1[nash]+pi2[nash]), abs(pi1[nash]-pi2[nash]))
          nash.ind = ord[1]
          #nash.ind = which.max(pi1[nash]+pi2[nash])
        }
        if (verbose) {
          eq.msg = paste0("(",round(pi1[nash],2),",", round(pi2[nash],2),")")
          eq.msg[nash.ind] = paste0(eq.msg[nash.ind],"*")
          cat(paste0("\n\n", length(nash), " static NE in state ", sdf$x[xrow], ": ", paste0(unique(eq.msg), collapse=" ")))
        }

        nash = nash[nash.ind]
      }
      res.a[counter] = nash
      res.u1[counter] = br1[nash]
      res.u2[counter] = br2[nash]
    }
  }
  quick_df(x=sdf$x[xrows], s.ae=res.a, u1=res.u1, u2=res.u2)
}
