# Solve MPE

mpe.example = function() {
  x.df = quick_df(x=c("x1","x2"), x_add = c(0,10))
  g = rel_game("Bertrand with Investment") %>%
    rel_states(x.df,A1, vec.pi.fun=vec.pi.fun, vec.trans.fun=vec.trans.fun, vec.static.pi.fun = vec.static.pi.fun, static.A.fun = static.A.fun) %>%
    rel_compile()


  g.mpe = rel_mpe(g, delta=0.9)

}

rel_mpe = function(g, delta=g$param$delta, static.eq=NULL, max.iter = 1000, tol=1e-8, ax=NULL) {
  restore.point("rel_mpe")

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
  u.old = matrix(-Inf, nx, 2)
  iter = 0


  pi.mat=cbind(g$ax.pi$pi1,g$ax.pi$pi2)
  i = 1
  while (iter <= max.iter) {
    iter = iter+1
    T = as.matrix(ax.trans[ax, ])
    u = solve(diag(nx) - delta * T, (1 - delta) *
        (pi.mat[ax,]+static.u))
    if (approxeq(u, u.old, tol = tol)) {
        break
    }
    if (i==1) {
      ax = c_pl1_best_reply_ax(u[,1],ax,sdf$na1,sdf$na2)
    } else {
      ax = c_pl2_best_reply_ax(u[,2],ax,sdf$na1,sdf$na2)
    }
    i = (i %% 2) + 1
    u.old = u
  }
  quick_df(x=sdf$x,u1=u[,1],u2=u[,2],a=ax-g$sdf$lag.cumsum.na)

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
          nash.ind = which.max(pi1[nash]+pi2[nash])
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
  quick_df(x=sdf$x[xrows], a=res.a, u1=res.u1, u2=res.u2)
}
