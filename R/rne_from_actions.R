#' Checks if an equilibrium eq with negotiation payoffs is an RNE
#'
#' We simply solve the truncated game with r1 and r2
#' and check whether the resultig r1 and r2 are the same
rel_is_eq_rne = function(g, eq=g[["eq"]],  r1 = eq$r1,  r2 = eq$r2, r.tol=1e-10, verbose=TRUE) {
  restore.point("rel_is_eq_rne")

  # Solve for the SPE of the capped equilibrium
  g = rel_spe(g, r1=r1, r2 = r2, no.exist.action="nothing")

  spe = g[["eq"]]
  if (is.null(spe)) {
    cat("\nThe truncated game has no SPE in pure strategies. No RNE found.")
    g$found.rne = FALSE
    return(g)
  }
  spe$trunc.r1 = r1
  spe$trunc.r2 = r2

  g$spe = g$eq = spe
  #diff = compare.eq(eq,g[["eq"]])
  if (max(abs(r1-spe$r1)) < r.tol & max(abs(r2-spe$r2)) < r.tol) {
    cat("\nCongratulations, we found an RNE in pure strategies. Call get.eq to retrieve the RNE.")
    g$found.rne = TRUE
  } else {
    if (verbose)
      cat("\nSorry, but the SPE of the truncated game yields different negotiation payoffs than we entered. We did not find an RNE. Call get.eq to retrieve the SPE of the truncated game.")
    g$found.rne = FALSE
  }
  return(g)
}

# Just take the equilibrium action profiles
# Compute the optimal payoffs and r
# Then check if for those r we have a SPE of the
# truncated game
rel_rne_from_eq_actions = function(g, eq=g[["eq"]],iterations=1,save.eq.li=FALSE,...) {
  restore.point("rel_rne_from_eq_actions")
  if (iterations > 1) {
    restore.point("rel_rne_from_eq_actions_multi")
    if (save.eq.li) {
      g$eq.li = list("vector", iterations+1)
      eq$iteration = 0
      g$eq.li[[1]] = eq
    }

    for (iter in 1:iterations) {
      cat("\nIteration ", iter)
      g = rel_rne_from_eq_actions(g,eq=eq, iterations=1,...)
      #g = rel_rne_from_eq_actions(g,eq=eq, iterations=1)
      eq = g[["eq"]]
      if (save.eq.li & !is.null(eq)) {
        eq$iteration = iter
        g$eq.li[[iter+1]] = eq
      }

      if (g$found.rne)
        return(g)
      if (is.null(eq))
        return(g)
    }
    return(g)
  }

  # Compute r1 and r2 corresponding to the
  # action profiles of the capped equilibrium
  r_eq = compute.optimal.payoffs.from.eq.actions(g, eq)
  g = rel_is_eq_rne(g,r_eq,...)
  g
}

rel_rne_from_capped = function(g, eq = g$capped_rne,...) {
  rel_rne_from_eq_actions(g, eq, ...)
}


# Assume we have a guess of equilibrium action profiles
# e.g. from solving a capped game
compute.optimal.payoffs.from.eq.actions = function(g, eq) {
  restore.point("compute.optimal.payoffs.from.eq.actions")

  delta = g$param$delta
  rho = g$param$rho
  sdf = g$sdf

  # 1. Find a1.hat from a1
  ax1 = eq.a.to.ax(g,eq$a1)
  q1 = (1-delta)*g$ax.pi$pi1 +
    as.vector(g$ax.trans %*% ((1-rho)*eq$v1+rho*eq$r1))
  ax1.hat = c_pl1_best_reply_ax(q1,ax1,sdf$na1,sdf$na2)
  a1.hat = eq.ax.to.a(g,ax1.hat)

  # 2. Find a2.hat from a2
  ax2 = eq.a.to.ax(g,eq$a2)
  q2 = (1-delta)*g$ax.pi$pi2 +
    as.vector(g$ax.trans %*% ((1-rho)*eq$v2+rho*eq$r2))
  ax2.hat = c_pl2_best_reply_ax(q2,ax2,sdf$na1,sdf$na2)
  a2.hat = eq.ax.to.a(g,ax2.hat)

  ind.mat = cbind(ae=eq$ae, a1.hat=a1.hat, a2.hat=a2.hat)

  if (isTRUE(g$is.multi.stage)) {
    res = compute.optimal.payoffs.from.actions(g, ind.mat, eq$static.Pi, eq$static.c1, eq$static.c2)
  } else {
    res = compute.optimal.payoffs.from.actions(g, ind.mat)
  }
  res
}


# Computes after.cap.payoffs given some fixed action
# profiles
compute.optimal.payoffs.from.actions = function(g, ind.mat = compute.after.cap.action.inds(g), static.Pi=rep(0,nx), static.c1=rep(0,nx), static.c2=rep(0,nx),nx=NROW(g$sdf)) {
  restore.point("compute.optimal.payoffs.from.actions")

  delta = g$param$delta
  rho = g$param$rho
  beta1 = g$param$beta1
  beta2 = 1-beta1

  sdf = g$sdf
  nx = NROW(sdf)

  if (any(is.na(ind.mat))) {
    stop("After cap actions are not specified for all states. Cannot compute after-cap payoffs.")
  }
  k.var="ae"

  pi = rep(NA_real_,nx)
  tau = matrix(0,nx,nx)
  colnames(tau) = sdf$x

  row = 1
  for (row in 1:nx) {
    ind = ind.mat[row,k.var]
    # Joint payoffs
    pi[row] = sdf$pi1[[row]][ind] + sdf$pi2[[row]][ind]+static.Pi[row]
    # Fill dense transition matrix
    trans.mat = sdf$trans.mat[[row]]
    cols =colnames(trans.mat)
    if (NROW(trans.mat)==0) {
      tau[row,cols] = 1
    } else {
      tau[row, cols] = trans.mat[ind,]
    }
  }
  U = solve(diag(nx)-delta*tau, (1-delta)*pi)

  k.var = "a1.hat"; tau[] = 0;
  for (row in 1:nx) {
    ind = ind.mat[row,k.var]
    # Player i's payoff
    pi[row] = sdf$pi1[[row]][ind]+static.c1[row]
    trans.mat = sdf$trans.mat[[row]]
    cols =colnames(trans.mat)
    if (NROW(trans.mat)==0) {
      tau[row,cols] = 1
    } else {
      tau[row, cols] = trans.mat[ind,]
    }
  }
  tau1 = tau; pi1 = pi
  #v1 = solve(diag(nx)-delta*tau, (1-delta)*pi)

  k.var = "a2.hat"; tau[] = 0;
  for (row in 1:nx) {
    ind = ind.mat[row,k.var]
    # Player i's payoff
    pi[row] = sdf$pi2[[row]][ind]+static.c2[row]
    trans.mat = sdf$trans.mat[[row]]
    cols =colnames(trans.mat)
    if (NROW(trans.mat)==0) {
      tau[row,cols] = 1
    } else {
      tau[row, cols] = trans.mat[ind,]
    }
  }
  tau2 = tau; pi2 = pi

  # Don't need to bother about new negotiations
  if (rho == 0) {
    v1 = solve(diag(nx)-delta*tau1, (1-delta)*pi1)
    v2 = solve(diag(nx)-delta*tau2, (1-delta)*pi2)
    r1 = v1 + beta1*(U-v1-v2)
    r2 = U-r1
    return(
      quick_df(x=sdf$x,r1=r1,r2=r2,U=U,v1=v1,v2=v2,
        ae=ind.mat[,1], a1=ind.mat[,2], a2=ind.mat[,3])
    )
  }

  # Shorter computation with 2x2 blocks
  # w = c(v1,v2)
  # w = b + A %*% w
  b = c(
    (1-delta)*pi1 + delta*rho*beta1*tau1 %*% U, # v1
    (1-delta)*pi2 + delta*rho*beta2*tau2 %*% U  # v2
  )
  # Matrix part for v1
  A = rbind(
    # matrix part for v1
    cbind(
      delta*tau1*(1-rho + rho*(1-beta1)),
      -delta*tau1*rho*beta1
    ),
    # matrix part for v2
    cbind(
      -delta*tau2*rho*beta2,
      delta*tau2*(1-rho + rho*(1-beta2))
    )
  )
  # Solve for w: (I-A) w = b
  w = solve( diag(2*nx)-A, b)
  v1=w[1:nx]
  v2=w[(nx+1):(2*nx)]
  r1 = v1+beta1*(U-v1-v2)
  r2 = U-r1


  res = quick_df(x=sdf$x,
    r1=r1,
    r2=r2,
    U=U,
    v1=v1,
    v2=v2,
    ae=ind.mat[,1], a1=ind.mat[,2], a2=ind.mat[,3]
  )
  #res %>% mutate(r1.check = v1+beta1*(U-v1-v2))
  return(res)




  # Now we need to solve simultaneously for
  # v1, v2, r1, r2
  # By stacking
  #
  # w = c(v1,v2,r1,r2)
  #
  # w = b + A %*% w
  #
  # b = c( (1-delta)*pi1, (1-delta*) )

  b = c(
    (1-delta)*pi1, # v1 const
    (1-delta)*pi2, # v2 const
    beta1*U,       # r1 const
    beta2*U        # r2 const
  )

  zeros = matrix(0,nx,nx)
  I = diag(nx)
  # Matrix part for v1
  A1 = cbind( delta*tau1*(1-rho), zeros, delta*tau1*rho, zeros)
  # Matrix part for v2
  A2 = cbind(zeros, delta*tau2*(1-rho), zeros, delta*tau2*rho)
  # Matrix part for r1
  A3 = cbind((1-beta1)*I, -beta1*I, zeros, zeros)

  # Matrix part for r2
  A4 = cbind( -beta2*I,(1-beta2)*I, zeros, zeros)

  A = rbind(A1,A2,A3,A4)

  # Solve for w: (I-A) w = b
  w = solve( diag(4*nx)-A, b)


  res = quick_df(x=sdf$x,
    r1=w[(2*nx+1):(3*nx)],
    r2=w[(3*nx+1):(4*nx)],
    U=U,
    v1=w[1:nx],
    v2=w[(nx+1):(2*nx)],
    ae=ind.mat[,1], a1=ind.mat[,2], a2=ind.mat[,3]
  )
  #res %>% mutate(r1.check = v1+beta1*(U-v1-v2))
  return(res)

}


