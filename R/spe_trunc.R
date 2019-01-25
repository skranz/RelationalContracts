# Find SPE of truncated games with fixed r

examples.spe.trunc = function() {
  g0 = matrix(c(1,2,-2,0),2,2)
  g1 = matrix(c(1,4,-4,0),2,2)

  g = rel_game("PD 2 States") %>%
    rel_state("x0",A1=list(a_1=c("C1","D1")),A2=list(a_2=c("C2","D2")),pi1=g0, pi2=t(g0)) %>%
    rel_state("x1",A1=list(a_1=c("C1","D1")),A2=list(a_2=c("C2","D2")),pi1=g1, pi2=t(g1)) %>%
    rel_transition("x0","x1", prob=0.1) %>%
    rel_compile() %>%
    rel.prepare.for.spe()
  br1 = trunc.spe.cheating.payoffs(g,i=1,delta=0.2)
  br2 = trunc.spe.cheating.payoffs(g,i=2,delta=0.2)

  cbind(bind_rows(g$sdf$a.grid), pi1=unlist(g$sdf$pi1),br1=br1, pi2=unlist(g$sdf$pi2),br2=br2)



  g = rel_game("Principal-Agent Variation") %>%
    rel_state("x0",A2=list(e=c(0,1)),pi1=~ e, pi2=~ -0.5*e) %>%
    rel_state("x1",pi1=0.3, pi2=0.3) %>%
    rel_transition("x0","x1", e=1) %>%
    rel_transition("x1","x0") %>%
    rel_compile() %>%
    rel.prepare.for.spe()

  g$ax.pi

  animate.capped.rne.history(g,x=NULL)

}


solve.trunc.spe = function(g,tol.feasible = 1e-10, verbose=FALSE,r1 = g$sdf$r1; r2 = g$sdf$r2) {
  restore.point("solve.trunc.spe")

  R = r1+r2
  delta = g$param$delta
  rho = g$param$rho
  beta1 = g$param$beta1

  sdf = g$sdf
  ax.pi = g$ax.pi

  nax = NROW(ax.pi)
  nx = NROW(sdf)

  # admissible ax action profiles
  admiss = 1:nax
  admiss.sizes = sdf$na.vec

  infeas.e = TRUE
  infeas.1 = TRUE
  infeas.2 = TRUE

  iter = 0
  while(TRUE) {
    iter = iter+1

    if (verbose) {
      cat("\n")
      cat("\n*************************************************************")
      cat(paste0("\nIteration ",iter))
    }

    # Calculate optimal equilibrium state actions
    # if some of the previous action profiles became infeasible
    if (infeas.e) {
      if (verbose) {
        print("Compute highest payoff")
      }
      res = trunc.spe.highest.U(g, admiss, admiss.sizes)
      U = res$U
      axe = res$ax
    }

    # a1
		if (infeas.1) {
		  res = trunc.spe.harshest.punishment(g,i=1, admiss=admiss, admiss.sizes=admiss.sizes)
			v1 = res$vi
			ax1 = res$ax
			# Cheating payoffs for all ax given the just
			# calculated punishment payoff  for all states x
			q1.hat = trunc.spe.cheating.payoffs(g,i=1,delta=delta, rho=rho,v = v1,r=r1)[admiss]
		}
    # a2
		if (infeas.2) {
		  res = trunc.spe.harshest.punishment(g,i=2, admiss=admiss, admiss.sizes=admiss.sizes)
			v2 = res$vi
			ax2 = res$ax
			# Cheating payoffs for all ax given the just
			# calculated punishment payoff  for all states x
			q2.hat = trunc.spe.cheating.payoffs(g,i=1,delta=delta, rho=rho,v = v2,r=r2)[admiss]
		}

    V = v1+v2

    U.hat = (1-delta)*ax.pi$Pi[admiss] +
      delta * (g$ax.trans[admiss,,drop=FALSE] %*% ((1-rho)* U + rho*R))

    infeas.admiss = which(U.hat-q1.hat-q2.hat < -tol.feasible)
    infeas.ax = admiss[infeas.admiss]

    # Check whether all optimal action profiles are feasible
    # Note that policies are indixed on ax (not on admiss)
    infeas.e = any(axe %in% infeas.ax)
    infeas.1 = any(ax1 %in% infeas.ax)
    infeas.2 = any(ax2 %in% infeas.ax)
		# If none of the optimal action profiles is infeasible, we can stop
    if (!(infeas.e | infeas.1 | infeas.2)) break;

    # Remove infeasible action profiles from the set of admissible action profiles
    admiss = admiss[-infeas.admiss]
    admiss.sizes = tabulate(g$ax.pi$xrow[admiss],nx)

    # Remove newly infeasible rows
    # from q1.hat and q2.hat if they
    # are not newly computed next round
    if (!infeas.1) q1.hat = q1.hat[-infeas.admiss]
    if (!infeas.2) q2.hat = q2.hat[-infeas.admiss]
    #browser()
    if (any(admiss.sizes == 0)) {
      warning("There does not exist a subgame perfect equilibrium")
      return(NULL)
    }
  }
  ae = ax.pi$a[axe]
  a1 = ax.pi$a[ax1]
  a2 = ax.pi$a[ax2]

  r1 = v1 + beta1*(U-v1-v2)
  r2 = U-r1
  spe = quick_df(
    x=sdf$x,
    r1=r1,
    r2=r2,
    U=U,
    v1=v1,
    v2=v2,
    ae=ae,
    a1=a1,
    a2=a2
  )
  return(spe)
}


#' Calculates the highest joint payoff
#' The returned policy are indexed on ax (not on admiss)
trunc.spe.compute.highest.U = function(g, admiss, admiss.sizes) {
  restore.point("trunc.spe.compute.highest.U")

  T = g$ax.trans[admiss,,drop=FALSE]
  Pi = g$ax.pi$Pi[admiss]
  r = g$sdf$r1+g$sdf$r2

  res = trunc_policy_iteration(T=T,Pi=Pi,r=r,delta=g$param$delta,rho=g$param$rho, na.vec=admiss.sizes)
  return(list(Ue=res$V, ax = admiss[res$p]))
}

trunc.spe.harshest.punishment = function(g,i,admiss, admiss.sizes, tol=1e-10, verbose=FALSE, use.cpp=TRUE) {
  restore.point("trunc.spe.harshest.punishment")

  delta = g$param$delta
  rho = g$param$rho
  r = g$sdf[[paste0("r",i)]]

  # Start with action profiles that minimize player i's static cheating payoff
  cheat.pay = trunc.spe.cheating.payoffs(g,i)[admiss]

  # Get for every state that admissible action profile that minimizes player i's static cheating payoff
  act.axi = admiss[which.chunk.maxs(-cheat.pay,admiss.sizes, use.cpp=use.cpp)]

  # Get corresponding cheating payoffs
  # We solve a MDP for player i
  v = trunc.spe.full.dyn.vi(g,i,act.axi)$vi
  old.cheat.pay = cheat.pay
  counter = 0
  while (TRUE) {
    counter = counter+1
    cheat.pay = trunc.spe.cheating.payoffs(g,i,v=v,r=r, rho=rho,delta=delta)[admiss]
    # Stop if player i cannot improve his cheating payoff in any state
    if (approxeq(cheat.pay,old.cheat.pay,tol)) {
      break;
    }
    act.axi = admiss[which.chunk.maxs(-cheat.pay,admiss.sizes, use.cpp=use.cpp)]

    # Get corresponding cheating payoffs
    # We solve a MDP for player i
    v = trunc.spe.full.dyn.vi(g,i,act.axi)$vi
    old.cheat.pay = cheat.pay
  }
  #print(rbind(label.ax(m,a.to.ax(m,act.a)), v))
  if (verbose)
    cat("\n\nget.harshest.punishment iterations: ", counter,"\n")

  return(list(vi=v,ax = act.axi))
}

# ax.admiss is a nx x 1 vector of action profiles
# one profile for every state x
# static cheating payoffs are already added
trunc.spe.full.dyn.vi = function(g,i,axi, r=g$sdf[[paste0("r",i)]]) {
  restore.point("trunc.spe.full.dyn.vi")

  if (i==1) {
    replies = c_pl1_ax_replies(axi,g$sdf$na1,g$sdf$na2)
    pi = g$ax.pi$pi1[replies]
    sizes = g$sdf$na1
  } else {
    replies = c_pl2_ax_replies(axi,g$sdf$na1,g$sdf$na2)
    pi = g$ax.pi$pi2[replies]
    sizes = g$sdf$na2
  }
  # Transition function between states
  T = g$ax.trans[replies,,drop=FALSE]

  res = trunc_policy_iteration(T,pi,delta=g$param$delta,rho=g$param$rho,r=r,na.vec=sizes)
  opt.ax = replies[ret$p]
  return(list(vi=ret$V, ax=opt.ax))
}


# Returns player i's cheating payoff for every admissible ax profile
# This means we have duplication as several ax profiles correspond to one ax_i profile
trunc.spe.cheating.payoffs = function(g, i=1,v=rep(0,nx), r=rep(0,nx), delta = g$param$delta, rho=g$param$rho, nx=NROW(g$sdf), use.cpp=TRUE) {
  restore.point("r.trunc.spe.cheating.payoffs")

  sdf = g$sdf
  nax = g$nax
  sizes = sdf$na.vec
  if (i==1) {
    nai = sdf$na1
    naj = sdf$na2
    pi = g$ax.pi$pi1
  } else {
    nai = sdf$na2
    naj = sdf$na1
    pi = g$ax.pi$pi2
  }

  u_ax = (1-delta)*pi + delta*
    as.vector(g$ax.trans %*% ((1-rho)*v + rho*r))

  if (use.cpp) {
    if (i==1) {
      c_pl1_ax_best_reply_payoffs(u_ax,nai,naj,nx)
    } else {
      c_pl2_ax_best_reply_payoffs(u_ax,nai,naj,nx)
    }

  } else {
    if (i==1) {
      r.pl1.ax.best.reply.payoffs(u_ax,nai,naj,nx)
    } else {
      r.pl2.ax.best.reply.payoffs(u_ax,nai,naj,nx)
    }
  }
}



r.pl1.ax.best.reply.payoffs = function(u_ax, nai, naj, nx) {
  restore.point("r.pl1.ax.best.reply.payoff")

  start_ind = 0
  br_ax = rep(0, length(u_ax))
  # Loop through all states
  for (xrow in 1:nx) {
    u_br = -Inf
    # Loop through other player's actions
    # in current state
    for (aj in 1:naj[xrow]) {
      ind = start_ind + aj
      # Loop through own actions
      # To find best reply payoff
      for (ai in 1:nai[xrow]) {
        ind = ind + (ai-1)*naj[xrow]
        u_cur = u_ax[ind]
        if (u_cur > u_br | ai==1) {
          u_br = u_cur
        }
      }
      # Loop through own actions to
      # set best reply payoff
      ind = start_ind + aj
      for (ai in 1:nai[xrow]) {
        ind = ind + (ai-1)*naj[xrow]
        br_ax[ind] = u_br
      }
    }
    start_ind = start_ind + naj[xrow]*nai[xrow]
  }
  br_ax
}
r.pl2.ax.best.reply.payoffs = function(u_ax, nai, naj, nx) {
  restore.point("r.pl2.ax.best.reply.payoff")
  start_ind = 0
  br_ax = rep(0, length(u_ax))

  # Loop through all states
  for (xrow in 1:nx) {
    u_br = -Inf

    # Loop through other player's actions
    # in current state
    for (aj in 1:naj[xrow]) {
      ind = start_ind + (aj-1)*nai[xrow]
      # Loop through own actions
      for (ai in 1:nai[xrow]) {
        ind = ind+1
        u_cur = u_ax[ind]
        if (u_cur > u_br | ai==1) {
          u_br = u_cur
        }
      }
      ind = start_ind + (aj-1)*nai[xrow]

      for (ai in 1:nai[xrow]) {
        ind = ind+1
        br_ax[ind] = u_br
      }
    }
    start_ind = start_ind + naj[xrow]*nai[xrow]
  }
  br_ax
}


rel.prepare.for.spe = function(g) {
  restore.point("rel.prepare.for.spe")
  sdf = g$sdf
  nx = NROW(g$sdf)
  na.vec = g$sdf$na1*g$sdf$na2
  lag.cumsum.na = c(0,cumsum(na.vec[-nx]))

  a = sequence(na.vec)
  nax = length(a)
  xrow = rep(1:nx, na.vec)
  ax = 1:nax

  # from ax to a
  # a = ax-lag.cumsum.na[xrow]

  pi1 = unlist(sdf$pi1)
  pi2 = unlist(sdf$pi2)
  Pi = pi1+pi2

  ax.trans = matrix(0,nax,nx)
  colnames(ax.trans) = sdf$x
  for (row in 1:nx) {
    trans.mat = sdf$trans.mat[[row]]
    ax.rows = lag.cumsum.na[row]+(1:na.vec[row])
    if (NROW(trans.mat)>0) {
      ax.trans[ax.rows,colnames(trans.mat)] = trans.mat
    } else {
      ax.trans[ax.rows,colnames(trans.mat)] = 1
    }
  }

  #
  sdf$na.vec = na.vec
  sdf$lag.cumsum.na = lag.cumsum.na
  g$sdf = sdf

  g$ax.pi = quick_df(xrow=xrow, a=a, ax=ax, pi1=pi1,pi2=pi2,Pi=Pi)
  g$ax.trans = ax.trans
  g$nax = nax
  g

}
