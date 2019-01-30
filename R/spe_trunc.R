# Find SPE of truncated games with fixed r


examples.multistage.spe.trunc = function() {
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


  vec.pi.fun = function(ax.df, i.cost=10*x.step, x.step=1,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = -i.cost*i1,
      pi2 = -i.cost*i2
    )
  }
  vec.static.pi.fun = function(ax.df, c1=0, c2=0,a=10,b=1,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = (a-b*(q1+q2)-c1)*q1,
      pi2 = (a-b*(q1+q2)-c2)*q2
    )
  }

  vec.trans.fun = function(ax.df,x.step,x.max,dep.prob=0,...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()
    ax.df = mutate(ax.df,
      i1.prob = i1 / (1+i1),
      i2.prob = i2 / (1+i2)
    )
    dp = dep.prob
    trans = independent.transitions(ax.df,
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
  x.df = as_data_frame(expand.grid(x1=x.seq,x2=x.seq))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)

  g = rel_game("Cournot with Investment") %>%
    rel_param(c1=0,c2=0,x.step=x.step, x.max=x.max,dep.prob=0.1,a=100, i.cost=10, i.seq=c(0,1,2)) %>%
    rel_states(x.df,A.fun=A.fun, vec.pi.fun=vec.pi.fun, vec.trans.fun=vec.trans.fun, vec.static.pi.fun = vec.static.pi.fun, static.A.fun = static.A.fun) %>%
    rel_compile()

  g = g %>%  rel_capped_rne(T=20, delta=0.9, rho=0.99, save.history = FALSE, use.cpp = TRUE, add.stationary = TRUE, save.details = TRUE)
  eq = g$eq
  eq$r_lab = paste0(round(eq$r1)," ", round(eq$r2),"\n", eq$ae.lab)
  ggplot(eq, aes(x=x1,y=x2, fill=stationary)) + geom_raster(interpolate=FALSE) + geom_label(aes(label=r_lab), fill="white", alpha=0.5, size=3, label.padding=unit(0.1,"lines"))
  rne.diagram(g, just.eq.chain = !TRUE)

  det = get.rne.details(g, x="100_0")
}

compute.eq.trans.mat = function(g, ae = if (isTRUE(g$is.multi.stage)) eq$d.ae else eq$ae, eq=g$eq) {
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



stationary.eq.distribution = function(g, eq=g$eq, tol = 1e-10, start=rep(1/NROW(g$sdf), NROW(g$sdf)), iterations=200) {
  restore.point("stationary.eq.distribution")
  mat = compute.eq.trans.mat(g, eq=eq)
  res = start
  for (i in 1:iterations) {
    nres = res %*% mat
    change = max(abs(res-nres))
    res = nres
  }
  res = as.vector(res)
  attr(res,"change") <- change
  res
}


examples.spe.trunc = function() {
  g0 = matrix(c(1,2,-2,0),2,2)
  g1 = matrix(c(1,4,-4,0),2,2)

  g = rel_game("PD 2 States") %>%
    rel_state("x0",A1=list(a_1=c("C1","D1")),A2=list(a_2=c("C2","D2")),pi1=g0, pi2=t(g0)) %>%
    rel_state("x1",A1=list(a_1=c("C1","D1")),A2=list(a_2=c("C2","D2")),pi1=g1, pi2=t(g1)) %>%
    rel_transition("x0","x1", prob=0.1) %>%
    rel_compile() %>%
    rel.prepare.for.spe()

  g$param$delta = 0.6
  spe = solve.trunc.spe(g, verbose=!TRUE)
  (spe)
}


solve.trunc.spe = function(g,tol.feasible = 1e-10, verbose=FALSE,r1 = g[["r1"]], r2 = g[["r2"]]) {
  restore.point("solve.trunc.spe")

  if (is.null(r1)) {
    r1 = r2 = rep(0, NROW(g$sdf))
  }
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

  v1 = v2 = rep(0,nx)
  iter = 0


  while(TRUE) {
    iter = iter+1

    if (verbose) {
      cat("\n")
      cat("\n*************************************************************")
      cat(paste0("\n     Iteration ",iter,"\n"))
    }

    # Calculate optimal equilibrium state actions
    # if some of the previous action profiles became infeasible
    if (infeas.e) {
      res = trunc.spe.highest.U(g, admiss, admiss.sizes,r1=r1,r2=r2)
      U = res$U
      axe = res$ax
      if (verbose) {
        cat(paste0("Highest payoffs: ", paste0(U, collapse=", ")))
      }

    }

    # a1
		if (infeas.1) {
		  res = trunc.spe.harshest.punishment(g,i=1, admiss=admiss, admiss.sizes=admiss.sizes, verbose=verbose, r=r1)
			v1 = res$vi
			ax1 = res$ax
			# Cheating payoffs for all ax given the just
			# calculated punishment payoff  for all states x
			q1.hat = trunc.spe.cheating.payoffs(g,i=1,delta=delta, rho=rho,v = v1,r=r1)[admiss]
			if (verbose) {
        cat(paste0("v1: ", paste0(v1, collapse=", ")))
      }

		}
    # a2
		if (infeas.2) {
		  res = trunc.spe.harshest.punishment(g,i=2, admiss=admiss, admiss.sizes=admiss.sizes,verbose=verbose,r=r2)
			v2 = res$vi
			ax2 = res$ax
			# Cheating payoffs for all ax given the just
			# calculated punishment payoff  for all states x
			q2.hat = trunc.spe.cheating.payoffs(g,i=1,delta=delta, rho=rho,v = v2,r=r2)[admiss]
			if (verbose) {
        cat(paste0("v2: ", paste0(v2, collapse=", ")))
      }

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
trunc.spe.highest.U = function(g, admiss, admiss.sizes, r1=g[["r1"]],r2=g[["r2"]]) {
  restore.point("trunc.spe.highest.U")

  T = g$ax.trans[admiss,,drop=FALSE]
  Pi = g$ax.pi$Pi[admiss]
  r = r1+r2

  res = trunc_policy_iteration(T=T,Pi=Pi,r=r,delta=g$param$delta,rho=g$param$rho, na.vec=admiss.sizes)
  return(list(Ue=res$V, ax = admiss[res$p]))
}

trunc.spe.harshest.punishment = function(g,i,admiss, admiss.sizes, tol=1e-10, verbose=FALSE, use.cpp=TRUE, r=g$sdf[[paste0("r",i)]], v= rep(0,NROW(g$sdf))) {
  restore.point("trunc.spe.harshest.punishment")

  delta = g$param$delta
  rho = g$param$rho

  # Start with action profiles that minimize player i's cheating payoffs
  # given previous v (initially previous v is 0)
  cheat.pay = trunc.spe.cheating.payoffs(g,i,v = v,r=r)[admiss]

 # cbind(g$ax.grid,cheat.pay, g$ax.pi)

  # Get for every state that admissible action profile that minimizes player i's static cheating payoff
  act.axi = admiss[which.chunk.maxs(-cheat.pay,admiss.sizes, use.cpp=use.cpp)]

  # Get corresponding cheating payoffs
  # We solve a MDP for player i
  v = trunc.spe.full.dyn.vi(g,i,act.axi, r=r)$vi
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
  opt.ax = replies[res$p]
  return(list(vi=res$V, ax=opt.ax))
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

#  if (FALSE) {
#    u_br = c_pl2_ax_best_reply_payoffs(u_ax,nai,naj,nx)
#    u_br_r = r.pl2.ax.best.reply.payoffs(u_ax,nai,naj,nx)
#    cbind(g$ax.grid,u_ax,u_br,u_br_r, g$ax.pi)
#  }
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
