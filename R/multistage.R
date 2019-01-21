# A special game with two action stages in a period
#
# In the first stage only static actions can be chosen that don't affect
# the state transitions, e.g. output in a Cournot model
#
# In the 2nd stage actions can be chosen that affect states, e.g. investments
# in a Cournot game.
#
# Separating into two stages can reduce dimensionality and can allow to solve games
# quicker and with less memory.
#
# New negotiations only take place in the first stage
# No discounting between stages

examples.multistage = function() {
  library(RelationalContracts)

  static.A.fun = function(x1,x2, x.seq,e.seq,...) {
    restore.point("static.A.fun")
    A1 = list(b1=c("b",""))
    A2 = quick_df(b2=c("b", rep("", length(e.seq))),e=c(0,e.seq))
    list(A1=A1,A2=A2)
  }

  A.fun = function(x1,x2,stage, x.seq,...) {
    restore.point("static.A.fun")
    A1 = list(a1=x.seq[x.seq>=x1])
    A2 = list(a2=x.seq[x.seq>=x2])
    list(A1=A1,A2=A2)
  }

  vec.static.pi.fun = function(ax.df,...) {
    restore.point("vec.pi.fun")
    ax.df %>%
      transmute(
        x=x,
        pi1= ifelse(b1 == "b" | b2=="b",-x1,e),
        pi2= ifelse(b1 == "b" | b2=="b",-x2,- 1/2 * e^2),
      )
  }

  vec.trans.fun = function(ax.df, final=FALSE,...) {
    restore.point("trans.fun")
    ax.df %>%
      select(x,a1,a2) %>%
      unique() %>%
      transmute(xs=x,xd=paste0(a1, " ",a2),a1=a1,a2=a2, prob=1)
  }


  x.seq = seq(0,1, by=0.1)
  #x.seq = c(0,0.01,0.05,0.1,0.2,0.5,1)
  x.df = as_data_frame(expand.grid(x1=x.seq,x2=x.seq, stringsAsFactors = FALSE)) %>%
    mutate(x= paste0(x1," ", x2))

  g = rel_game("Slowly Intensifying Repeated Principal-Agent") %>%
    rel_param(x.seq=x.seq, e.seq=seq(0,1,by=0.1)) %>%
    rel_states(x.df,
      # Static effort stage
      static.A.fun=static.A.fun,
      vec.static.pi.fun = vec.static.pi.fun,
      # Dynamic relationship intensification stage
      A.fun = A.fun,
      pi1 = 0, pi2=0,
      vec.trans.fun=vec.trans.fun
    )

  g = rel_compile(g)
  g = rel_capped_rne(g,T=2, adjusted.delta=0.17, rho=0.7,use.cpp=FALSE,tie.breaking = "random")

  g = rel_capped_rne(g,T=20, adjusted.delta=0.17, rho=0.7,tie.breaking = "random")
  library(microbenchmark)
  microbenchmark(
    rel_capped_rne(g,T=50,use.cpp=TRUE),
    rel_capped_rne(g,T=50,use.cpp=FALSE),
#    rel.capped.rne.multistage.old(g,T=20),
    times=1
  )

  rne.diagram(g,just.eq.chain = TRUE)
  (rne = g$rne)
}


# Called at the end of rel_compile
# if we have a repeated multistage game
# Compiles static stage
add.rel.multistage.compile = function(g,...) {
  restore.point("add.rel.multistage.compile")

  gs = g$static_defs
  gs$x.df = g$x.df
  gs$param = g$param
  gs = rel_compile(gs, compute.just.static = TRUE)

  g$gs = gs

  g$dyn.rep.li = compute.rep.game.action.lists(g$sdf)
  g$static.rep.li = compute.rep.game.action.lists(gs$sdf)

  g
}


capped.rne.multistage.iterations = function(g,T=1,rne=g$rne, tie.breaking, debug_row=-1, tol=1e-12, use.cpp=TRUE, save.details=FALSE, save.history=FALSE) {
  restore.point("capped.rne.multistage.iterations")

  if (T<=0) {
    return(list(rne=rne,details=NULL, history=NULL))
  }
  if (save.history & use.cpp) {
    use.cpp = FALSE
  }


  # TO DO: Compile transmats before in a useful form
  sdf = g$sdf
  if (use.cpp) {
    transmats = lapply(1:NROW(sdf), function(row) {
      trans.mat = sdf$trans.mat[[row]]
      if (NROW(trans.mat)==0) {
        x = sdf$x[row]; na1 = sdf$na1[row]; na2 = sdf$na2[row]
        trans.mat = matrix(1,na1*na2,1)
        colnames(trans.mat) = x
      }
      trans.mat
    })

    T.cpp = T-save.details

    if (T.cpp > 0) {
      rne = cpp_capped_rne_multistage_iterations(T=T.cpp, sdf=sdf,rne=rne,transmats=transmats,
        static_rep_li = g$static.rep.li,
        delta=g$param$delta, rho=g$param$rho,beta1 = g$param$beta1,
        tie_breaking=tie.breaking, tol=tol, debug_row=debug_row)
    }
    if (save.details) {
      res = r.capped.rne.multistage.iterations(T=1, g=g, rne=rne, tie.breaking=tie.breaking, save.details=save.details)
    } else {
      res = list(rne=rne, details=NULL)
    }
  } else {
    res = r.capped.rne.multistage.iterations(T=T, g=g, rne=rne, tie.breaking=tie.breaking,tol=tol, save.details=save.details, save.history=save.history)
  }
  res
}

# Iterate capped RNE over T periods using pure R
# Return res_rne
r.capped.rne.multistage.iterations = function(T, g, rne, tie.breaking, delta=g$param$delta, rho=g$param$rho,beta1 = g$param$beta1, tol=1e-12, save.details=FALSE, save.history=FALSE) {
  restore.point("r.capped.multistage.rne.iterations")

  delta=g$param$delta
  rho=g$param$rho

  sdf = g$sdf

  next_U = rne_U = rne$U
  next_v1 = rne_v1 = rne$v1; next_v2 = rne_v2 = rne$v2
  next_r1 = rne_r1 = rne$r1; next_r2 = rne_r2 = rne$r2

  rne_actions = matrix(0L, NROW(sdf),6)
  colnames(rne_actions) = c("s.ae","s.a1","s.a2","d.ae","d.a1","d.a2")

  if (save.details) {
    details.li = vector("list",NROW(sdf))
    x.df = g$x.df
  }
  if (save.history) {
    history.li = vector("list",T)
  }

  # Compute all remaining periods
  for (iter in seq_len(T)) {
    for (row in 1:NROW(sdf)) {
      x = sdf$x[row]
      # 1. Solve dynamic stage
      na1 = sdf$na1[row]
      na2 = sdf$na2[row]
      trans.mat = sdf$trans.mat[[row]]
      if (NROW(trans.mat)==0) {
        xd = x
      } else {
        xd = colnames(trans.mat)
      }
      dest.rows = match(xd, sdf$x)

      # Include code to compute U v and r for the current state
      U.hat = (1-delta)*(sdf[["pi1"]][[row]] + sdf[["pi2"]][[row]]) +
        delta * trans.mat.mult(trans.mat, next_U[dest.rows])

      # "q-value" of punishment payoffs
      q1.hat = (1-delta)*sdf$pi1[[row]] +
        delta * trans.mat.mult(trans.mat,( (1-rho)*next_v1[dest.rows] + rho*next_r1[dest.rows] ))

      q2.hat = (1-delta)*sdf$pi2[[row]] +
        delta * trans.mat.mult(trans.mat,( (1-rho)*next_v2[dest.rows] + rho*next_r2[dest.rows] ))

      # v1.hat is best reply q for player 1
      # Note player 1 is col player
      q1.hat = matrix(q1.hat,na2, na1)
      v1.hat.short = rowMaxs(q1.hat)
      v1.hat = rep(v1.hat.short, times=na1)

      # v2.hat is best reply q for player 2
      q2.hat = matrix(q2.hat,na2, na1)
      v2.hat.short = colMaxs(q2.hat)
      v2.hat = rep(v2.hat.short, each=na2)

      # Compute which action profiles are implementable
      IC.holds = U.hat+tol >= v1.hat + v2.hat

      # Can at least one action profile be implemented?

      if (sum(IC.holds)==0) {
        # Maybe just return empty RNE
        # instead
        stop(paste0("In state ", x, " period ", t," no pure action profile can satisfy the incentive constraint. Thus no pure RNE exists in the capped game."))
      }

      dU = max(U.hat[IC.holds])
      dv1 = min(v1.hat[IC.holds])
      dv2 = min(v2.hat[IC.holds])

      # 2. Solve static stage

      # Action lists of the repeated game
      s.li = g$static.rep.li[[row]]

      # Available liquidity after static stage
      L.av = 1/(1-delta)*(dU-dv1-dv2)

      # Use previously computed list
      # of candidates for optimal profiles

      rows = which(L.av - s.li$ae.df$L >= -tol)
      if (length(rows)==0) stop(paste0("No incentive compatible pure static action profile exists in period ",t))
      s.e = s.li$ae.df[rows[1],]

      rows = which(L.av - s.li$a1.df$L >= -tol)
      if (length(rows)==0) stop(paste0("No incentive compatible pure static action profile exists in period ",t))
      s.1 = s.li$a1.df[rows[1],]

      rows = which(L.av - s.li$a2.df$L >= -tol)
      if (length(rows)==0) stop(paste0("No incentive compatible pure static action profile exists in period ",t))
      s.2 = s.li$a2.df[rows[1],]

      U = (1-delta)*s.e$G+dU
      v1 = (1-delta)*s.1$c1 + dv1
      v2 = (1-delta)*s.2$c2 + dv2

      r1 = v1 + beta1*(U-v1-v2)
      r2 = v2 + (1-beta1)*(U-v1-v2)


      rne_U[row] = U;
      rne_v1[row] = v1; rne_v2[row] = v2
      rne_r1[row] = r1; rne_r2[row] = r2

      # Find actions
      if (iter == T | save.history) {
        d.a = r_rne_find_actions(U,v1,v2,U.hat,v1.hat,v2.hat, IC.holds, next_r1, next_r2, trans.mat, dest.rows, tie.breaking, tol=1e-12)
        rne_actions[row,] = c(s.e$.a,s.1$.a,s.2$.a, d.a)
      }

      if (save.details & iter==T) {
        pi1 = sdf$pi1[[row]]
        Er1 = trans.mat.mult(trans.mat, next_r1[dest.rows])
        # Continuation payoff if new negotiation in next period
        u1_neg = (1-delta)*pi1 + delta*Er1

        pi2 = sdf$pi2[[row]]
        Er2 = trans.mat.mult(trans.mat, next_r2[dest.rows])
        # Continuation payoff if new negotiation in next period
        u2_neg = (1-delta)*pi2 + delta*Er2

        slack = U.hat - (v1.hat + v2.hat)

        arows = seq_along(IC.holds)
        details.li[[row]] = cbind(
          x.df[x.df$x==x,],
          sdf$a.grid[[row]],
          quick_df(
            d.can.ae = (abs(U.hat-dU)<tol & IC.holds)*1 + (arows==d.a[1]),
            d.can.a1 = (abs(v1.hat-dv1)<tol & IC.holds)*1 + (arows==d.a[2]),
            d.can.a2 = (abs(v2.hat-dv2)<tol & IC.holds)*1 + (arows==d.a[3]),
            IC.holds=IC.holds,
            slack=slack,

            pi1 = pi1,
            Er1 = Er1,
            u1_neg = u1_neg,

            pi2 = pi2,
            Er2 = Er2,
            u2_neg = u2_neg,

            r1=r1,
            r2=r2,

            U.hat = U.hat,
            v1.hat=v1.hat,
            v2.hat=v2.hat,
            U=U,
            v1=v1,
            v2=v2
          )
        )
      }

    }
    if (save.history) {
      res_rne = cbind(quick_df(t=T-iter+1,x=sdf$x,r1=rne_r1,r2=rne_r2, U=rne_U, v1=rne_v1,v2=rne_v2),rne_actions)
      res_rne = add.rne.action.labels(g,res_rne)
      history.li[[iter]] = res_rne
    }

    if (iter < T) {
      next_U = rne_U
      next_v1 = rne_v1; next_v2 = rne_v2
      next_r1 = rne_r1; next_r2 = rne_r2
    }
  }
  res_rne = cbind(quick_df(x=sdf$x,r1=rne_r1,r2=rne_r2, U=rne_U, v1=rne_v1,v2=rne$v2),rne_actions)
  res_rne = add.rne.action.labels(g,res_rne)

  details = if (save.details) bind_rows(details.li)
  history = if (save.history) bind_rows(history.li)

  return(list(rne=res_rne, details=details, history=history))


}



#' Solve an RNE for a capped version of a multistage game
rel.capped.rne.multistage.old = function(g,T, save.details=FALSE, tol=1e-10,  delta=g$param$delta, rho=g$param$rho, res.field="rne", tie.breaking=c("slack","random","first","last")[1], add=TRUE, keep.all.t=FALSE) {
  restore.point("rel.capped.rne.multistage")
  if (!g$is_compiled) g = rel_compile(g)

  if (add) {
    pinfo = g$prev.capped.rne.info
    if (!is.null(pinfo)) {
      #if (pinfo$delta)
    }
  }

  g$param$delta = delta
  g$param$rho = rho

  sdf = g$sdf
  adj_delta = (1-rho)*delta
  beta1 = g$param$beta1

  if (save.details) {
    x.df = non.null(g$x.df, quick_df(x=sdf$x))
  }

  # Use vectors for higher speed
  rne.x = rep(sdf$x,times=T)
  rne.t=rep(T:1,each=NROW(sdf))
  n = length(rne.x)
  rne.r1 = rep(NA_real_,n)
  rne.r2 = rep(NA_real_,n)
  rne.U = rep(NA_real_,n)
  rne.v1 = rep(NA_real_,n)
  rne.v2 = rep(NA_real_,n)

  rne.s.ae = rep(NA_integer_,n)
  rne.s.a1 = rep(NA_integer_,n)
  rne.s.a2 = rep(NA_integer_,n)

  rne.d.ae = rep(NA_integer_,n)
  rne.d.a1 = rep(NA_integer_,n)
  rne.d.a2 = rep(NA_integer_,n)


  rne.details = NULL
  if (save.details)
    rne.details = vector("list",NROW(rne))

  # First solve repeated games for all states
  # These are the continuation payoffs in state T
  rows = 1:NROW(sdf)
  for (row in rows) {
    if (is.null(sdf$rep[[row]])) {
      sdf$rep[[row]] = solve.x.rep.multistage(g,row=row)
    }

    # Compute U, v, r
    rep = sdf$rep[[row]] %>%
      filter(adj_delta >= delta_min, adj_delta < delta_max)

    rne.U[row] = rep[1,"U"]
    rne.r1[row] = rep[1,"r1"]
    rne.r2[row] = rep[1,"r2"]
    rne.s.ae[row] = rep[1,"s.ae"]
    rne.s.a1[row] = rep[1,"s.a1"]
    rne.s.a2[row] = rep[1,"s.a2"]
    rne.d.ae[row] = rep[1,"d.ae"]
    rne.d.a1[row] = rep[1,"d.a1"]
    rne.d.a2[row] = rep[1,"d.a2"]

    w = ((1-delta) / (1-adj_delta))
    v1 = w*rep$v1_rep + (1-w)*rep$r1
    v2 = w*rep$v2_rep + (1-w)*rep$r2
    rne.v1[row] = v1
    rne.v2[row] = v2
  }

  g$sdf = sdf


  t = T-1
  srow = 1
  # Compute all remaining periods
  for (t in rev(seq_len(T-1))) {
    for (srow in 1:NROW(sdf)) {
      row = srow + (T-t)*NROW(sdf)
      x = sdf$x[srow]

      # 1. Solve dynamic stage

      na1 = sdf$na1[srow]
      na2 = sdf$na2[srow]
      trans.mat = sdf$trans.mat[[srow]]
      #rownames(trans.mat) = make.state.lab.a(sdf[srow,])

      if (NROW(trans.mat)==0) {
        trans.mat = matrix(1,na1*na2,1)
        colnames(trans.mat) = x
      }

      xd = colnames(trans.mat)

      dest.rows = match(xd, sdf$x) + (T-(t+1))*NROW(sdf)


      # Include code to compute U v and r for the current state
      U.hat = (1-delta)*(sdf$pi1[[srow]] + sdf$pi2[[srow]]) +
        delta * (trans.mat %*% rne.U[dest.rows])
      U.hat = as.vector(U.hat)

      # "q-value" of punishment payoffs
      q1.hat = (1-delta)*sdf$pi1[[srow]] +
        delta * (trans.mat %*% ( (1-rho)*rne.v1[dest.rows] + rho*rne.r1[dest.rows] ))

      q2.hat = (1-delta)*sdf$pi2[[srow]] +
        delta * (trans.mat %*% ( (1-rho)*rne.v2[dest.rows] + rho*rne.r2[dest.rows] ))


      # v1.hat is best reply q for player 1
      # Note player 1 is col player
      q1.hat = matrix(q1.hat,na2, na1)
      v1.hat.short = rowMaxs(q1.hat)
      v1.hat = rep(v1.hat.short, times=na1)


      # v2.hat is best reply q for player 2
      q2.hat = matrix(q2.hat,na2, na1)
      v2.hat.short = colMaxs(q2.hat)
      v2.hat = rep(v2.hat.short, each=na2)

      # Compute which action profiles are implementable
      IC.holds = U.hat+tol >= v1.hat + v2.hat

      # Can at least one action profile be implemented?

      if (sum(IC.holds)==0) {
        # Maybe just return empty RNE
        # instead
        stop(paste0("In state ", x, " period ", t," no pure action profile can satisfy the incentive constraint. Thus no pure RNE exists in the capped game."))
      }

      U = max(U.hat[IC.holds])
      v1 = min(v1.hat[IC.holds])
      v2 = min(v2.hat[IC.holds])

      # Pick dynamic equilibrium actions
      # using the specified tie.breaking rule
      slack = U.hat - (v1.hat + v2.hat)
      if (tie.breaking=="slack") {
        tb = slack
        const = 1
      } else if (tie.breaking=="last") {
        tb = seq_len(NROW(U.hat))
        const = 1
      } else if (tie.breaking=="first") {
        tb = rev(seq_len(NROW(U.hat)))
        const=1
      } else {
        const = 1
        tb = runif(NROW(U.hat))
        #restore.point("hdfhdf")
        #if (t==1 & x=="0 0") stop()
        const = 1
      }

      rne.d.ae[row] = which.max((const+tb) * (abs(U.hat-U)<tol & IC.holds))
      rne.d.a1[row] = which.max((const+tb) * (abs(v1.hat-v1)<tol & IC.holds))
      rne.d.a2[row] = which.max((const+tb) * (abs(v2.hat-v2)<tol & IC.holds))

      # 2. Solve static stage

      # Action lists of the repeated game
      s.li = g$static.rep.li[[srow]]

      dU = U; dv1=v1; dv2=v2
      # Available liquidity after static stage
      # TO DO: Check formula
      L.av = 1/(1-delta)*(dU-dv1-dv2)

      # Use previously computed list
      # of candidates for optimal profiles
      #
      # Filter takes too long
      #s.e = filter(s.li$ae.df,L.av-L >= -tol)[1,]
      #s.1 = filter(s.li$a1.df,L.av-L >= -tol)[1,]
      #s.2 = filter(s.li$a2.df,L.av-L >= -tol)[1,]

      rows = which(L.av - s.li$ae.df$L >= -tol)
      if (length(rows)==0) stop(paste0("No incentive compatible pure static action profile exists in period ",t))
      s.e = s.li$ae.df[rows[1],]

      rows = which(L.av - s.li$a1.df$L >= -tol)
      if (length(rows)==0) stop(paste0("No incentive compatible pure static action profile exists in period ",t))
      s.1 = s.li$a1.df[rows[1],]

      rows = which(L.av - s.li$a2.df$L >= -tol)
      if (length(rows)==0) stop(paste0("No incentive compatible pure static action profile exists in period ",t))
      s.2 = s.li$a2.df[rows[1],]



      U = (1-delta)*s.e$G+dU
      v1 = (1-delta)*s.1$c1 + dv1
      v2 = (1-delta)*s.2$c2 + dv2

      r1 = v1 + beta1*(U-v1-v2)
      r2 = v2 + (1-beta1)*(U-v1-v2)


      rne.U[row] = U;
      rne.v1[row] = v1; rne.v2[row] = v2
      rne.r1[row] = r1; rne.r2[row] = r2
      rne.s.ae[row] = s.e$.a
      rne.s.a1[row] = s.1$.a
      rne.s.a2[row] = s.2$.a




      # Save only details about dynamic stage
      if (save.details) {
        pi1 = sdf$pi1[[srow]]
        Er1 = as.vector(trans.mat %*% (rne.r1[dest.rows]))
        # Continuation payoff if new negotiation in next period
        u1_neg = (1-delta)*pi1 + delta*Er1

        pi2 = sdf$pi2[[srow]]
        Er2 = as.vector(trans.mat %*% (rne.r2[dest.rows]))
        # Continuation payoff if new negotiation in next period
        u2_neg = (1-delta)*pi2 + delta*Er2


        arows = seq_along(IC.holds)
        a.info = cbind(
          quick_df(t=t),
          x.df[x.df$x==x,],
          sdf$a.grid[[srow]],
          quick_df(
            d.can.ae = (abs(U.hat-dU)<tol & IC.holds)*1 + (arows==rne.d.ae[row]),
            d.can.a1 = (abs(v1.hat-dv1)<tol & IC.holds)*1 + (arows==rne.d.a1[row]),
            d.can.a2 = (abs(v2.hat-dv2)<tol & IC.holds)*1 + (arows==rne.d.a2[row]),
            IC.holds=IC.holds,
            slack=slack,

            pi1 = pi1,
            Er1 = Er1,
            u1_neg = u1_neg,

            pi2 = pi2,
            Er2 = Er2,
            u2_neg = u2_neg,

            r1=r1,
            r2=r2,

            U.hat = U.hat,
            v1.hat=v1.hat,
            v2.hat=v2.hat,
            U=U,
            v1=v1,
            v2=v2
          )
        )
        rne.details[[row]] = a.info
      }

    }
  }
  rne = quick_df(
    x = rne.x,
    t = rne.t,
    r1 = rne.r1,
    r2 = rne.r2,
    U=rne.U,
    v1=rne.v1,
    v2=rne.v2,

    s.ae=rne.s.ae,
    s.a1=rne.s.a1,
    s.a2=rne.s.a2,

    d.ae=rne.d.ae,
    d.a1=rne.d.a1,
    d.a2=rne.d.a2
  )

  # Add some additional info

  rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","d.ae"), cols2=c("x","a"))
  d.lab = g$a.labs.df$lab[rows]
  rows = match.by.cols(rne,g$gs$a.labs.df, cols1=c("x","s.ae"), cols2=c("x","a"))
  s.lab = g$gs$a.labs.df$lab[rows]
  rne$ae.lab = paste0(s.lab," | ", d.lab)

  rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","d.a1"), cols2=c("x","a"))
  d.lab = g$a.labs.df$lab[rows]
  rows = match.by.cols(rne,g$gs$a.labs.df, cols1=c("x","s.a1"), cols2=c("x","a"))
  s.lab = g$gs$a.labs.df$lab[rows]
  rne$a1.lab = paste0(s.lab," | ", d.lab)

  rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","d.a2"), cols2=c("x","a"))
  d.lab = g$a.labs.df$lab[rows]
  rows = match.by.cols(rne,g$gs$a.labs.df, cols1=c("x","s.a2"), cols2=c("x","a"))
  s.lab = g$gs$a.labs.df$lab[rows]
  rne$a2.lab = paste0(s.lab," | ", d.lab)



  if (!is.null(g$x.df))
    rne = left_join(rne, g$x.df, by="x")

  g$sdf = sdf
  g[[res.field]] = rne
  g[[paste0(res.field,".details")]] = rne.details
  g
}


