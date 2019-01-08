examples.rne_capped = function() {
  e.seq = seq(0,1, by=0.1); xL=0; xH=0.2
  g = rel_game("Weakly Monotone Vulnerability Paradox") %>%
    # Initial State
    rel_state("xL", A1=list(move=c("stay","vul")),A2=list(e=e.seq)) %>%
    rel_payoff("xL",pi1=~e, pi2=~ -0.5*e*e*(e>=0)) %>%
    rel_transition("xL","xH",move="vul") %>%
    # High vulnerability
    rel_state("xH", A1=NULL,A2=list(e=unique(c(-xH,e.seq)))) %>%
    rel_payoff("xH",pi1=~e, pi2=~ -0.5*e*e*(e>=0)) %>%
    rel_compile()

  g = rel_rne(g, delta=0.9, rho=0.9)
  (rne=g$rne)


  g = rel_capped_rne(g,T=100, delta=0.9, rho=0.9,use.cpp=!TRUE)
  (rne=g$rne)

}

arms.race.example = function() {

  A.fun = function(x1,x2, x.max,...) {
    restore.point("A.fun")
    list(
      A1=list(h1=0:x1, i1=c(if (x1>0) "d","",if (x1<x.max) "b")),
      A2=list(h2=0:x2, i2=c(if (x2>0) "d","",if (x2<x.max) "b"))
    )
  }

  pi.fun = function(a.df,c,k,...) {
    restore.point("pi.fun")
    transmute(a.df,
      pi1 = -c*h1 - h2 - k*(i1=="b"),
      pi2 = -c*h2 - h1 - k*(i2=="b")
    )
  }

  trans.fun = function(x,x1,x2,a.df,x.max, success.prob,...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()
    a.df = unique(select(a.df, i1,i2))
    sp = success.prob

    trans = rbind(
      # (1,0)
      mutate(a.df, g1=1,g2=0,
        prob= (i1=="b")*sp* ( (i2=="b")*(1-sp)+(i2==""))),
      # (0,1)
      mutate(a.df, g1=0,g2=1,
        prob= (i2=="b")*sp* ( (i1=="b")*(1-sp)+(i1==""))),
      # (1,1)
      mutate(a.df, g1=1,g2=1,
        prob= (i1=="b")*sp*(i2=="b")*sp),
      # (-1,0)
      mutate(a.df, g1=-1,g2=0,
        prob= (i1=="d")*( (i2=="b")*(1-sp)+(i2==""))),
      # (0,-1)
      mutate(a.df, g1=0,g2=-1,
        prob= (i2=="d")*( (i1=="b")*(1-sp)+(i1==""))),
      # (-1,-1)
      mutate(a.df, g1=-1,g2=-1,
        prob= (i1=="d")*(i2=="d")),
      # (-1,1)
      mutate(a.df, g1=-1,g2=1,
        prob= (i1=="d")*( (i2=="b")*sp)),
      # (1,-1)
      mutate(a.df, g1=1,g2=-1,
        prob= (i2=="d")*( (i1=="b")*sp))
    ) %>%
      filter(prob > 0 )

    trans = mutate(trans,
        nx1 = pmin(x.max,pmax(x1+g1,0)),
        nx2 = pmin(x.max,pmax(x2+g2,0)),
        xd = paste0(nx1,"_",nx2),
        xs=x
      ) %>%
      filter(xs != xd) %>%
      select(xs,xd,i1,i2,prob)
    trans
  }


  x.max = 2
  x.df = as_data_frame(expand.grid(x1=0:x.max,x2=0:x.max))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)

  g = rel_game("Arms Race") %>%
    rel_param(delta=0.995, rho=0.4, c=0, k=2,x.max=x.max, success.prob=1) %>%
    rel_states_fun(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun) %>%
    rel_compile() %>%
    rel_capped_rne(T=100)

  rne.diagram(g,t=98)

  g=g %>% rel_capped_rne(T=50, save.details = TRUE)



  #rne = g$rne %>% filter(t<max(g$rne$t), t==1)
  #rne

  de = get.rne.details(g)
  d = filter(de, t==1, x=="0_0")


  d = rne %>%
    filter(can.ae==2) %>%
    mutate(iv1 =  0 + (i1=="b") - (i1=="d"),iv2 = 0 + (i2=="b") - (i2=="d"))

  library(ggplot2)
  ggplot(d, aes(x=t,y=iv1)) + geom_point(size=1.5, color="red", alpha=0.5) + facet_grid(x1~x2, labeller=label_both) + geom_point(aes(x=t,y=iv2), size=1.5, color="blue", alpha=0.5) + theme_bw()

  de = rne %>% filter(x %in% c("3_0"), t==5) %>%
    filter(h1==0, h2==0)
  de


  tdf = g$tdf

  View(rne)
}



#' Solve an RNE for a capped version of a multistage game
rel_capped_rne = function(g,T, tol=1e-10,  delta=g$param$delta, rho=g$param$rho, adjusted.delta=NULL, res.field="rne", tie.breaking=c("equal_r", "slack","random","first","last","max_r1","max_r2")[1], use.cpp=TRUE, save.details=FALSE, add.iterations=FALSE) {
  restore.point("rel_capped_rne")
  if (!g$is_compiled) g = rel_compile(g)


  res = compute.delta.rho(delta, rho, adjusted.delta)
  g$param$delta = res$delta
  g$param$rho = res$rho

  if (use.cpp) {
    if (!require(RelationalContractsCpp)) {
      warning("Cannot use faster C++ functions since the package RelationalContractsCpp is not installed.")
      use.cpp = FALSE
    }
  }


  # Solve repeated games for all states
  g = rel_solve_all_repeated_games(g)

  sdf = g$sdf
  adj_delta = (1-rho)*delta
  beta1 = g$param$beta1
  beta2 = 1-beta1

  if (add.iterations) {
    rne = g[[res.field]]
    if (is.null(rne)) add.iterations=FALSE
  }
  if (!add.iterations) {
    rne = capped.rne.rep.period(g,delta=delta, rho=rho)
    T = T-1
  }

  if (!isTRUE(g$is.multi.stage)) {
    res = capped.rne.iterations(g,T, rne=rne,save.details=save.details, use.cpp=use.cpp,tie.breaking = tie.breaking,tol = tol)
  } else {
    res = capped.rne.mutltistage.iterations(g,T, rne=rne,save.details=save.details, use.cpp=use.cpp,tie.breaking = tie.breaking,tol = tol)
  }
  rne = res$rne
  details = res$details

  rne = add.rne.action.labels(g,rne)
  if (!is.null(g$x.df))
    rne = left_join(rne, g$x.df, by="x")

  g$sdf = sdf
  g[[res.field]] = rne
  g[[paste0(res.field,".details")]] = details
  g
}

capped.rne.iterations = function(g,T=1,rne=g$rne, tie.breaking, debug_row=-1, tol=1e-12, use.cpp=TRUE, save.details=FALSE) {
  restore.point("capped.rne.iterations")

  if (T<=0) {
    return(list(rne=rne,details=NULL))
  }

  # TO DO: Compile transmats before in a useful form
  sdf = g$sdf
  if (use.cpp) {
    transmats = lapply(1:NROW(sdf), function(row) {
      trans.mat = sdf$trans.mat[[row]]
      if (is.null(trans.mat)) {
        x = sdf$x[row]; na1 = sdf$na1[row]; na2 = sdf$na2[row]
        trans.mat = matrix(1,na1*na2,1)
        colnames(trans.mat) = x
      }
      trans.mat
    })

    T.cpp = T-save.details

    if (T.cpp > 0) {
      res_rne = cpp_capped_rne_iterations(T=T.cpp, sdf=sdf,rne=rne,transmats=transmats,
        delta=g$param$delta, rho=g$param$rho,beta1 = g$param$beta1,
        tie_breaking=tie.breaking, tol=tol, debug_row=debug_row)
    }
    if (save.details) {
      res = r.capped.rne.iterations(T=1, g=g, rne=rne, tie.breaking=tie.breaking, save.details=save.details, tol=tol)
    } else {
      res = list(rne=res_rne, details=NULL)
    }
  } else {
    res = r.capped.rne.iterations(T=T, g=g, rne=rne, tie.breaking=tie.breaking, save.details=save.details)
  }
  res
}

# Iterate capped RNE over T periods using pure R
# Return res_rne
r.capped.rne.iterations = function(T, g, rne, tie.breaking, use.final=!is.null(g$final.tdf),delta=g$param$delta, rho=g$param$rho,beta1 = g$param$beta1, tol=1e-12, save.details=FALSE) {
  restore.point("r.capped.rne.iterations")

  sdf = g$sdf

  next_U = rne_U = rne$U
  next_v1 = rne_v1 = rne$v1; next_v2 = rne_v2 = rne$v2
  next_r1 = rne_r1 = rne$r1; next_r2 = rne_r2 = rne$r2

  rne_actions = matrix(0L, NROW(sdf),3)
  colnames(rne_actions) = c("ae","a1","a2")

  if (save.details) {
    details.li = vector("list",NROW(sdf))
    x.df = g$x.df
  }

  iter=1;row=2;
  # Compute all remaining periods
  for (iter in seq_len(T)) {
    for (row in 1:NROW(sdf)) {
      x = sdf$x[row]

      na1 = sdf$na1[row]
      na2 = sdf$na2[row]
      if (use.final & iter==1) {
        trans.mat = sdf$final.trans.mat[[row]]
      } else {
        trans.mat = sdf$trans.mat[[row]]
      }
      if (is.null(trans.mat)) {
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

      U = max(U.hat[IC.holds])
      v1 = min(v1.hat[IC.holds])
      v2 = min(v2.hat[IC.holds])

      r1 = v1 + beta1*(U-v1-v2)
      r2 = v2 + (1-beta1)*(U-v1-v2)


      rne_U[row] = U;
      rne_v1[row] = v1; rne_v2[row] = v2
      rne_r1[row] = r1; rne_r2[row] = r2

      # Find actions
      if (iter == T) {
        rne_actions[row,] = r_rne_find_actions(U,v1,v2,U.hat,v1.hat,v2.hat, IC.holds, next_r1, next_r2, trans.mat, dest.rows, tie.breaking, tol=1e-12)
      }

      if (save.details & iter==T) {
        restore.point("capped.iterations.save.details")
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
            can.ae = (abs(U.hat-U)<tol & IC.holds)*1 + (arows==rne_actions[row,1]),
            can.a1 = (abs(v1.hat-v1)<tol & IC.holds)*1 + (arows==rne_actions[row,2]),
            can.a2 = (abs(v2.hat-v2)<tol & IC.holds)*1 + (arows==rne_actions[row,3]),
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
    if (iter < T) {
      next_U = rne_U
      next_v1 = rne_v1; next_v2 = rne_v2
      next_r1 = rne_r1; next_r2 = rne_r2
    }
  }
  res_rne = cbind(quick_df(x=sdf$x,r1=rne_r1,r2=rne_r2, U=rne_U, v1=rne_v1,v2=rne$v2),rne_actions)
  res_rne = add.rne.action.labels(g,res_rne)

  if (save.details) {
    details = bind_rows(details.li)
    return(list(rne=res_rne, details=details))
  } else {
    return(list(rne=res_rne, details=NULL))
  }

}



# Solve the last period where all stages remain a repeated game
capped.rne.rep.period = function(g, delta=g$param$delta, rho=g$param$rho) {
  restore.point("capped.rne.rep.period")

  if (is.null(g$rep.games.df))
    stop("Please first call rel_solve_all_repeated_games to solve the repeated games for all states, assuming the state stays fixed.")
  adj_delta = delta*(1-rho)

  w = ((1-delta) / (1-adj_delta))
  res = g$rep.games.df %>%
    filter(adj_delta >= delta_min, adj_delta < delta_max) %>%
    mutate(
      v1 = w*v1_rep + (1-w)*r1,
      v2 = w*v2_rep + (1-w)*r2
    )
  if (isTRUE(g$is.multi.stage)) {
    cols = c("x","r1","r2","U","v1","v2","s.ae","s.a1","s.a2","d.ae","d.a1","d.a2")
  } else {
    cols = c("x","r1","r2","U","v1","v2","ae","a1","a2")
  }
  res[,cols,drop=FALSE]

}

#' Solve an RNE for a capped version of the game
#'
#' Once the capped version of the game reaches period T,
#' the state cannot change anymore.
#' We can solve such capped games via a fast backward induction
#' algorithm. There always exists a unique RNE payoff.
#'
#' @param g The game object
#' @param T The number of periods until states can change
#' @param save.details If yes, detailed information about the equilibrium for each state and period will be stored in g and can be retrieved via the function get.rne.details
rel_capped_rne_old = function(g,T, save.details=FALSE, tol=1e-10,  delta=g$param$delta, rho=g$param$rho, adjusted.delta=NULL, res.field="rne", tie.breaking=c("slack","random","first","last")[1], use.cpp=TRUE) {
  restore.point("rel_capped_rne_old")
  if (!g$is_compiled) g = rel_compile(g)

  res = compute.delta.rho(delta, rho, adjusted.delta)
  g$param$delta = res$delta
  g$param$rho = res$rho

  if (isTRUE(g$is.multi.stage)) {
    g = capped.rne.multistage(g,T, tol=tol, res.field=res.field, tie.breaking=tie.breaking, use.cpp=use.cpp, save.details=save.details)
    return(g)
  }


  sdf = g$sdf
  adj_delta = (1-rho)*delta
  beta1 = g$param$beta1
  beta2 = 1-beta1

  if (save.details) {
    x.df = non.null(g$x.df, quick_df(x=sdf$x))
  }


  rne = data_frame(x = rep(sdf$x,times=T),t=rep(T:1,each=NROW(sdf)), r1=NA,r2=NA, U=NA, v1=NA,v2=NA,ae=NA,a1=NA,a2=NA)


  rne.details = NULL
  if (save.details)
    rne.details = vector("list",NROW(rne))

  # First solve repeated games for all states
  # These are the continuation payoffs in state T
  rows = 1:NROW(sdf)
  for (row in rows) {
    if (is.null(sdf$rep[[row]])) {
      sdf$rep[[row]] = solve.x.repgame(g,row=row)
    }

    # Compute U, v, r
    rep = sdf$rep[[row]] %>%
      filter(adj_delta >= delta_min, adj_delta < delta_max)


    rne$U[row] = rep$U
    rne$r1[row] = rep$r1
    rne$r2[row] = rep$r2
    rne$ae[row] = rep$ae
    rne$a1[row] = rep$a1
    rne$a2[row] = rep$a2



    w = ((1-delta) / (1-adj_delta))
    v1 = w*rep$v1_rep + (1-w)*rep$r1
    v2 = w*rep$v2_rep + (1-w)*rep$r2
    rne$v1[row] = v1
    rne$v2[row] = v2
  }
  rne

  g$sdf = sdf


  t = T-1
  srow = 1
  # Compute all remaining periods
  for (t in rev(seq_len(T-1))) {
    is.final = (!is.null(g$final.tdf)) & (t == T-1)
    for (srow in 1:NROW(sdf)) {
      x = sdf$x[srow]

      na1 = sdf$na1[srow]
      na2 = sdf$na2[srow]
      if (!is.final) {
        trans.mat = sdf$trans.mat[[srow]]
      } else {
        trans.mat = sdf$final.trans.mat[[srow]]
      }
        #rownames(trans.mat) = make.state.lab.a(sdf[srow,])

      if (is.null(trans.mat)) {
        trans.mat = matrix(1,na1*na2,1)
        colnames(trans.mat) = x
      }

      xd = colnames(trans.mat)

      dest.rows = match(xd, sdf$x) + (T-(t+1))*NROW(sdf)


      # Include code to compute U v and r for the current state
      U.hat = (1-delta)*(sdf$pi1[[srow]] + sdf$pi2[[srow]]) +
        delta * (trans.mat %*% rne$U[dest.rows])
      U.hat = as.vector(U.hat)

      # "q-value" of punishment payoffs
      q1.hat = (1-delta)*sdf$pi1[[srow]] +
        delta * (trans.mat %*% ( (1-rho)*rne$v1[dest.rows] + rho*rne$r1[dest.rows] ))

      q2.hat = (1-delta)*sdf$pi2[[srow]] +
        delta * (trans.mat %*% ( (1-rho)*rne$v2[dest.rows] + rho*rne$r2[dest.rows] ))


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

      r1 = v1 + beta1*(U-v1-v2)
      r2 = v2 + beta2*(U-v1-v2)

      row = srow + (T-t)*NROW(sdf)

      rne$U[row] = U;
      rne$v1[row] = v1; rne$v2[row] = v2
      rne$r1[row] = r1; rne$r2[row] = r2

      # Pick equilibrium actions
      # If indifferent choose the one with the largest
      # slack in the IC


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

      rne$ae[row] = which.max((const+tb) * (abs(U.hat-U)<tol & IC.holds))
      rne$a1[row] = which.max((const+tb) * (abs(v1.hat-v1)<tol & IC.holds))
      rne$a2[row] = which.max((const+tb) * (abs(v2.hat-v2)<tol & IC.holds))


      if (save.details) {
        pi1 = sdf$pi1[[srow]]
        Er1 = as.vector(trans.mat %*% (rne$r1[dest.rows]))
        # Continuation payoff if new negotiation in next period
        u1_neg = (1-delta)*pi1 + delta*Er1

        pi2 = sdf$pi2[[srow]]
        Er2 = as.vector(trans.mat %*% (rne$r2[dest.rows]))
        # Continuation payoff if new negotiation in next period
        u2_neg = (1-delta)*pi2 + delta*Er2


        arows = seq_along(IC.holds)
        a.info = cbind(
          quick_df(t=t),
          x.df[x.df$x==x,],
          sdf$a.grid[[srow]],
          quick_df(
            can.ae = (abs(U.hat-U)<tol & IC.holds)*1 + (arows==rne$ae[row]),
            can.a1 = (abs(v1.hat-v1)<tol & IC.holds)*1 + (arows==rne$a1[row]),
            can.a2 = (abs(v2.hat-v2)<tol & IC.holds)*1 + (arows==rne$a2[row]),
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

  # Add some additional info

  rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","ae"), cols2=c("x","a"))
  rne$ae.lab = g$a.labs.df$lab[rows]

  rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","a1"), cols2=c("x","a"))
  rne$a1.lab = g$a.labs.df$lab[rows]

  rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","a2"), cols2=c("x","a"))
  rne$a2.lab = g$a.labs.df$lab[rows]

  if (!is.null(g$x.df))
    rne = left_join(rne, g$x.df, by="x")

    #rne$ae.lab = left_join(select(rne,x,a=ae), g$a.labs.df, by=c("x","a"))$lab
    #rne$a1.lab = left_join(select(rne,x,a=a1), g$a.labs.df, by=c("x","a"))$lab
    #rne$a2.lab = left_join(select(rne,x,a=a2), g$a.labs.df, by=c("x","a"))$lab

  g$sdf = sdf
  g[[res.field]] = rne
  g[[paste0(res.field,".details")]] = rne.details
  g
}

