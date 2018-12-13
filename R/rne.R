example.rne = function() {
  e.seq = c(0,1); xL=0; xH=5;
  g = rel_game("Simple Vulnerability Paradox") %>%
    rel_param(delta=0.8, rho=0.5, c=0.4, xL=xL,xH=xH) %>%
    # Initial State
    rel_state("x0", A1=c("to_xL","to_xH"),A2=list(e=e.seq)) %>%
    rel_payoff("x0",pi1=0,pi2=0) %>%
    rel_transition("x0",c("xL","xH"),a1=c("to_xL","to_xH")) %>%
    # Low vulnerability
    rel_state("xL", A2=list(e=unique(c(-xL,e.seq)))) %>%
    rel_payoff("xL",pi1=~e, pi2=~ -c*e*(e>=0)) %>%
    # High vulnerability
    rel_state("xH", A1=NULL,A2=list(e=unique(c(-xH,e.seq)))) %>%
    rel_payoff("xH",pi1=~e, pi2=~ -c*e*(e>=0)) %>%
    #rel_transition("xH","x0", prob=1) %>%
    rel_compile() %>%
    rel_rne()

  compute.x.trans.mat("x0",g)
  compute.x.trans.mat("xL",g)

  g = rel_rne(g)

  rne = g$rne
  rne
  solve_x_repgame(g,"xL")



  e.seq = c(0,1); xL=0; xH=5;
  g = rel_game("Vulnerability Paradox") %>%
    rel_param(delta=0.8, rho=0.5, c=0.4, xL=xL,xH=xH) %>%
    # Low vulnerability
    rel_state("xL", A1=c(move="stay","vul"),A2=list(e=unique(c(-xL,e.seq)))) %>%
    rel_payoff("xL",pi1=~e, pi2=~ -c*e*(e>=0)) %>%
    rel_transition("xL","xH",move="vul") %>%
    # High vulnerability
    rel_state("xH", A1=NULL,A2=list(e=unique(c(-xH,e.seq)))) %>%
    rel_payoff("xH",pi1=~e, pi2=~ -c*e*(e>=0)) %>%
    #rel_transition("xH","x0", prob=1) %>%
    rel_compile() %>%
    rel_capped_rne(T=10)

  (rne = g$rne)


  g = rel_game("Blackmailing Game") %>%
    rel_param(delta=0.8, rho=0.5, c=0.4, xL=xL,xH=xH) %>%
    # x0
    rel_state("x0", A1=list(move=c("stay","reveal"))) %>%
    rel_payoff("x0",pi1=0, pi2=1) %>%
    rel_transition("x0","x1",move="reveal") %>%
    # x1
    rel_state("x1") %>%
    rel_payoff("x1",pi1=0, pi2=0) %>%
    rel_compile()

  g=g %>% rel_capped_rne(T=20)

  (rne = g$rne)

  library(ggplot2)
  ggplot(filter(rne,x=="x0"), aes(y=r1,x=t)) + geom_line()



}


arms.race.example = function() {

  A.fun = function(x.df, x.max,...) {
    restore.point("A.fun")
    x1=x.df$x1; x2=x.df$x2
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

  trans.fun = function(x, x.df,a.df,x.max, success.prob,...) {
    restore.point("trans.fun")
    x1=x.df$x1;x2=x.df$x2;
    a.df = unique(select(a.df, i1,i2))
    sp = success.prob
    res = mutate(a.df,
        prob = ifelse(i1=="b", sp,1)*ifelse(i2=="b",sp,1),
        iv1 = 0 + (i1=="b") - (i1=="d"),
        iv2 = 0 + (i2=="b") - (i2=="d"),
        nx1 = pmin(x.max,pmax(x1+iv1,0)),
        nx2 = pmin(x.max,pmax(x2+iv2,0)),
        xd = paste0(nx1,"_",nx2),
        xs=x
      ) %>%
      filter(xs != xd) %>%
      select(xs,xd,i1,i2,prob)
    res
  }


  x.max = 3
  x.df = as_data_frame(expand.grid(x1=0:x.max,x2=0:x.max))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)

  g = rel_game("Arms Race") %>%
    rel_param(delta=0.995, rho=0.4, c=0, k=2,x.max=x.max, success.prob=0.5) %>%
    rel_states_fun(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun) %>%
    rel_compile()

  g=g %>% rel_capped_rne(T=10, save.details = TRUE)

  #rne = g$rne %>% filter(t<max(g$rne$t), t==1)
  #rne

  res = get.rne.details(g)

  d = res %>%
    filter(can.ae==2) %>%
    mutate(iv1 =  0 + (i1=="b") - (i1=="d"),iv2 = 0 + (i2=="b") - (i2=="d"))

  library(ggplot2)
  ggplot(d, aes(x=t,y=iv1)) + geom_point(size=1.5, color="red", alpha=0.5) + facet_grid(x1~x2, labeller=label_both) + geom_point(aes(x=t,y=iv2), size=1.5, color="blue", alpha=0.5) + theme_bw()

  de = res %>% filter(x %in% c("3_0"), t==5) %>%
    filter(h1==0, h2==0)
  de


  tdf = g$tdf

  View(rne)
}



get.rne.details = function(g, x=NULL,t=NULL) {
  restore.point("get.rne.details")

  if (is.null(g$rne.details)) return(NULL)
  if (is.null(x)) x = g$sdf$x

  if (!is.null(t)) {
    rows = which(g$rne$x %in% x & g$rne$t %in% t)
  } else {
    rows = which(g$rne$x %in% x)
  }
  if (length(rows)==1) {
    g$rne.details[[rows]]
  } else {
    bind_rows(g$rne.details[rows])
  }

}

print.rne = function(g) {

}

rel_rne = function(g,...) {
  restore.point("rel_rne")
  if (!g$is_compiled) g = rel_compile(g)

  sdf = g$sdf
  delta = g$param$delta
  rho = g$param$rho
  adj_delta = (1-rho)*delta
  beta1 = g$param$beta1
  beta2 = 1-beta1


  res = data_frame(x = sdf$x, solved=FALSE, r1=NA,r2=NA, U=NA, v1=NA,v2=NA,ae=NA,a1=NA,a2=NA)




  # First solve repeated games for all terminal states
  rows = which(sdf$is_terminal)
  row = rows[1]
  for (row in rows) {

    if (is.null(sdf$rep[[row]])) {
      sdf$rep[[row]] = solve_x_repgame(g,state=sdf[row,])
    }

    # Compute U, v, r
    rep = sdf$rep[[row]] %>%
      filter(adj_delta >= delta_min, adj_delta < delta_max)


    res$U[row] = rep$U
    res$r1[row] = rep$r1
    res$r2[row] = rep$r2
    res$ae[row] = rep$ae
    res$a1[row] = rep$a1
    res$a2[row] = rep$a2

    w = ((1-delta) / (1-adj_delta))
    v1 = w*rep$v1_rep + (1-w)*rep$r1
    v2 = w*rep$v2_rep + (1-w)*rep$r2
    res$v1[row] = v1
    res$v2[row] = v2
  }
  res$solved[rows] = TRUE
  res

  # Compute state transitions for all remaining states
  for (row in which(!res$solved)) {
    if (is.null(sdf$trans.mat[[row]])) {
      x = sdf$x[row]
      sdf$trans.mat[row] = list(compute.x.trans.mat(x=x,g=g))
    }
    to.self = x %in% colnames(sdf$trans.mat[[row]])
    if (to.self)
      stop(paste0("The non-terminal state ", x, " has a positive probability (", max(trans.mat[,x]), ") to transit to itself. Cannot yet find RNE in such games."))
  }

  find.next.state.row = function() {
    x.solved = sdf$x[res$solved]
    rows = which(!res$solved)
    for (row in rows) {
      xd = colnames(sdf$trans.mat[[row]])
      if (all(xd %in% x.solved))
        return(row)
    }
    # No row could be found
    return(NA)
  }



  while(sum(!res$solved)>0) {
    row = find.next.state.row()
    if (is.na(row)) {
      x.unsolved = sdf$x[!res$solved]
      stop(paste0("Cannot compute RNE since there is a cycle among the non-terminal state(s) ", paste0(x.unsolved, collapse=", ")))
    }


    x = sdf$x[row]
    trans.mat = sdf$trans.mat[[row]]
    xd = colnames(trans.mat)
    dest.rows = match(xd, res$x)
    na1 = sdf$na1[row]
    na2 = sdf$na2[row]

    # Include code to compute U v and r for the current state
    U.hat = (1-delta)*(sdf$pi1[[row]] + sdf$pi2[[row]]) +
      delta * (trans.mat %*% res$U[dest.rows])

    # "q-value" of punishment payoffs
    q1.hat = (1-delta)*sdf$pi1[[row]] +
      delta * (trans.mat %*% ( (1-rho)*res$v1[dest.rows] + rho*res$r1[dest.rows] ))

    q2.hat = (1-delta)*sdf$pi2[[row]] +
      delta * (trans.mat %*% ( (1-rho)*res$v2[dest.rows] + rho*res$r2[dest.rows] ))

    # v1 is best reply q for player 1
    q1.hat = matrix(q1.hat,na1, sdf$na2)
    v1.hat.short = colMaxs(q1.hat)
    v1.hat = rep(v1.hat.short, each=na1)
    # Extend to all a action profiles


    # v2 is best reply q for player 2
    q2.hat = matrix(q2.hat,sdf$na1[row], sdf$na2[row])
    v2.hat.short = rowMaxs(q2.hat)
    v2.hat = rep(v2.hat.short, times=na2)


    # Compute which action profiles are implementable
    IC.holds = (U.hat >= v1.hat + v2.hat)

    # Can at least one action profile be implemented?
    if (sum(IC.holds)==0) {
      # Maybe just return empty RNE
      # instead
      stop(paste0("In state ", x, " no pure action profile can satisfy the incentive constraint. Thus no pure RNE exists."))
    }

    U = max(U.hat[IC.holds])
    v1 = min(v1.hat[IC.holds])
    v2 = min(v2.hat[IC.holds])

    r1 = v1 + beta1*(U-v1-v2)
    r2 = v2 + beta2*(U-v1-v2)

    res$U[row] = U;
    res$v1[row] = v1; res$v2[row] = v2
    res$r1[row] = r1; res$r2[row] = r2

    # Pick equilibrium actions
    # If indifferent choose the one with the largest
    # slack in the IC
    slack = U.hat - (v1.hat + v2.hat)
    res$ae[row] = which.max(slack * (U.hat==U & IC.holds))
    res$a1[row] = which.max(slack * (v1.hat==v1 & IC.holds))
    res$a2[row] = which.max(slack * (v2.hat==v2 & IC.holds))

    res$solved[row] = TRUE
  }

  # Add some additional info
  for (row in seq_len(NROW(res))) {
    res$ae.lab = left_join(select(res,x,a=ae), g$a.labs.df, by=c("x","a"))$lab
    res$a1.lab = left_join(select(res,x,a=a1), g$a.labs.df, by=c("x","a"))$lab
    res$a2.lab = left_join(select(res,x,a=a2), g$a.labs.df, by=c("x","a"))$lab
  }

  res = select(res,-solved)
  g$sdf = sdf
  g$rne = res
  g
}





compute.x.trans.mat = function(x,g, add.own=TRUE) {
  restore.point("compute.x.trans.mat")
  df = g$tdf[g$tdf$xs==x,]
  if (NROW(df)==0)
    return(NULL)

  xd = unique(df$xd)

  row = which(g$sdf$x == x)

  a.grid = g$sdf$a.grid[[row]]

  actions = intersect(colnames(a.grid), colnames(df))
  #actions = setdiff(actions, actions[is.na(df[1,actions])])

  # Transition matrix: rows action profiles, cols = destination cols
  mat = matrix(0,NROW(a.grid), length(xd))
  colnames(mat) = xd

  mat.cols = seq_along(xd)
  names(mat.cols) = xd

  inds = unique(df$.def.ind)
  ind = inds[1]
  for (ind in inds) {
    d = df[df$.def.ind == ind,,drop=FALSE]
    act = actions[as.vector(!is.na(d[1,actions]))]

    if (length(act)==0) {
      # Set same transition probability for all actions
      new.mat = matrix(d$prob, NROW(mat), NROW(d), byrow = TRUE)
      mat[, d$xd] = new.mat
    } else {
      # Transition probabilities depend on actions
      # Match rows from d to those of a.grid
      # based on actions in act
      mrows = match.by.cols(a.grid,d,cols=act)
      #mrows[2] = NA
      use = !is.na(mrows)
      use.mrows = mrows[use]
      grid = cbind(seq_along(mrows)[use],mat.cols[d$xd[use.mrows]])
      mat[grid] = d$prob[use.mrows]
    }

  }
  sumProbs = rowSums(mat)
  if (any(mat<0))
    stop(paste0("Have computed negative transition probabilities for state ", x))
  if (any(sumProbs>1))
    stop(paste0("Have computed sum of transition probabilities larger than 1 for state ",x))

  if (add.own) {
    if (any(sumProbs<1)) {
      mat = cbind(.own = 1-sumProbs, mat)
      colnames(mat)[1] = x
    }
  }
  mat
}




rel_capped_rne = function(g,T,..., save.details=FALSE) {
  restore.point("rel_capped_rne")
  if (!g$is_compiled) g = rel_compile(g)

  sdf = g$sdf
  delta = g$param$delta
  rho = g$param$rho
  adj_delta = (1-rho)*delta
  beta1 = g$param$beta1
  beta2 = 1-beta1

  if (save.details) {
    x.df = non.null(g$x.df, quick_df(x=sdf$x))
  }


  res = data_frame(x = rep(sdf$x,times=T),t=rep(T:1,each=NROW(sdf)), r1=NA,r2=NA, U=NA, v1=NA,v2=NA,ae=NA,a1=NA,a2=NA)


  rne.details = NULL
  if (save.details)
    rne.details = vector("list",NROW(res))

  a.res = data_frame(row=integer(0))

  # First solve repeated games for all states
  # These are the continuation payoffs in state T
  rows = 1:NROW(sdf)
  for (row in rows) {
    if (is.null(sdf$rep[[row]])) {
      sdf$rep[[row]] = solve_x_repgame(g,state=sdf[row,])
    }

    # Compute U, v, r
    rep = sdf$rep[[row]] %>%
      filter(adj_delta >= delta_min, adj_delta < delta_max)


    res$U[row] = rep$U
    res$r1[row] = rep$r1
    res$r2[row] = rep$r2
    res$ae[row] = rep$ae
    res$a1[row] = rep$a1
    res$a2[row] = rep$a2



    w = ((1-delta) / (1-adj_delta))
    v1 = w*rep$v1_rep + (1-w)*rep$r1
    v2 = w*rep$v2_rep + (1-w)*rep$r2
    res$v1[row] = v1
    res$v2[row] = v2
  }
  res

  # Compute all transition matrices
  for (row in 1:NROW(sdf)) {
    if (is.null(sdf$trans.mat[[row]])) {
      x = sdf$x[row]
      sdf$trans.mat[row] = list(compute.x.trans.mat(x=x,g=g))
    }
  }


  t = T-1
  srow = 1
  # Compute all remaining periods
  for (t in rev(seq_len(T-1))) {
    for (srow in 1:NROW(sdf)) {
      x = sdf$x[srow]

      na1 = sdf$na1[srow]
      na2 = sdf$na2[srow]
      trans.mat = sdf$trans.mat[[srow]]
      #rownames(trans.mat) = make.state.lab.a(sdf[srow,])

      if (is.null(trans.mat)) {
        trans.mat = matrix(1,na1*na2,1)
        colnames(trans.mat) = x
      }

      xd = colnames(trans.mat)

      dest.rows = match(xd, sdf$x) + (T-(t+1))*NROW(sdf)


      # Include code to compute U v and r for the current state
      U.hat = (1-delta)*(sdf$pi1[[srow]] + sdf$pi2[[srow]]) +
        delta * (trans.mat %*% res$U[dest.rows])
      U.hat = as.vector(U.hat)

      # "q-value" of punishment payoffs
      q1.hat = (1-delta)*sdf$pi1[[srow]] +
        delta * (trans.mat %*% ( (1-rho)*res$v1[dest.rows] + rho*res$r1[dest.rows] ))

      q2.hat = (1-delta)*sdf$pi2[[srow]] +
        delta * (trans.mat %*% ( (1-rho)*res$v2[dest.rows] + rho*res$r2[dest.rows] ))

      # v1 is best reply q for player 1
      q1.hat = matrix(q1.hat,na1, na2)
      v1.hat.short = colMaxs(q1.hat)
      v1.hat = rep(v1.hat.short, each=na1)
      # Extend to all a action profiles


      # v2 is best reply q for player 2
      q2.hat = matrix(q2.hat,na1, na2)
      v2.hat.short = rowMaxs(q2.hat)
      v2.hat = rep(v2.hat.short, times=na2)


      # Compute which action profiles are implementable
      IC.holds = U.hat >= v1.hat + v2.hat

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

      res$U[row] = U;
      res$v1[row] = v1; res$v2[row] = v2
      res$r1[row] = r1; res$r2[row] = r2

      # Pick equilibrium actions
      # If indifferent choose the one with the largest
      # slack in the IC
      slack = U.hat - (v1.hat + v2.hat)
      res$ae[row] = which.max(slack * (U.hat==U & IC.holds))
      res$a1[row] = which.max(slack * (v1.hat==v1 & IC.holds))
      res$a2[row] = which.max(slack * (v2.hat==v2 & IC.holds))


      if (save.details) {
        pi1 = sdf$pi1[[srow]]
        Er1 = as.vector(trans.mat %*% (res$r1[dest.rows]))
        # Continuation payoff if new negotiation in next period
        u1_neg = (1-delta)*pi1 + delta*Er1

        pi2 = sdf$pi2[[srow]]
        Er2 = as.vector(trans.mat %*% (res$r2[dest.rows]))
        # Continuation payoff if new negotiation in next period
        u2_neg = (1-delta)*pi2 + delta*Er2


        arows = seq_along(IC.holds)
        a.info = cbind(
          quick_df(t=t),
          x.df[x.df$x==x,],
          sdf$a.grid[[srow]],
          quick_df(
            can.ae = (U.hat==U & IC.holds)*1 + (arows==res$ae[row]),
            can.a1 = (v1.hat==v1 & IC.holds)*1 + (arows==res$ae[row]),
            can.a2 = (v2.hat==v2 & IC.holds)*1 + (arows==res$ae[row]),
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

  rows = match.by.cols(res,g$a.labs.df, cols1=c("x","ae"), cols2=c("x","a"))
  res$ae.lab = g$a.labs.df$lab[rows]

  rows = match.by.cols(res,g$a.labs.df, cols1=c("x","a1"), cols2=c("x","a"))
  res$a1.lab = g$a.labs.df$lab[rows]

  rows = match.by.cols(res,g$a.labs.df, cols1=c("x","a2"), cols2=c("x","a"))
  res$a2.lab = g$a.labs.df$lab[rows]

  if (!is.null(g$x.df))
    res = left_join(res, g$x.df, by="x")

    #res$ae.lab = left_join(select(res,x,a=ae), g$a.labs.df, by=c("x","a"))$lab
    #res$a1.lab = left_join(select(res,x,a=a1), g$a.labs.df, by=c("x","a"))$lab
    #res$a2.lab = left_join(select(res,x,a=a2), g$a.labs.df, by=c("x","a"))$lab

  g$sdf = sdf
  g$rne = res
  g$rne.details = rne.details
  g
}

