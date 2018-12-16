example.rne = function() {

  e.seq = c(0,1); xL=0; xH=5;
  g = rel_game("Weakly Monotone Vulnerability Paradox") %>%
    rel_param(delta=0.8, rho=0.5, c=0.4, xL=xL,xH=xH) %>%
    # Initial State
    rel_state("xL", A1=list(move=c("stay","vul")),A2=list(e=e.seq)) %>%
    rel_payoff("xL",pi1=~e, pi2=~ -c*e*(e>=0)) %>%
    rel_transition("xL","xH",move="vul") %>%
    # High vulnerability
    rel_state("xH", A1=NULL,A2=list(e=unique(c(-xH,e.seq)))) %>%
    rel_payoff("xH",pi1=~e, pi2=~ -c*e*(e>=0)) %>%
    rel_compile()

  g = rel_rne(g)




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
    rne = mutate(a.df,
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
    rne
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

  rne = get.rne.details(g)

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


  rne = data_frame(x = sdf$x, solved=FALSE, r1=NA,r2=NA, U=NA, v1=NA,v2=NA,ae=NA,a1=NA,a2=NA)




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
  rne$solved[rows] = TRUE
  rne

  # Compute state transitions for all remaining states
  for (row in which(!rne$solved)) {
    if (is.null(sdf$trans.mat[[row]])) {
      x = sdf$x[row]
      sdf$trans.mat[row] = list(compute.x.trans.mat(x=x,g=g))
    }
    #to.self = x %in% colnames(sdf$trans.mat[[row]])
    #if (to.self)
    #  stop(paste0("The non-terminal state ", x, " has a positive probability (", max(trans.mat[,x]), ") to transit to itself. Cannot yet find RNE in such games."))
  }
  g$sdf = sdf

  find.next.state.row = function() {
    x.solved = sdf$x[rne$solved]
    rows = which(!rne$solved)
    for (row in rows) {
      xd = colnames(sdf$trans.mat[[row]])
      if (all(xd %in% c(sdf$x[row],x.solved)))
        return(row)
    }
    # No row could be found
    return(NA)
  }



  while(sum(!rne$solved)>0) {
    row = find.next.state.row()
    if (is.na(row)) {
      x.unsolved = sdf$x[!rne$solved]
      stop(paste0("Cannot compute RNE since there is a cycle among the non-terminal state(s) ", paste0(x.unsolved, collapse=", ")))
    }
    x = sdf$x[row]
    trans.mat = sdf$trans.mat[[row]]

    # Check if state transists to itself
    if (x %in% colnames(trans.mat)) {
      cat("Solve weakly monotone state", x, "...")
      res = solve.weakly.monotone.state(x,rne,g,sdf)
      if (res$ok) {
        rne$U[row] = res$U;
        rne$v1[row] = res$v1; rne$v2[row] = res$v2
        rne$r1[row] = res$r1; rne$r2[row] = res$r2
        rne$a1[row] = res$a1; rne$a2[row] = res$a2
        rne$ae[row] = res$ae
        rne$solved[row] = TRUE
        cat(" ok")
        next
      } else {
        stop("Cannot find an RNE in weakly monotone state ", x)
      }
    }

    xd = colnames(trans.mat)
    dest.rows = match(xd, rne$x)
    na1 = sdf$na1[row]
    na2 = sdf$na2[row]

    # Include code to compute U v and r for the current state
    U.hat = (1-delta)*(sdf$pi1[[row]] + sdf$pi2[[row]]) +
      delta * (trans.mat %*% rne$U[dest.rows])

    # "q-value" of punishment payoffs
    q1.hat = (1-delta)*sdf$pi1[[row]] +
      delta * (trans.mat %*% ( (1-rho)*rne$v1[dest.rows] + rho*rne$r1[dest.rows] ))

    q2.hat = (1-delta)*sdf$pi2[[row]] +
      delta * (trans.mat %*% ( (1-rho)*rne$v2[dest.rows] + rho*rne$r2[dest.rows] ))

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

    rne$U[row] = U;
    rne$v1[row] = v1; rne$v2[row] = v2
    rne$r1[row] = r1; rne$r2[row] = r2

    # Pick equilibrium actions
    # If indifferent choose the one with the largest
    # slack in the IC
    slack = U.hat - (v1.hat + v2.hat)
    rne$ae[row] = which.max(slack * (U.hat==U & IC.holds))
    rne$a1[row] = which.max(slack * (v1.hat==v1 & IC.holds))
    rne$a2[row] = which.max(slack * (v2.hat==v2 & IC.holds))

    rne$solved[row] = TRUE
  }

  # Add some additional info
  for (row in seq_len(NROW(rne))) {
    rne$ae.lab = left_join(select(rne,x,a=ae), g$a.labs.df, by=c("x","a"))$lab
    rne$a1.lab = left_join(select(rne,x,a=a1), g$a.labs.df, by=c("x","a"))$lab
    rne$a2.lab = left_join(select(rne,x,a=a2), g$a.labs.df, by=c("x","a"))$lab
  }

  rne = select(rne,-solved)
  g$sdf = sdf
  g$rne = rne
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


  rne = data_frame(x = rep(sdf$x,times=T),t=rep(T:1,each=NROW(sdf)), r1=NA,r2=NA, U=NA, v1=NA,v2=NA,ae=NA,a1=NA,a2=NA)


  rne.details = NULL
  if (save.details)
    rne.details = vector("list",NROW(rne))

  a.rne = data_frame(row=integer(0))

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

  # Compute all transition matrices
  for (row in 1:NROW(sdf)) {
    if (is.null(sdf$trans.mat[[row]])) {
      x = sdf$x[row]
      sdf$trans.mat[row] = list(compute.x.trans.mat(x=x,g=g))
    }
  }
  g$sdf = sdf


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
        delta * (trans.mat %*% rne$U[dest.rows])
      U.hat = as.vector(U.hat)

      # "q-value" of punishment payoffs
      q1.hat = (1-delta)*sdf$pi1[[srow]] +
        delta * (trans.mat %*% ( (1-rho)*rne$v1[dest.rows] + rho*rne$r1[dest.rows] ))

      q2.hat = (1-delta)*sdf$pi2[[srow]] +
        delta * (trans.mat %*% ( (1-rho)*rne$v2[dest.rows] + rho*rne$r2[dest.rows] ))

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

      rne$U[row] = U;
      rne$v1[row] = v1; rne$v2[row] = v2
      rne$r1[row] = r1; rne$r2[row] = r2

      # Pick equilibrium actions
      # If indifferent choose the one with the largest
      # slack in the IC
      slack = U.hat - (v1.hat + v2.hat)
      rne$ae[row] = which.max(slack * (U.hat==U & IC.holds))
      rne$a1[row] = which.max(slack * (v1.hat==v1 & IC.holds))
      rne$a2[row] = which.max(slack * (v2.hat==v2 & IC.holds))


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
            can.ae = (U.hat==U & IC.holds)*1 + (arows==rne$ae[row]),
            can.a1 = (v1.hat==v1 & IC.holds)*1 + (arows==rne$ae[row]),
            can.a2 = (v2.hat==v2 & IC.holds)*1 + (arows==rne$ae[row]),
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
  g$rne = rne
  g$rne.details = rne.details
  g
}


solve.weakly.monotone.state = function(x,rne,g, sdf, tol=1e-8) {
  restore.point("solve.weakly.monotone.state")
  rne = rne
  delta = g$param$delta
  rho = g$param$rho
  beta1 = g$param$beta1
  beta2 = 1-beta1

  row = which(sdf$x==x)
  # Transition probability to own states
  trans.mat = g$sdf$trans.mat[[row]]
  omega = trans.mat[,x]

  # Transitions to other states
  xd = setdiff(colnames(trans.mat),x)
  trans.mat = trans.mat[,xd, drop=FALSE]
  dest.rows = match(xd, rne$x)

  na1 = sdf$na1[row]
  na2 = sdf$na2[row]

  pi1 = sdf$pi1[[row]]
  pi2 = sdf$pi2[[row]]
  Pi = pi1+pi2

  a.grid = sdf$a.grid[[row]]

  A = 1:NROW(a.grid)
  EU_x = as.vector(trans.mat %*% rne$U[dest.rows])
  Evr1_x = as.vector(trans.mat %*% ((1-rho)*rne$v1[dest.rows]+rho*rne$r1[dest.rows]))
  Evr2_x = as.vector(trans.mat %*% ((1-rho)*rne$v2[dest.rows]+rho*rne$r2[dest.rows]))


  # S1: Compute U.tilde(a,x) for all actions
  U.tilde = (1 / (1-delta*omega)) * ((1-delta)*Pi + delta*(EU_x))


  K1.vec = (1-delta)*pi1 + delta*Evr1_x
  K2.vec = (1-delta)*pi2 + delta*Evr2_x

  m1.vec = delta*omega*rho*beta1
  d1.vec = 1-delta*omega*((1-rho)+rho*(1-beta1))

  m2.vec = delta*omega*rho*beta2
  d2.vec = 1-delta*omega*((1-rho)+rho*(1-beta2))

  # L1 Loop through all ae starting with largest value of U.tilde
  # In the moment simply loop through all ae
  Ae = A[order(-U.tilde)]

  ae = Ae[1]
  a1.tilde = A[1]
  a2.tilde = A[1]
  for (ae in Ae) {
    # L2 Loop through all combinations of a1.tilde and a2.tilde
    U = U.tilde[ae]
    for (a1.tilde in A) {
      for (a2.tilde in A) {
        # M1 Compute v and r
        m1 = m1.vec[a1.tilde]; m2 = m2.vec[a2.tilde]
        d1 = d1.vec[a1.tilde]; d2 = d2.vec[a2.tilde]
        K1 = K1.vec[a1.tilde]; K2 = K2.vec[a2.tilde]

        v1 = (m1*d2*U -m1*m2*U - m1*K2+d2*K1) / (d1*d2-m1*m2)
        v2 = (m2*d1*U -m2*m1*U - m2*K1+d1*K2) / (d1*d2-m1*m2)

        r1 = v1 + beta1*(U-v1-v2)
        r2 = v2 + beta2*(U-v1-v2)

        # Check v1 equation
        v1.diff = v1-(K1+delta*omega[a1.tilde]*((1-rho)*v1+rho*r1))
        if (abs(v1.diff) > tol) {
          restore.point("computation.error")
          stop(paste0("v1 not correctly computed!"))
        }

        # M2: Now compute U.hat, v.hat for all action profiles taking U, v1, v2, r1 and r2 as given
        U.hat = (1-delta)*Pi + delta*(EU_x+omega*U)
        q1.hat = (1-delta)*pi1 + delta*(Evr1_x + omega*((1-rho)*v1+rho*r1))
        q2.hat = (1-delta)*pi2 + delta*(Evr2_x + omega*((1-rho)*v2+rho*r2))


        # v1.hat is best reply q for player 1
        q1.hat = matrix(q1.hat,na1, sdf$na2)
        v1.hat.short = colMaxs(q1.hat)
        v1.hat = rep(v1.hat.short, each=na1)


        # v2.hat is best reply q for player 2
        q2.hat = matrix(q2.hat,sdf$na1[row], sdf$na2[row])
        v2.hat.short = rowMaxs(q2.hat)
        v2.hat = rep(v2.hat.short, times=na2)


        # Compute which action profiles are implementable
        IC.holds = (U.hat >= v1.hat + v2.hat)
        slack = U.hat - (v1.hat + v2.hat)

        # M3 check incentive constraint for ae
        if (!IC.holds[ae]) next

        # M4 check if there is another incentive compatible
        # a with higher payoff
        better.ae = IC.holds & (U.hat > U+tol)
        if (any(better.ae)) next

        # M5 Check if there are better incentive compatible
        # punishment profiles
        better.a1 = IC.holds & (v1.hat < v1-tol)
        if (any(better.a1)) next

        better.a2 = IC.holds & (v2.hat < v2-tol)
        if (any(better.a2)) next

        # M6 check if there are incentive compatible a1 and a2
        # that implement payoffs v1 and v2, respectively.
        is.a1 = IC.holds & abs(v1.hat-v1)<tol
        if (sum(is.a1)==0) next
        a1 = which.max(slack * ((abs(v1.hat-v1)<tol) & IC.holds))

        is.a2 = IC.holds & (abs(v2.hat-v2)<tol)
        if (sum(is.a2)==0) next
        a2 = which.max(slack * ((abs(v2.hat-v2)<tol) & IC.holds))

        # M7 We have found an RNE
        return(list(ok=TRUE,rne.row=list(x=x,U=U,v1=v1,v2=v2,r1=r1,r2=r2, ae=ae,a1=a1, a2=a2)))

      }
    }
  }
  return(list(ok=FALSE))
}
