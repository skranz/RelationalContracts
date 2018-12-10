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
    rel_transition("xH","x0", prob=0.1) %>%
    rel_compile() %>%
    rel_rne()

  compute.x.trans.mat("x0",g)
  compute.x.trans.mat("xL",g)

  g = rel_rne(g)

  rne = g$rne

  solve_x_repgame(g,"xL")

  g

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
      trans.mat = compute.x.trans.mat(x=x,g=g)
      to.self = x %in% colnames(trans.mat)
      if (to.self)
        stop(paste0("The non-terminal state ", x, " has a positive probability (", max(trans.mat[,x]), ") to transit to itself. Cannot yet find RNE in such games."))
      sdf$trans.mat[[row]] = trans.mat


    }
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

    res$ae[row] = which.max(U.hat[IC.holds])
    res$a1[row] = which.min(v1.hat[IC.holds])
    res$a2[row] = which.min(v2.hat[IC.holds])

    res$solved[row] = TRUE
  }

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
