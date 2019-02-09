example.rne = function() {


  reveal.prob = 0.1
  g = rel_game("Blackmailing with Brinkmanship") %>%
    rel_param(delta=0.5, rho=0.5) %>%
    # Initial State
    rel_state("x0", A1=list(move=c("keep","reveal")),A2=NULL) %>%
    rel_payoff("x0",pi1=0, pi2=1) %>%
    rel_transition("x0","x1",move="reveal", prob=reveal.prob) %>%
    # Evidence Revealed
    rel_state("x1", A1=NULL,A2=NULL) %>%
    rel_payoff("x1",pi1=0, pi2=0) %>%
    rel_compile() %>%
    rel_rne()

  (rne = get.rne(g))
  (g$eq)


  reveal = seq(0,1,by=0.05)
  g = rel_game("Blackmailing with Endogenous Brinkmanship") %>%
    rel_param(delta=0.9, rho=0.5) %>%
    # Initial State
    rel_state("x0", A1=list(reveal=reveal),A2=NULL) %>%
    rel_payoff("x0",pi1=0, pi2=1) %>%
    rel_transition("x0","x1",reveal=reveal, prob=reveal) %>%
    # Evidence Revealed
    rel_state("x1", A1=NULL,A2=NULL) %>%
    rel_payoff("x1",pi1=0, pi2=0) %>%
    rel_compile() %>%
    rel_rne()

  (g$eq)

  reveal = seq(0,1,by=0.05)
  #reveal = c(0,0.51)
  g = rel_game("Blackmailing with Endogenous Brinkmanship") %>%
    rel_param(delta=0.9, rho=0.5) %>%
    # Initial State
    rel_state("x0", A1=list(reveal=reveal),A2=NULL) %>%
    rel_payoff("x0",pi1=0, pi2=1) %>%
    rel_transition("x0","x1",reveal=reveal, prob=reveal) %>%
    # Evidence Revealed
    rel_state("x1", A1=NULL,A2=NULL) %>%
    rel_payoff("x1",pi1=0, pi2=0) %>%
    rel_compile() %>%
    rel_capped_rne(T=100, save.details = FALSE, use.cpp=TRUE)

  (rne=g$eq)
  g$sdf$trans.mat

  de = get.rne.details(g)

  e = e.seq = seq(0,1, by=0.1); xL=0; xH=0.1;

  plot(e, e-0.5*e*e)

  g = rel_game("Weakly Monotone Vulnerability Paradox") %>%
    rel_param(delta=0.5, rho=0.1, c=0.5, xL=xL,xH=xH) %>%
    # Initial State
    rel_state("xL", A1=list(move=c("stay","vul")),A2=list(e=e.seq)) %>%
    rel_payoff("xL",pi1=~e, pi2=~ -c*e*e*(e>=0)) %>%
    rel_transition("xL","xH",move="vul") %>%
    # High vulnerability
    rel_state("xH", A1=NULL,A2=list(e=unique(c(-xH,e.seq)))) %>%
    rel_payoff("xH",pi1=~e, pi2=~ -c*e*e*(e>=0)) %>%
    rel_compile()

  cat(rel_to_mermaid_code(g))

  g = rel_rne(g)
  (rne = get.rne(g))
  rne


  gc = rel_capped_rne(g, T=10)
  rne = gc$rne
  filter(rne, t==1)


  g$sdf$rep

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

  rne.diagram(g)

  g = rel_rne(g)

  rne = g$eq
  rne


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

  (rne = get.rne(g,TRUE))


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

  (rne = g$eq)

  library(ggplot2)
  ggplot(filter(rne,x=="x0"), aes(y=r1,x=t)) + geom_line()



}


get.rne = get.spe = function(g, extra.cols="ae", eq=g$eq) {
  get.eq(g, extra.cols, eq)
}

get.eq = function(g, extra.cols="ae", eq=g$eq) {
  restore.point("get.eq")

  if (length(extra.cols)>0 & !isTRUE(g$is.multi.stage)) {
    ax.add = g$sdf$lag.cumsum.na
    for (col in extra.cols) {
      ax = eq[[col]]+ax.add
      extra = g$ax.grid[ax,-c(1:2), drop=FALSE]
      ax.extra = g$ax.extra
      if (!is.null(ax.extra))
        extra = cbind(extra, ax.extra[ax,,drop=FALSE])
      colnames(extra) = paste0(col,".", colnames(extra))
      eq = cbind(eq, extra)
    }
  } else if (length(extra.cols)>0 & isTRUE(g$is.multi.stage)) {
    ax.add = g$sdf$lag.cumsum.na
    s.ax.add = g$gs$sdf$lag.cumsum.na
    col = "ae"
    for (col in extra.cols) {

      ax = eq[[col]]+ax.add
      extra = g$ax.grid[ax,-c(1:2)]
      ax.extra = g$ax.extra
      if (!is.null(ax.extra))
        extra = cbind(extra, ax.extra[ax,,drop=FALSE])

      ax = eq[[paste0("s.",col)]]+s.ax.add
      extra = cbind(extra, g$gs$ax.grid[ax,-c(1:2),drop=FALSE])
      ax.extra = g$gs$ax.extra
      if (!is.null(ax.extra))
        extra = cbind(extra, ax.extra[ax,])

      if (col != "ae")
        colnames(extra) = paste0(col,".", colnames(extra))
      eq = cbind(eq,extra)
    }
  }
  eq
}

add.action.details = function(g,eq, action.cols=c("ae","a1","a2"), ax.grid=g$ax.grid) {
  restore.point("add.action.details")
  add.li = lapply(action.cols, function(col) {
    grid = eq[,c("x",col)]
    colnames(grid)[2] = ".a"
    add.a = left_join(grid, ax.grid, by=c("x",".a"))[,-c(1:2), drop=FALSE]
    colnames(add.a) = paste0(col,".", colnames(add.a))
    add.a
  })
  do.call(cbind, c(list(eq), add.li))
}

get.rne.details = function(g, x=NULL) {
  restore.point("get.rne.details")

  if (is.null(g$eq.details)) {
    message("No details have been saved. Use the argument save.details=TRUE in your call to rel_rne or rel_capped_rne.")
    return(NULL)
  }
  if (is.null(x)) return(g$eq.details)
  g$eq.details[g$eq.details$x %in% x,]

}

#' Find an RNE for a game with (weakly) monotone state transitions
#'
#' If the game has strictly monotone state transitions,
#' i.e. non-terminal states will be reached at most once,
#' there exists a unique RNE payoff.
#'
#' For weak monotone state transititions no RNE or multiple
#' RNE payoffs may exist.
#'
#' You can call rel_capped_rne to solve a capped version of the game
#' that allows state changes only up to some period T. Such a capped
#' version always has a unique RNE payoff.
#'
#' @param g The game object
#' @param save.details If yes, detailed information about the equilibrium for each state and period will be stored in g and can be retrieved via the function get.rne.details
rel_rne = function(g, delta=g$param$delta, rho=g$param$rho, adjusted.delta=NULL, beta1=g$param$beta1,verbose=TRUE,...) {
  restore.point("rel_rne")
  if (!g$is_compiled) g = rel_compile(g)

  if (isTRUE(g$is.multi.stage)) {
    stop("For games with a static and dynamic stage, so far only capped RNE can be computed, via the function re_capped_rne.")
  }
  res = compute.delta.rho(delta, rho, adjusted.delta)
  g$param$delta = delta = res$delta
  g$param$rho = rho = res$rho

  g$param$beta1 = beta1
  sdf = g$sdf
  adj_delta = (1-rho)*delta
  beta2 = 1-beta1


  rne = data_frame(x = sdf$x, solved=FALSE, r1=NA,r2=NA, U=NA, v1=NA,v2=NA,ae=NA,a1=NA,a2=NA)


  # First solve repeated games for all terminal states
  rows = which(sdf$is_terminal)
  g = rel_solve_repgames(g,rows=rows)
  sdf=g$sdf
  row = rows[1]

  for (row in rows) {
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
      if (verbose)
        cat("\nSolve weakly monotone state", x, "...")
      res = solve.weakly.monotone.state(x,rne,g,sdf)
      if (res$ok) {
        rne[row, names(res$rne.row)] = res$rne.row
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

  rne = add.rne.action.labels(g,rne)

  rne = select(rne,-solved)

  g$sdf = sdf
  g$eq = rne
  if (verbose) cat("\n")

  g
}



solve.weakly.monotone.state = function(x,rne,g, sdf, tol=1e-12) {
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
        # Note player 1 is col player
        q1.hat = matrix(q1.hat,na2, na1)
        v1.hat.short = rowMaxs(q1.hat)
        v1.hat = rep(v1.hat.short, times=na1)


        # v2.hat is best reply q for player 2
        q2.hat = matrix(q2.hat,na2, na1)
        v2.hat.short = colMaxs(q2.hat)
        v2.hat = rep(v2.hat.short, each=na2)


        # Compute which action profiles are implementable
        IC.holds = (U.hat+tol >= v1.hat + v2.hat)
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
        a1 = which.max(slack * is.a1 + is.a1)

        is.a2 = IC.holds & (abs(v2.hat-v2)<tol)
        if (sum(is.a2)==0) next
        a2 = which.max(slack * is.a2+is.a2)

        restore.point("found.eq")
        # M7 We have found an RNE
        return(list(ok=TRUE,rne.row=list(x=x,U=U,v1=v1,v2=v2,r1=r1,r2=r2, ae=ae,a1=a1, a2=a2)))

      }
    }
  }
  return(list(ok=FALSE))
}

# Internal function to select (dynamic) equilibrium action profiles
# for a current state of an RNE
# using the specified tie.breaking rule.
#
# Is called from r_capped_rne_iterations
r_rne_find_actions = function(U,v1,v2,U.hat,v1.hat,v2.hat, IC.holds, next.r1=NULL, next.r2=NULL, trans.mat=NULL, dest.rows=NULL, tie.breaking=c("equal_r","random",  "slack","first","last","max_r1","max_r2")[1], tol=1e-12) {
  restore.point("r_rne_find_actions")
  const = 1
  if (tie.breaking=="equal_r") {
    next_r_diff = -abs(next.r1-next.r2)
    tb = trans.mat.mult(trans.mat,next_r_diff[dest.rows])
    const = 1+-min(tb) + max(tb)-min(tb)
  } else if (tie.breaking=="random") {
    tb = runif(NROW(U.hat))
  } else if (tie.breaking=="slack") {
    tb = slack = U.hat - (v1.hat + v2.hat)
  } else if (tie.breaking=="last") {
    tb = seq_len(NROW(U.hat))
  } else if (tie.breaking=="first") {
    tb = rev(seq_len(NROW(U.hat)))
  } else if (tie.breaking=="max_r1") {
    tb = trans.mat.mult(trans.mat,next.r1[dest.rows])
    const = 1-min(tb) + max(tb)-min(tb)
  } else if (tie.breaking=="max_r2") {
    tb = trans.mat.mult(trans.mat,next.r2[dest.rows])
    const = 1-min(tb) + max(tb)-min(tb)
  } else {
    stop(paste0("Unknown tie.breaking rule ", tie.breaking))
  }

  ae = which.max((const+tb) * (abs(U.hat-U)<tol & IC.holds))
  a1 = which.max((const+tb) * (abs(v1.hat-v1)<tol & IC.holds))
  a2 = which.max((const+tb) * (abs(v2.hat-v2)<tol & IC.holds))
  c(ae,a1,a2)
}

compute.delta.rho = function(delta=NULL, rho=NULL, adjusted.delta=NULL) {
  if (!is.null(adjusted.delta)) {
    if (!is.null(rho)) {
      if (rho > 1-adjusted.delta) {
        stop("For and adjusted.delta of ", adjusted.delta, " the negotatiation probability rho can be at most ", 1-adjusted.delta)
      } else if (rho == 1-adjusted.delta) {
        warning(call.=FALSE, paste0("For adjusted.delta = ", adjusted.delta, " and rho = ", rho,", we have delta = 1, which can be problematic. Better pick a lower rho."))
      }
      delta = adjusted.delta / (1-rho)
    } else {
      if (delta < adjusted.delta) {
        stop("If you provide only an delta and adjusted.delta and set rho=NULL then delta cannot be smaller than adjusted.delta")
      }
      rho = 1- (adjusted.delta / delta)
    }
  }
  list(delta=delta, rho=rho)
}
