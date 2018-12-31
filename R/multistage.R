# A special game with two action stages in a period
#
# In the first stage only static actions can be chosen that don't affect
# the state tranistions, e.g. output in a Cournot model
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
  g = rel_capped_rne(g,T=20, adjusted.delta=0.18, rho=0.7)

  rne.diagram(g,just.eq.chain = TRUE)
  (rne = g$rne)
}


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

compute.rep.game.action.lists = function(sdf, rows=seq_len(NROW(sdf))) {
  restore.point("compute.rep.game.action.lists")
  li = lapply(rows, function(row) {
    pi1 = sdf$pi1[[row]]
    pi2 = sdf$pi2[[row]]
    na1 = sdf$na1[[row]]; na2 = sdf$na2[[row]]

    c1 = find.best.reply.payoffs(1,pi1,na1,na2)
    c2 = find.best.reply.payoffs(2,pi2,na1,na2)
    G = pi1+pi2
    # Liquidity requirement
    L = c1+c2-G

    a.df = quick_df(.a=seq_along(pi1),G=G,c1=c1,c2=c2,L=L)


    # Faster than arrange
    ae.df = a.df[order(-a.df$G),]

    minL = c(Inf,cummin(ae.df$L[-NROW(a.df)]))
    ae.df = ae.df[ae.df$L < minL,,drop=FALSE]


    #a1.df = a.df %>% arrange(c1)
    a1.df = a.df[order(a.df$c1),]

    minL = c(Inf,cummin(a1.df$L[-NROW(a.df)]))
    a1.df = a1.df[a1.df$L < minL,,drop=FALSE]

    #a2.df = a.df %>% arrange(c2)
    a2.df = a.df[order(a.df$c2),]
    minL = c(Inf,cummin(a2.df$L[-NROW(a.df)]))
    a2.df = a2.df[a2.df$L < minL,,drop=FALSE]
    list(ae.df=ae.df, a1.df=a1.df, a2.df=a2.df)
  })
  li
}


# Solving a repeated simply static dynamic multistage game with perfect monitoring
solve.x.rep.multistage = function(g,x=NULL,row=NULL, tol=1e-10, beta1=g$param$beta1, make.strat.lab=FALSE) {
  restore.point("solve.x.rep.multistage")

  if (!is.null(row)) {
    x=g$sdf$x[row]
  } else {
    row = which(g$sdf$x==x)
  }
  # Dynamic and static action lists containing ae.df, a1.df, a2.df
  s.li = g$static.rep.li[[row]]
  d.li = g$dyn.rep.li[[row]]

  # Number of list elements
  s.len = sapply(s.li, NROW)
  d.len = sapply(d.li, NROW)

  # Starting pos in each list
  a.pos = matrix(1,2,3)
  # Length each list as matrix
  a.len = rbind(s=s.len,d=d.len)

  # Number of considered action profile combinations
  ncomb = prod(s.len)*prod(d.len)

  res.li = vector("list",ncomb)

  cur.comb = 0
  lowest.delta = Inf
  # Go through all combinations of possible action profiles
  while(all(a.pos<= a.len)) {
    s.ae = s.li$ae.df[a.pos[1,1],]
    s.a1 = s.li$a1.df[a.pos[1,2],]
    s.a2 = s.li$a2.df[a.pos[1,3],]

    d.ae = d.li$ae.df[a.pos[2,1],]
    d.a1 = d.li$a1.df[a.pos[2,2],]
    d.a2 = d.li$a2.df[a.pos[2,3],]

    Md = d.ae$G - d.a1$c1 - d.a2$c2
    Ms = s.ae$G - s.a1$c1 - s.a2$c2

    # We have a stage game NE in both stages
    if (Ms==0 && Md==0) {
      cur.comb = cur.comb+1

      res.li[[cur.comb]] = quick_df(
        delta.min = 0,
        U = s.ae$G+d.ae$G,
        v1 = s.a1$c1+d.a1$c1,
        v2 = s.a2$c2+d.a2$c2,
        s.ae=s.ae$.a,
        s.a1=s.a1$.a,
        s.a2=s.a2$.a,
        d.ae=d.ae$.a,
        d.a1=d.a1$.a,
        d.a2=d.a2$.a
      )
      break
    }

    # Critical interest rates
    sr.ae = (Md+Ms) / (s.ae$L - Md)
    sr.a1 = (Md+Ms) / (s.a1$L - Md)
    sr.a2 = (Md+Ms) / (s.a2$L - Md)

    dr.ae = (Md+Ms) / (d.ae$L)
    dr.a1 = (Md+Ms) / (d.a1$L)
    dr.a2 = (Md+Ms) / (d.a2$L)

    r.crit = matrix(c(sr.ae,dr.ae,sr.a1,dr.a1,sr.a2,dr.a2),2,3)
    delta.crit = 1 / (1+r.crit)
    max.delta.crit = max(delta.crit)

    # Combination does reduce delta
    if (max.delta.crit < lowest.delta) {
      lowest.delta = max.delta.crit
      cur.comb = cur.comb+1

      res.li[[cur.comb]] = quick_df(
        delta.min = lowest.delta,
        U = s.ae$G+d.ae$G,
        v1 = s.a1$c1+d.a1$c1,
        v2 = s.a2$c2+d.a2$c2,
        s.ae=s.ae$.a,
        s.a1=s.a1$.a,
        s.a2=s.a2$.a,
        d.ae=d.ae$.a,
        d.a1=d.a1$.a,
        d.a2=d.a2$.a
      )
    }

    replace = which(delta.crit == max.delta.crit,arr.ind = TRUE)
    a.pos[replace] = a.pos[replace]+1

  }

  res = bind_rows(res.li) %>% arrange(delta.min)

  if (make.strat.lab) {
    d.labs = g$a.labs.df$lab[g$a.labs.df$x==x]
    s.labs = g$gs$a.labs.df$lab[g$gs$a.labs.df$x==x]

    ae.lab = paste0(s.labs[res$s.ae]," | ",d.labs[res$d.ae])
    a1.lab = paste0(s.labs[res$s.a1]," | ",d.labs[res$d.a1])
    a2.lab = paste0(s.labs[res$s.a2]," | ",d.labs[res$d.a2])
    strat.lab = paste0("(",ae.lab,") (",a1.lab,") (",a2.lab,")")
  } else {
    strat.lab = ""
  }


  # Transform to same format as
  rep = transmute(res,
    delta_min=delta.min,
    delta_max=c(delta.min[-1],1),
    r1 = v1+beta1*(U-v1-v2),
    r2 = v2+(1-beta1)*(U-v1-v2),
    U = U,
    v1_rep = v1,
    v2_rep = v2,
    strat.lab = strat.lab,
    s.ae = s.ae,
    s.a1 = s.a1,
    s.a2 = s.a2,
    d.ae = d.ae,
    d.a1 = d.a1,
    d.a2 = d.a2
  )
  rep
}

find.best.reply.payoffs = function(i,pi,na1,na2) {
  if (i==1) {
    # Player 1's actions are columns
    pi.mat = matrix(pi,na2,na1)
    c.short = rowMaxs(pi.mat)
    c = rep(c.short, times=na1)
  } else {
    # Player 2's actions are rows
    pi.mat = matrix(pi,na2,na1)
    c.short = colMaxs(pi.mat)
    c = rep(c.short, each=na2)
  }
  c
}


#' Solve an RNE for a capped version of a multistage game
rel.capped.rne.multistage = function(g,T, save.details=FALSE, tol=1e-10,  delta=g$param$delta, rho=g$param$rho, res.field="rne", tie.breaking=c("slack","random","first","last")[1]) {
  restore.point("rel.capped.rne.multistage")
  if (!g$is_compiled) g = rel_compile(g)

  g$param$delta = delta
  g$param$rho = rho

  sdf = g$sdf
  adj_delta = (1-rho)*delta
  beta1 = g$param$beta1
  beta2 = 1-beta1

  if (save.details) {
    x.df = non.null(g$x.df, quick_df(x=sdf$x))
  }

  rne = data_frame(x = rep(sdf$x,times=T),t=rep(T:1,each=NROW(sdf)), r1=NA,r2=NA, U=NA, v1=NA,v2=NA,s.ae=NA,s.a1=NA,s.a2=NA,d.ae=NA,d.a1=NA,d.a2=NA)

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

    rne[row,c("U","r1","r2","s.ae","s.a1","s.a2","d.ae","d.a1","d.a2")] =
      rep[1,c("U","r1","r2","s.ae","s.a1","s.a2","d.ae","d.a1","d.a2")]

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
      row = srow + (T-t)*NROW(sdf)
      x = sdf$x[srow]

      # 1. Solve dynamic stage

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

      rne$d.ae[row] = which.max((const+tb) * (abs(U.hat-U)<tol & IC.holds))
      rne$d.a1[row] = which.max((const+tb) * (abs(v1.hat-v1)<tol & IC.holds))
      rne$d.a2[row] = which.max((const+tb) * (abs(v2.hat-v2)<tol & IC.holds))

      # 2. Solve static stage

      # Action lists of the repeated game
      s.li = g$static.rep.li[[srow]]

      dU = U; dv1=v1; dv2=v2
      # Available liquidity after static stage
      # TO DO: Check formula
      L.av = 1/(1-delta)*(dU-dv1-dv2)

      # Use previously computed list
      # of candidates for optimal profiles
      s.e = filter(s.li$ae.df,L.av-L >= -tol)[1,]
      s.1 = filter(s.li$a1.df,L.av-L >= -tol)[1,]
      s.2 = filter(s.li$a2.df,L.av-L >= -tol)[1,]

      U = (1-delta)*s.e$G+dU
      v1 = (1-delta)*s.1$c1 + dv1
      v2 = (1-delta)*s.2$c2 + dv2

      r1 = v1 + beta1*(U-v1-v2)
      r2 = v2 + beta2*(U-v1-v2)


      rne$U[row] = U;
      rne$v1[row] = v1; rne$v2[row] = v2
      rne$r1[row] = r1; rne$r2[row] = r2
      rne$s.ae[row] = s.e$.a
      rne$s.a1[row] = s.1$.a
      rne$s.a2[row] = s.2$.a




      # Save only details about dynamic stage
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
            d.can.ae = (abs(U.hat-dU)<tol & IC.holds)*1 + (arows==rne$d.ae[row]),
            d.can.a1 = (abs(v1.hat-dv1)<tol & IC.holds)*1 + (arows==rne$d.a1[row]),
            d.can.a2 = (abs(v2.hat-dv2)<tol & IC.holds)*1 + (arows==rne$d.a2[row]),
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
