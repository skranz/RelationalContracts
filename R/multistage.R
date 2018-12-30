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

  static.A.fun = function(x1,x2, x.seq,...) {
    restore.point("static.A.fun")
    A1 = list(b1=c("b",""))
    e=seq(0,1,by=0.5)
    A2 = quick_df(b2=c("b", rep("", length(e))),e=c(0,e))
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


  x.seq = seq(0,1, by=0.5)
  #x.seq = c(0,0.01,0.05,0.1,0.2,0.5,1)
  x.df = as_data_frame(expand.grid(x1=x.seq,x2=x.seq, stringsAsFactors = FALSE)) %>%
    mutate(x= paste0(x1," ", x2))

  g = rel_game("Slowly Intensifying Repeated Principal-Agent") %>%
    rel_param(x.seq=x.seq) %>%
    rel_states(x.df,
      # Static effort stage
      static.A.fun=static.A.fun,
      vec.static.pi.fun = vec.static.pi.fun,
      # Dynamic relationship intensification stage
      A.fun = A.fun,
      pi1 = 0, pi2=0,
      vec.trans.fun=vec.trans.fun
    )

  g=  rel_compile(g)
  sdf = g$sdf

  solve.rep.multistage(g,x="0 0")
  solve.rep.multistage(g,x="1 1")
  g$stages = c("e","m")
}


add.rel.multistage.compile = function(g,...) {
  restore.point("add.rel.multistage.compile")

  gs = g$static_defs
  gs$x.df = g$x.df
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


    ae.df = a.df %>% arrange(-G)
    minL = c(Inf,cummin(ae.df$L[-NROW(a.df)]))
    ae.df = ae.df[ae.df$L < minL,,drop=FALSE]

    a1.df = a.df %>% arrange(c1)
    minL = c(Inf,cummin(a1.df$L[-NROW(a.df)]))
    a1.df = a1.df[a1.df$L < minL,,drop=FALSE]

    a2.df = a.df %>% arrange(c2)
    minL = c(Inf,cummin(a2.df$L[-NROW(a.df)]))
    a2.df = a2.df[a2.df$L < minL,,drop=FALSE]
    list(ae.df=ae.df, a1.df=a1.df, a2.df=a2.df)
  })
  li
}


# Solving a repeated simply static dynamic multistage game with perfect monitoring
solve.rep.multistage = function(g,x, delta=g$param$delta, rho=g$param$rho, tol=1e-10) {
  restore.point("solve.rep.multistage")

  row = which(g$sdf$x==x)

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
        comb=cur.comb,
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
        comb=cur.comb,
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

  res = bind_rows(res.li)

  res

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
