# Functions to solve repeated simple multistage games
# This functionality was not included in the repgame package
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

  # Number of maximally considered action profile combinations
  ncomb = 1+sum(s.len-1)+sum(d.len-1)

  res.r = matrix(NA_real_,ncomb,7)
  res.i = matrix(NA_integer_,ncomb,6)

  colnames(res.r) = c("delta_min","delta_max","r1","r2","U","v1_rep","v2_rep")
  colnames(res.i) = c("s.ae","s.a1","s.a2","d.ae","d.a1","d.a2")

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
      v1 =  s.a1$c1+d.a1$c1
      v2 =  s.a2$c2+d.a2$c2
      U =  s.ae$G+d.ae$G
      res.r[cur.comb,] = c(
        0, # delta_min
        lowest.delta, # delta_max
        v1+beta1*(U-v1-v2), #r1
        v2+(1-beta1)*(U-v1-v2), #r2
        U,
        v1,
        v2
      )
      res.i[cur.comb,] = c(
        s.ae=s.ae$.a,
        s.a1=s.a1$.a,
        s.a2=s.a2$.a,
        d.ae=d.ae$.a,
        d.a1=d.a1$.a,
        d.a2=d.a2$.a
      )
      lowest.delta = 0
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
      cur.comb = cur.comb+1

      v1 =  s.a1$c1+d.a1$c1
      v2 =  s.a2$c2+d.a2$c2
      U =  s.ae$G+d.ae$G
      res.r[cur.comb,] = c(
        max.delta.crit, # delta_min
        lowest.delta, # delta_max
        v1+beta1*(U-v1-v2), #r1
        v2+(1-beta1)*(U-v1-v2), #r2
        U,
        v1,
        v2
      )
      res.i[cur.comb,] = c(
        s.ae=s.ae$.a,
        s.a1=s.a1$.a,
        s.a2=s.a2$.a,
        d.ae=d.ae$.a,
        d.a1=d.a1$.a,
        d.a2=d.a2$.a
      )
      lowest.delta = max.delta.crit
    }

    replace = which(delta.crit >= lowest.delta,arr.ind = TRUE)
    a.pos[replace] = a.pos[replace]+1

  }

  res = cbind(
    quick_df(x=rep(x, cur.comb)),
    as.data.frame(res.r[cur.comb:1,,drop=FALSE]),
    as.data.frame(res.i[cur.comb:1,,drop=FALSE])
  )

  if (make.strat.lab) {
    d.labs = g$a.labs.df$lab[g$a.labs.df$x==x]
    s.labs = g$gs$a.labs.df$lab[g$gs$a.labs.df$x==x]

    ae.lab = paste0(s.labs[res$s.ae]," | ",d.labs[res$d.ae])
    a1.lab = paste0(s.labs[res$s.a1]," | ",d.labs[res$d.a1])
    a2.lab = paste0(s.labs[res$s.a2]," | ",d.labs[res$d.a2])
    res$strat.lab = paste0("(",ae.lab,") (",a1.lab,") (",a2.lab,")")
  }

  res
}

