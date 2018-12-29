# Consider a special class of multistage games
# Actions that are relevant for state transitions are only specified in the final
# stage
# New negotiations only take place in the first stage
# No discounting between stages

examples.multistage = function() {
  library(RelationalContracts)

  A.fun = function(x1,x2,stage, x.seq,...) {
    restore.point("A.fun")
    if (stage=="m") {
      A1 = list(a1=x.seq[x.seq>=x1])
      A2 = list(a2=x.seq[x.seq>=x2])
    } else if (stage=="e") {
      A1 = list(b1=c("b",""))
      e=seq(0,1,by=0.5)
      A2 = quick_df(b2=c("b", rep("", length(e))),e=c(0,e))
    }
    list(A1=A1,A2=A2)
  }

  vec.pi.fun = function(ax.df,...) {
    restore.point("vec.pi.fun")
    res = ax.df %>%
      transmute(
        x=x,
        stage=stage,
        pi1= case_when(
          stage=="m" ~ 0,
          b1 == "b" | b2=="b" ~ -x1,
          TRUE ~ e
        ),
        pi2=case_when(
          stage=="m" ~ 0,
          b1 == "b" | b2=="b" ~ -x2,
          TRUE ~ - 1/2 * e^2
        )
      )
    res
  }

  vec.trans.fun = function(ax.df, final=FALSE,...) {
    restore.point("trans.fun")
    mt = ax.df %>%
      select(x,a1,a2) %>%
      unique() %>%
      transmute(xs=x,xd=paste0(a1, " ",a2),a1=a1,a2=a2, prob=1)
    bind_rows(et,mt)
  }


  x.seq = seq(0,1, by=0.5)
  #x.seq = c(0,0.01,0.05,0.1,0.2,0.5,1)
  x.df = as_data_frame(expand.grid(x1=x.seq,x2=x.seq, stringsAsFactors = FALSE)) %>%
    mutate(x= paste0(x1," ", x2))

  g = rel_game("Slowly Intensifying Repeated Principal-Agent") %>%
    rel_param(x.seq=x.seq) %>%
    rel_stages(c("e","m")) %>%
    rel_states(x.df,
      A.fun = A.fun,
      vec.pi.fun = vec.pi.fun,
      vec.trans.fun=vec.trans.fun,
      vec.final.trans.fun = vec.final.trans.fun
    )

  g=  rel_compile_multistage(g)
  sdf = g$sdf

  solve.rep.multistage(g,x="0 0")
  g$stages = c("e","m")
}

rel_stages = function(g, stages) {
  g$stages = stages
  g
}

rel_compile_multistage = function(g,...) {
  restore.point("rel_compile_multistage")

  stages = g$stages
  # Set default parameters

  # 1. Create a data frame with all states
  #def = g$state_defs[[2]]

  if (!is.null(g$x_df_def)) {
    g$x.df = bind_rows(g$x_df_def)
  } else {
    g$x.df = NULL
  }

  # Compute states defined by A.fun
  def = g$state_fun_defs[[1]]
  ns = length(g$stages)
  nx = length(def$x)


  A1 = vector("list", ns*nx)
  A2 = vector("list", ns*nx)
  a.grid = vector("list", ns*nx)
  na1 = na2 = rep(NA, ns*nx)
  row = xrow = 0
  for (x in def$x) {
    xrow = xrow+1
    x.df = get.x.df(def$x[xrow],g, TRUE)
    for (stage in g$stages) {
      row = row+1
      args = c(list(stage=stage),x.df, g$param,def$args)
      res = do.call(def$A.fun, args)
      A1[row] = list(res$A1)
      A2[row] = list(res$A2)
      na1[row] = compute.na(res$A1)
      na2[row] = compute.na(res$A2)
    }
  }
  sdf = quick_df(x=rep(def$x, each=ns),stage=rep(stages, times=nx),na1=na1,na2=na2,A1=A1,A2=A2)
  # Change order of A1 and A2 for compatibility
  # with repgame and dyngame

  sdf$a.grid = lapply(seq_len(NROW(sdf)), function(row) {
    A1 = sdf$A1[[row]]; A2 = sdf$A2[[row]]
    a.grid = factor.cols.as.strings(expand.grid2(A2,A1))
    cols = c(names(A1),names(A2))
    a.grid = a.grid[,cols]
    a.grid
  })
  sdf$row = seq_len(NROW(sdf))

  g$a.labs.df = bind_rows(lapply(seq_len(NROW(sdf)),function(row) {
    quick_df(x=sdf$x[row],a = seq_len(NROW(sdf$a.grid[[row]])), lab=make.state.lab.a(sdf[row,]))
  }))

  ax.grid = bind_rows(lapply(seq_len(NROW(sdf)), function(row) {
    cbind(quick_df(x=sdf$x[row],stage=sdf$stage[row], .a=seq_len(NROW(sdf$a.grid[[row]]))), sdf$a.grid[[row]])
  }))

  empty.action = sapply(ax.grid[3:NCOL(ax.grid)], function(vals) {
    all(vals == "" | is.na(vals))
  })
  g$ax.grid = ax.grid[c("x","stage", ".a", names(empty.action)[!empty.action])]

  ax.df = g$ax.grid
  if (!is.null(g$x.df)) {
    ax.df = inner_join(g$x.df,ax.df, by=c("x"="x"))
  }

  # 2. Evaluate and store payoff matrices
  pi1 = vector("list",nx*ns)
  pi2 = vector("list",nx*ns)


  # Payoff function
  def = g$payoff_fun_defs[[1]]
  if (!is.null(def$vec.pi.fun)) {
    #restore.point("vec.pi.fun not yet implemented!")
    args = c(list(ax.df=ax.df),g$param, def$args)
    res = do.call(def$vec.pi.fun, args)
    check.rel(has.cols(res,c("x","stage", "pi1","pi2")),"Your vectorized payoff function vec.pi.fun must return a data frame with the columns 'x','stage','pi1' and 'pi2'")
    check.rel(NROW(res)==NROW(ax.df),"Your vectorized payoff function vec.pi.fun must return a data frame with as many rows as its argument ax.df")

    res = group_by(res,x,stage) %>%
      summarize(pi1=list(pi1),pi2=list(pi2))
    rows = match.by.cols(res, sdf,cols = c("x","stage"))

    pi1[rows] = res$pi1
    pi2[rows] = res$pi2

  } else {
    for (x in def$x) {
      for (stage in stages) {
        row = which(sdf$x == x & sdf$stage==stage)
        a.grid = sdf$a.grid[[row]]
        args = c(get.x.df(x,g, TRUE),list(stage=stage,a.df = a.grid),g$param, def$args)
        res = do.call(def$pi.fun, args)
        pi1[row] = res["pi1"]
        pi2[row] = res["pi2"]
      }
    }
  }

  sdf$pi1 = pi1
  sdf$pi2 = pi2

  # 3. Evaluate and store state transitions
  # Note: Only last stage will be relevant for state transitions
  last.stage = g$stages[length(g$stages)]
  tax.df = ax.df[ax.df$stage == last.stage,,drop=FALSE]

  compile_trans_fun_def = function(ind, field="trans_fun_defs") {
    def = g[[field]][[ind]]

    if (!is.null(def$vec.trans.fun)) {
      args = c(list(ax.df = tax.df),g$param, def$args)
      res = do.call(def$vec.trans.fun, args)
    } else {
      res = bind_rows(lapply(def$x, function(x) {
        row = which(sdf$x == x & sdf$stage==last.stage)
        a.grid = sdf$a.grid[[row]]
        args = c(get.x.df(x,g, TRUE),list(a.df = a.grid),g$param, def$args)
        res = do.call(def$trans.fun, args)
        check.rel(has.cols(res,c("xs","xd","prob")), "Your manual state transition function must return a data frame that has the cols 'xs','xd', 'prob' and the names of relevant action profiles.")
        res
      }))
    }
    res$.def.ind = 1
    res
  }

  li2 = lapply(seq_along(g$trans_fun_defs),compile_trans_fun_def)

  tdf = bind_rows(li,li2) %>%
    filter(xs != xd) %>%
    unique()

  non.states = setdiff(unique(c(tdf$xd,tdf$xs)),sdf$x)
  check.rel(length(non.states)==0, paste0("You specify state transitions from or to unspecified states ", paste0(non.states, collapse=", "),". Make sure that you have not mis-spelled the state names."))


  if (NROW(tdf)>0) {
    sdf$is_terminal = !(sdf$x %in% tdf$xs)
  } else {
    sdf$is_terminal = TRUE
  }
  g$tdf = tdf

  # Result of repeated game assuming the state is fixed.
  # This will be a full characterization for all
  # discount factors

  sdf$rep = vector("list",NROW(sdf))
  sdf$trans.mat = vector("list",NROW(sdf))

  # Separate state transitions for final period in
  # a capped game
  if (!is.null(g$final_trans_fun_defs)) {
    li = lapply(seq_along(g$final_trans_fun_defs),compile_trans_fun_def, field="final_trans_fun_defs")

    g$final.tdf = bind_rows(li) %>%
      filter(xs != xd) %>%
      unique()
    sdf$final.trans.mat = vector("list",NROW(sdf))
  } else {
    g$final.tdf = NULL
  }

  # Compute action lists for repeated games
  ae.df.li = vector("list",NROW(sdf))
  a1.df.li = vector("list",NROW(sdf))
  a2.df.li = vector("list",NROW(sdf))

  for (row in 1:NROW(sdf)) {
    stage = sdf$stage[row]
    pi1 = sdf$pi1[[row]]
    pi2 = sdf$pi2[[row]]
    na1 = sdf$na1[[row]]; na2 = sdf$na2[[row]]

    c1 = find.best.reply.payoffs(1,pi1,na1,na2)
    c2 = find.best.reply.payoffs(2,pi2,na1,na2)
    G = pi1+pi2
    # Liquidity requirement
    L = c1+c2-G

    a.df = quick_df(stage=stage,.a=seq_along(pi1),G=G,c1=c1,c2=c2,L=L)


    ae.df = a.df %>% arrange(-G)
    minL = c(Inf,cummin(ae.df$L[-NROW(a.df)]))
    ae.df = ae.df[ae.df$L < minL,,drop=FALSE]

    a1.df = a.df %>% arrange(c1)
    minL = c(Inf,cummin(a1.df$L[-NROW(a.df)]))
    a1.df = a1.df[a1.df$L < minL,,drop=FALSE]

    a2.df = a.df %>% arrange(c2)
    minL = c(Inf,cummin(a2.df$L[-NROW(a.df)]))
    a2.df = a2.df[a2.df$L < minL,,drop=FALSE]

    ae.df.li[[row]] = ae.df
    a1.df.li[[row]] = a1.df
    a2.df.li[[row]] = a2.df

  }
  sdf$ae.df.li = ae.df.li
  sdf$a1.df.li = a1.df.li
  sdf$a2.df.li = a2.df.li


  g$sdf = sdf

  # Compute adjusted discount factor
  g$param$adj_delta = g$param$delta * (1-g$param$rho)

  g$is_compiled =TRUE


  # Compute all transition matrices
  for (row in sdf$row[sdf$stage==last.stage]) {
    x = sdf$x[row]
    g$sdf$trans.mat[row] = list(compute.x.trans.mat(x=x,g=g,row=row))
    if (!is.null(g$final.tdf)) {
      g$sdf$final.trans.mat[row] = list(compute.x.trans.mat(x=x,g=g, tdf=g$final.tdf,row=row))
    }
  }



  g
}



# Solving a repeated multistage game with perfect monitoring
solve.rep.multistage = function(g,x, delta=g$param$delta, rho=g$param$rho, tol=1e-10) {
  restore.point("solve.rep.multistage")
  stages = g$stages
  ns = length(stages)

  delta = delta*(1-rho)
  sdf = g$sdf
  # reverse stage order
  rows = which(sdf$x == x)

  # Action lists for each stage
  ae.df.li = sdf$ae.df.li[rows]
  ae.df = bind_rows(ae.df.li)
  ae.len = sapply(ae.df.li,NROW)
  ae.pos = c(1,ae.len[-ns])

  a1.df.li = sdf$a1.df.li[rows]
  a1.df = bind_rows(a1.df.li)
  a1.len = sapply(a1.df.li,NROW)
  a1.pos = c(1,a1.len[-ns])

  a2.df.li = sdf$a2.df.li[rows]
  a2.df = bind_rows(a2.df.li)
  a2.len = sapply(a2.df.li,NROW)
  a2.pos = c(1,a2.len[-ns])

  M = rev(cumsum(rev(ae.df$G[ae.pos]-a1.df$c1[a1.pos]-a2.df$c2[a2.pos])))
  M1 = M[1]
  M_

  # Critical interest rates
  r.ae = M


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
