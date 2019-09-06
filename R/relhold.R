example.relhold = function() {
  # Vulnerability Paradox
  e.seq = c(0,1); xL=0; xH=5;
  g = rel_game("Vulnerability Paradox") %>%
    rel_param(delta=0.8, rho=0.5, c=0.4, xL=xL,xH=xH) %>%
    # Low vulnerability
    rel_state("xL", A1=c("stay","vul"),A2=unique(c(-xL,e.seq))) %>%
    rel_payoff_v("xL",pi1=~a2, pi2=~ -c*a2*(a2>=0)) %>%
    rel_transition("xL","xH",a1="vul") %>%
    # High vulnerability
    rel_state("xH", A1=NULL,A2=unique(c(-xH,e.seq))) %>%
    rel_payoff("xH",pi1=~a2, pi2=~ -c*a2*(a2>=0))

  e.seq = c(0,1); xL=0; xH=5;
  g = rel_game("Simple Vulnerability Paradox") %>%
    rel_param(delta=0.8, rho=0.5, c=0.4, xL=xL,xH=xH) %>%
    # Initial State
    rel_state("x0", A1=c("to_xL","to_xH")) %>%
    rel_payoff("x0",pi1=0,pi2=0) %>%
    rel_transition("x0",c("xL","xH"),a1=c("to_xL","to_xH")) %>%
    # Low vulnerability
    rel_state("xL", A2=list(e=unique(c(-xL,e.seq)))) %>%
    rel_payoff("xL",pi1=~e, pi2=~ -c*e*(e>=0)) %>%
    # High vulnerability
    rel_state("xH", A1=NULL,A2=list(e=unique(c(-xH,e.seq)))) %>%
    rel_payoff("xH",pi1=~e, pi2=~ -c*e*(e>=0)) %>%
    rel_compile()

  g

  g

}

get.x.df = function(x,g, as.list=FALSE) {
  if (is.null(g$x.df)) {
    if (as.list) return(list(x=x))
    return(quick_df(x=x))
  }
  res = left_join(quick_df(x=x), g$x.df, by="x")
  if (as.list) return(as.list(res))
  res
}

get.def.x = function(x,g,x.df=g$x.df, sdf=g$sdf) {
  if (!is.null(x)) return(x)
  if (!is.null(x.df)) return(x.df$x)
  if (!is.null(sdf)) return(sdf$x)
  NULL
}

#' Compiles a relational contracting game
rel_compile = function(g,..., compute.just.static=FALSE) {
  restore.point("rel_compile")

  # Set default parameters

  # 1. Create a data frame with all states
  g_defs = g$defs

  param = g$param
  x.df = g$x.df
  if (is.null(x.df)) {
    if (!is.null(g_defs$x_df_def)) {
      x.df = bind_rows(g_defs$x_df_def)
    } else {
      x.df = NULL
    }
  }
  g$x.df = x.df


  #def = g_defs$state_defs[[1]]
  li = lapply(g_defs$state_defs, function(def) {
    A1 = eval.rel.expression(def$A1,g, null.value = "")
    na1 = compute.na(A1)
    A2 = eval.rel.expression(def$A2,g, null.value = "")
    na2 = compute.na(A2)

    # Change order of A1 and A2 for compatibility
    # with repgame and dyngame
    a.grid = factor.cols.as.strings(expand.grid2(A2,A1))
    a.grid.cols = c(names(A1),names(A2))
    a.grid = a.grid[,a.grid.cols,drop=FALSE]

    if (length(def[["x"]])==1) {
      state = quick_df(x=def$x,na1=na1,na2=na2, A1=list(A1),A2=list(A2),a.grid=list(a.grid))
    } else {
      restore.point("multiple.state.A")
      x = def[["x"]]
      n = length(x)
      if (n == 0)
        stop("rel_state called without specifiying x.")
      state = quick_df(x=x,na1=na1,na2=na2,A1=replicate(n,A1,simplify = FALSE),A2=replicate(n,A2,simplify = FALSE),a.grid=replicate(n,a.grid,simplify = FALSE))
    }
  })

  # Compute states defined by A.fun
  #def = g_defs$state_fun_defs[[1]]
  li2 = lapply(g_defs$state_fun_defs, function(def) {
    def$x = get.def.x(def$x,g)

    if (!is.null(def$vec.A.fun)) {
      args = c(get.x.df(def$x,g,TRUE),list(param, def$args))
      res = do.call(def$vec.A.fun,args)
      res$A1 = unique(res$A1)
      res$A2 = unique(res$A2)
      na1 = sapply(res$A1, compute.na)
      na2 = sapply(res$A2, compute.na)
      state = quick_df(x=x,na1=na1,na2=na2,A1=res$A1,A2=res$A2)
    } else {
      A1 = vector("list", length(def$x))
      A2 = vector("list", length(def$x))
      na1 = na2 = rep(NA, length(def$x))
      for (row in seq_along(def$x)) {
        args = c(get.x.df(def$x[row],g, TRUE), param,def$args)
        check.rel(length(args$x)==1,"There is some error in your state definitions. Make sure that each state x has a unique name.")


        res = do.call(def$A.fun, args)
        res$A1 = lapply(res$A1, unique)
        res$A2 = lapply(res$A2, unique)
        A1[row] = list(res$A1)
        A2[row] = list(res$A2)
        na1[row] = compute.na(res$A1)
        na2[row] = compute.na(res$A2)

      }
      state = quick_df(x=def$x,na1=na1,na2=na2,A1=A1,A2=A2)
    }
    # Change order of A1 and A2 for compatibility
    # with repgame and dyngame

    state$a.grid = lapply(seq_len(NROW(state)), function(row) {
      A1 = state$A1[[row]]
      A2 = state$A2[[row]]
      a.grid = factor.cols.as.strings(expand.grid2(A2,A1))
      cols = c(names(A1),names(A2))
      a.grid = a.grid[,cols]
      a.grid
    })
    state
  })

  sdf = bind_rows(li,li2)
  nx = NROW(sdf)
  sdf$row = seq_len(NROW(sdf))
  sdf$na.vec = sdf$na1*sdf$na2
  sdf$lag.cumsum.na = c(0,cumsum(sdf$na.vec[-nx]))


  g$a.labs.df = bind_rows(lapply(seq_len(NROW(sdf)),function(row) {
    quick_df(x=sdf$x[row],a = seq_len(NROW(sdf$a.grid[[row]])), lab=make.state.lab.a(sdf[row,]))
  }))


  ax.grid = bind_rows(lapply(seq_len(NROW(sdf)), function(row) {
    cbind(quick_df(x=sdf$x[row],.a=seq_len(NROW(sdf$a.grid[[row]]))), sdf$a.grid[[row]])
  }))
  empty.action = sapply(ax.grid[3:NCOL(ax.grid)], function(vals) {
    all(vals == "" | is.na(vals))
  })
  g$ax.grid = ax.grid[c("x",".a", names(empty.action)[!empty.action])]

  ax.df = g$ax.grid
  if (!is.null(x.df)) {
    ax.df = inner_join(g$x.df,ax.df, by=c("x"="x"))
  }

  if (is.null(x.df)) {
    x.df = quick_df(x=sdf$x)
  }
  g$x.df = x.df

  # 1b. Add x.T states for capped games
  x.T = rep(NA_character_, NROW(sdf))

  x.T.df = bind_rows(lapply(g_defs$xT_defs, function(def) {
    as_data_frame(def)
  }))
  if (NROW(x.T.df)>0) {
    rows = match(x.T.df$x, sdf$x)
    x.T[rows] = x.T.df$x.T
  }
  sdf$x.T = x.T


  # 2. Evaluate and store payoff matrices
  pi1 = vector("list",nx)
  pi2 = vector("list",nx)


  #def = g_defs$payoff_defs[[1]]
  empty.x = sdf$x
  for (def in g_defs$payoff_defs) {
    def.x = get.def.x(def$x,g)

    x = def.x
    for (x in def.x) {
      row = which(sdf$x == x)
      if (length(row)==0) {
        stop("You specify a payoff for the undefined state ", x)
      }

      state = sdf[row,]
      na = state$na1*state$na2
      pi1[[row]] = rep_len(compute.payoff.for.state(1,state, def,g),na)
      pi2[[row]] = rep_len(compute.payoff.for.state(2,state, def,g),na)
    }
  }

  # Payoff functions
  def = g_defs$payoff_fun_defs[[1]]
  for (def in g_defs$payoff_fun_defs) {
    if (!is.null(def$vec.pi.fun)) {
      args = c(list(ax.df=ax.df),param, def$args)
      res = do.call(def$vec.pi.fun, args)
      check.rel(has.cols(res,c("x","pi1","pi2")),"Your vectorized payoff function vec.pi.fun must return a data frame with the columns 'x','pi1' and 'pi2'")
      check.rel(NROW(res)==NROW(ax.df),"Your vectorized payoff function vec.pi.fun must return a data frame with as many rows as its argument ax.df")

      extra.col = attr(res,"extra.col")
      if (!is.null(extra.col)) {
        if (identical(extra.col,TRUE))
          extra.col = setdiff(colnames(res), c(colnames(ax.df),"pi1","pi2"))
        g$ax.extra = res[,extra.col]
      }


      res = group_by(res,x) %>%
        summarize(pi1=list(pi1),pi2=list(pi2))
      rows = match(res$x, sdf$x)

      pi1[rows] = res$pi1
      pi2[rows] = res$pi2

    } else {
      for (x in def$x) {
        row = which(sdf$x == x)
        a.grid = sdf$a.grid[[row]]
        args = c(get.x.df(x,g, TRUE),list(a.df = a.grid),param, def$args)
        res = do.call(def$pi.fun, args)
        pi1[row] = res["pi1"]
        pi2[row] = res["pi2"]
      }
    }
  }


  sdf$pi1 = pi1
  sdf$pi2 = pi2

  if (compute.just.static) {
    g$sdf = sdf
    return(g)
  }

  # Compute after cap state's payoffs

  after_cap_payoffs = bind_rows(g$default_after_cap_payoffs, bind_rows(lapply(g_defs$after_cap_payoffs_defs,as_data_frame)))

  if (NROW(after_cap_payoffs)>0) {
    rows = which(is.na(after_cap_payoffs$x.T))
    after_cap_payoffs$x.T[rows] = paste0(".#.",rows)
    g$after_cap_payoffs = after_cap_payoffs

  }

  after_cap_actions = lapply(g_defs$after_cap_actions_defs,as_data_frame) %>% bind_rows
  if (NROW(after_cap_actions)>0) {
    g$after_cap_actions = after_cap_actions
  }


  # 3. Evaluate and store state transitions
  # We will store state transitions as a matrix
  # Rows correspond to action profiles and columns to destination states
  # We only include states that can be reached
  # Colnames are the destination states. Values are the transition
  # probabilities.
  # If we have a terminal state, we leave it empty

  # First create a data frame with all transitions in sparse format
  ind = 1
  li = lapply(seq_along(g_defs$trans_defs), function(ind) {
    def = g_defs$trans_defs[[ind]]
    if (is(def$prob,"formula")) {

      df = as_data_frame(def[setdiff(names(df),"prob")])
      env = as.environment(param)
      parent.env(env) = parent.frame()

      df$prob = eval.rel.expression(def$prob,g = g,param = df,enclos=env)
    } else {
      df = as_data_frame(def)
    }
    if (!all(df$xs %in% sdf$x))
      stop("You define a state transition from an undefined state ", paste0(setdiff(df$xs,sdf$x), collapse=", "))

    if (!all(df$xd %in% sdf$x))
      stop("You define a state transition to an undefined state ", paste0(setdiff(df$xd,sdf$x), collapse=", "))

    df$.def.ind = ind
    df
  })

  compile_trans_fun_def = function(ind, field="trans_fun_defs") {
    def = g_defs[[field]][[ind]]

    if (!is.null(def$vec.trans.fun)) {
      args = c(list(ax.df = ax.df),param, def$args)
      res = do.call(def$vec.trans.fun, args)
    } else {
      res = bind_rows(lapply(def$x, function(x) {
        row = which(sdf$x == x)
        a.grid = sdf$a.grid[[row]]
        args = c(get.x.df(x,g, TRUE),list(a.df = a.grid),param, def$args)
        res = do.call(def$trans.fun, args)
        check.rel(has.cols(res,c("xs","xd","prob")), "Your state transition function must return a data frame that has the columns 'xs','xd', 'prob' as well as one column for each action variable that affects the state transitions (purely static actions can be ommited).")
        res

      }))

    }
    res$.def.ind = ind + length(g$trans_defs)
    check.rel(has.cols(res,c("xs","xd","prob")), "Your state transition function must return a data frame that has the columns 'xs','xd', 'prob' as well as one column for each action variable that affects the state transitions (purely static actions can be ommited).")

    res
  }

  li2 = lapply(seq_along(g_defs$trans_fun_defs),compile_trans_fun_def)

  tdf = bind_rows(li,li2) %>%
    filter(xs != xd) %>%
    unique()

  if (NROW(tdf)>0) {
    non.states = setdiff(unique(c(tdf$xd,tdf$xs)),sdf$x)
    check.rel(length(non.states)==0, paste0("You specify state transitions from or to unspecified states ", paste0(non.states, collapse=", "),". Make sure that you have not mis-spelled the state names."))


    sdf$is_terminal = !(sdf$x %in% tdf$xs)
  } else {
    sdf$is_terminal = TRUE
  }
  g$tdf = tdf

  # Result of repeated game assuming the state is fixed
  # This will be a full characterization for all
  # discount factors

  sdf$rep = vector("list",NROW(sdf))
  sdf$trans.mat = vector("list",NROW(sdf))

  g$sdf = sdf


  g$is_compiled =TRUE


  # Compute all transition matrices
  for (row in 1:NROW(sdf)) {
    x = sdf$x[row]
    g$sdf$trans.mat[row] = list(compute.x.trans.mat(x=x,g=g))
  }

  if (isTRUE(g$is.multi.stage)) {
    g = add.rel.multistage.compile(g)
  }

  # Clear definitions to save memory
  g$defs = NULL

  g = prepare.for.spe(g)

  g
}

compute.na = function(A) {
  if (is.data.frame(A)) return(NROW(A))
  prod(sapply(A, length))
}


compute.payoff.for.state = function(player=1,state, def, g) {
  restore.point("compute.payoff.matrix.for.state")
  pi.expr = def[[paste0("pi", player)]]
  if (is.matrix(pi.expr)) {
    return(as.vector(t(pi.expr)))
  }

  if (!is.null(g$x.df)) {
    args = c(as.list(state$a.grid[[1]]),g$param, as.list(g$x.df[state$row,]))
  } else {
    args = c(as.list(state$a.grid[[1]]),g$param,x=state$x)
  }

  pi.val = eval.rel.expression(pi.expr,param=args)
}


#' Creates a new relational contracting game
rel_game = function(name="Game", param=NULL, ...) {

  g = list(name=name,param=list(delta=0.9, rho=0, beta1=0.5),defs=list(),is_compiled=FALSE, ...)
  class(g) = c("relgame","list")
  if (!is.null(param))
    g = rel_param(param)
  g
}


#' Add parameters to a relational contracting game
#'
#' @param g a relational contracting game created with rel_game
#' @param delta The discount factor
#' @param rho The negotiation probability
#' @param ... other parameters that can e.g. be used in payoff functions
#' @return Returns the updated game
rel_param = function(g,..., delta=non.null(param[["delta"]], 0.9), rho=non.null(param[["rho"]], 0), beta1=non.null(param[["beta1"]],1/2), param=g[["param"]]) {

  restore.point("rel_param")
  param = list(delta=delta, rho=rho,beta1=beta1, ...)
  g$param = param
  g
}



#' Add a state to a relational contracting game
#'
#' @param g a relational contracting game created with rel_game
#' @param x The name of the state
#' @param A1 The action set of player 1. Can be a numeric or character vector
#' @param A2 The action set of player 2. Can be a numeric or character vector
#' @param pi1 Player 1's payoff. Value(s) or formula
#' @param pi2 Player 2's payoff. Values(s) or formula
#' @param x.T Relevant when solving a capped game. Which terminal state shall be set in period T onwards. By default, we stay in state x.
#' @return Returns the updated game
rel_state = function(g, x,A1=list(a1=""),A2=list(a2=""), pi1=NULL, pi2=NULL, x.T=NULL) {
  if (!is.list(A1)) A1 = list(a1=A1)
  if (!is.list(A2)) A2 = list(a2=A2)
  if (is.data.frame(x)) {
    x.df = x
    x = x.df$x
  } else {
    x.df = NULL
  }
  if (!is.null(x.df))
    g = add.to.rel.defs(g,"x_df_def", x.df)

  if (!is.null(x.T))
    g = add.to.rel.defs(g, "xT_defs",list(x=x,x.T=x.T))


  g = add.to.rel.defs(g, "state_defs",list(x=x, A1=A1,A2=A2))

  if (!is.null(pi1) | !is.null(pi2)) {
    g = rel_payoff(g,x=x,pi1=pi1,pi2=pi2)
  }
  g
}




#' Add multiple states via functions
#'
#' @param g a relational contracting game created with rel_game
#' @param x The names of the states
#' @param A1 The action set of player 1. Can be a numeric or character vector
#' @param A2 The action set of player 2. Can be a numeric or character vector
#' @param A.fun Alternative to specify fixed A1 and A2, a function that returns action sets
#' @param pi1 Player 1's payoff. Value(s) or formula
#' @param pi2 Player 2's payoff. Values(s) or formula
#' @param pi.fun Alternative to specify pi1 and pi2 as formula, a function that returns for each state a data frame with with pi1 and pi2 and as many rows as action profiles
#' @param vec.pi.fun Alternative to pi.fun. A vectorized function that returns payoffs directly for all combinations of states and action profiles.
#' @param trans.fun A function that specifies state transitions
#' @param vec.trans.fun Alternative to trans.fun, a vectorized function that specifies state transitions
#' @param x.T Relevant when solving a capped game. Which terminal state shall be set in period T onwards. By default, we stay in state x.
#' @return Returns the updated game
rel_states = function(g, x,A1=NULL, A2=NULL, pi1=NULL, pi2=NULL, A.fun=NULL, pi.fun=NULL, vec.pi.fun=NULL, trans.fun=NULL, vec.trans.fun=NULL, static.A.fun=NULL, static.pi.fun=NULL, vec.static.pi.fun=NULL,static.A1=NULL, static.A2=NULL, static.pi1=NULL, static.pi2=NULL,x.T=NULL,  ...) {
  args=list(...)
  restore.point("rel_states")
  if (is.data.frame(x)) {
    x.df = x
    x = x.df$x
  } else {
    x.df = NULL
  }
  if (!is.null(x.df))
    g = add.to.rel.defs(g,"x_df_def", x.df)

  if (!is.null(x.T))
    g = add.to.rel.defs(g, "xT_defs",list(x=x,x.T=x.T))

  if (!is.null(A1) | !is.null(A2)) {
    g = add.to.rel.defs(g, "state_defs",list(x=x, A1=A1,A2=A2))
  }


  if (!is.null(A.fun)) {
    obj = list(x=x,A.fun=A.fun, args=args)
    g = add.to.rel.defs(g, "state_fun_defs",obj)
  }

  if (!is.null(pi1) & !is.null(pi2)) {
    g = rel_payoff(g,x=x,pi1=pi1,pi2=pi2)
  } else if (!is.null(pi.fun) | !is.null(vec.pi.fun)) {
    obj = list(x=x,pi.fun=pi.fun, vec.pi.fun=vec.pi.fun, args=args)
    g = add.to.rel.defs(g, "payoff_fun_defs",obj)
  } else {
    stop("You must specify either pi1 and pi2 or pi.fun or vec.pi.fun.")
  }

  if (!is.null(trans.fun) | !is.null(vec.trans.fun)) {
    obj = list(x=x,trans.fun=trans.fun, vec.trans.fun=vec.trans.fun, args=args)
    g = add.to.rel.defs(g, "trans_fun_defs",obj)
  }

  if (!is.null(static.A.fun) | !is.null(static.A1) | !is.null(static.A2)){
    g$is.multi.stage = TRUE
    gs = list(defs=list())

    if (!is.null(static.A.fun)) {
      obj = list(x=x,A.fun=static.A.fun, args=args)
      gs = add.to.rel.defs(gs,"state_fun_defs",obj)
    } else if (!is.null(static.A1) | !is.null(static.A2)) {
      obj = list(x=x,A1=static.A1, A2=static.A2, args=args)
      gs = add.to.rel.defs(gs,"state_defs",obj)
    }

    if (!is.null(static.pi.fun) | !is.null(vec.static.pi.fun)) {
      obj = list(x=x,pi.fun=static.pi.fun, vec.pi.fun=vec.static.pi.fun, args=args)
      gs = add.to.rel.defs(gs, "payoff_fun_defs",obj)
    } else if (!is.null(static.pi1) & !is.null(static.pi2)) {
      obj = list(x=x,pi1=static.pi1, pi2=static.pi2, args=args)
      gs = add.to.rel.defs(gs, "payoff_defs",obj)
    } else {
      stop("You must specify either static.pi1 and static.pi2 or static.pi.fun or vec.static.pi.fun.")
    }

    g$defs$gs = gs
  }

  g
}

#' Add a payoff function to one or several states
#'
#' @param g a relational contracting game created with rel_game
#' @param x Name of one or multiple states. If NULL assume the payoff function holds for all states
#' @param pi1 Player 1's payoff. Value(s) or formula
#' @param pi2 Player 2's payoff. Values(s) or formula
#' @return Returns the updated game
rel_payoff = function(g, x=NULL,pi1=0,pi2=0) {
  obj = list(x=x,pi1=pi1,pi2=pi2)
  add.to.rel.defs(g, "payoff_defs",obj)
}

#' Version of rel_payoff that takes a function
rel_payoff_fun = function(g,pi.fun=NULL,x=NULL,vec.pi.fun=NULL,...) {
  obj = list(x=x,pi.fun=pi.fun,type="fun", vec.pi.fun=vec.pi.fun)
  add.to.rel.defs(g, "payoff_fun_defs",obj)
}



#' Add a state transition from one state to one or several states
#'
#' @param g a relational contracting game created with rel_game
#' @param xs Name(s) of source states
#' @param xd Name(s) of destination states
#' @param ... named action and their values
#' @param prob transition probability
#' @return Returns the updated game
rel_transition = function(g, xs,xd,...,prob=1) {
  obj = list(xs=xs,xd=xd,..., prob=prob)
  add.to.rel.defs(g, "trans_defs",obj)
}

#' Add a state transition function
#'
#' @param g a relational contracting game created with rel_game
#' @param xs Name(s) of source states
#' @param xd Name(s) of destination states
#' @param ... named action and their values
#' @param prob transition probability
#' @return Returns the updated game
rel_transition_fun = function(g, trans.fun,x=NULL,..., vectorized=FALSE) {
  obj = list(x=x,trans.fun=trans.fun,vectorized=vectorized, args=list(...))
  add.to.rel.defs(g, "trans_fun_defs",obj)
}

#' Specify the SPE payoff set(s) of the truncated game(s) after a cap in period T. While we could specify a complete repeated game that is played after the cap, it also suffices to specify just an SPE payoff set of the truncated game of the after-cap state.
#' @param g a relational contracting game created with rel_game
#' @param x The state(s) for which this after-cap payoff set is applied. If NA (default) and also x.T is NA, it applies to all states.
#' @param U The highest joint payoff in the truncated repeated game starting from period T.
#' @param v1 The lowest SPE payoff of player 1 in the truncated game. These are average discounted payoffs using delta as discount factor.
#' @param v2 Like v1, but for player 2.
#' @param v1.rep Alternative to v1. Player 1 lowest SPE payoff in the repeated game with adjusted discount factor delta*(1-rho). Will be automatically converted into v1_trunc based on rho, delta, and bargaining weight. Are often easier to specify.
#' @param v2.rep Like v1.rep, but for player 2.
#' @param x.T Instead of specifiying the argument x, we can specify as x.T a name of the after-cap state. This can be  refereed to as the argument x.T in rel_state and rel_states
#' @return Returns the updated game
rel_after_cap_payoffs = function(g,x=NA, U, v1=NA, v2=NA, v1.rep=NA, v2.rep=NA, x.T=NA) {
  restore.point("rel_after_cap_payoffs")

  if (is.na(v1.rep) & is.na(v1))
    stop("You must specify either v1 or v1.rep.")
  if (is.na(v2.rep) & is.na(v2))
    stop("You must specify either v2 or v2.rep.")

  def = list(U=U, v1=v1, v2=v2, v1.rep=v1.rep, v2.rep=v2.rep,x=x,x.T=x.T)
  if (is.na(x)) {
    g$default_after_cap_payoffs = def
  } else {
    g = add.to.rel.defs(g,"after_cap_payoffs_defs",def)
  }

  g
}


#' Fix action profiles for the equilibrium path (ae) and during punishment (a1.hat and a2.hat) that are assumed to be played after the cap in period T onwards. The punishment profile a1.hat is the profile in which player 1 already plays a best-reply (in a1 he might play a non-best reply). From the specified action profiles in all states, we can compute the relevant after-cap payoffs U(x), v1(x) and v2(x) assuming that state transitions would continue.
#' @param g a relational contracting game created with rel_game
#' @param x The state(s) for which this after-cap payoff set is applied. If NA (default) and also x.T is NA, it applies to all states.
#' @param ae A named list that specifies the equilibrum action profiles.
#' @param a1.hat A named list that specifies the action profile when player 1 is punished.
#' @param a2.hat A named list that specifies the action profile when player 2 is punished.
#' @param x.T Instead of specifiying the argument x, we can specify as x.T a name of the after-cap state. This can be  refereed to as the argument x.T in rel_state and rel_states
#' @return Returns the updated game
rel_after_cap_actions = function(g,x=NA, ae, a1.hat,a2.hat, x.T=NA) {
  restore.point("rel_after_cap_actions")
  def = nlist(ae=list(ae),a1.hat=list(a1.hat),a2.hat=list(a2.hat),x=x,x.T=x.T)
  add.to.rel.defs(g,"after_cap_actions_defs",def)
}



print.relgame = function(g,...) {
  cat(paste0("\nRelational Contracting Game ", g$name))
  cat(paste0("\n",length(g$states), " states"))
  x = g
  class(x) = "list"
  print(x)
}

add.to.rel.defs = function(g, var, obj) {
  g[["defs"]][[var]] = c(g[["defs"]][[var]],list(obj))
  g
}


add.to.rel.list = function(li, var, obj) {
  li[[var]] = c(li[[var]],list(obj))
  li
}

eval.rel.expression = function(e,g=NULL, param=g$param, vectorized=FALSE, null.value = NULL, enclos=parent.frame()) {
  restore.point("eval.rel.expression")
  if (is.data.frame(e)) return(e)


  if (is.list(e))
    return(lapply(e,eval.rel.expression, g=g, param=param, vectorized=vectorized, null.value=null.value, enclos=enclos))

  if (class(e)=="formula")
    e = e[[2]]

  if (is.null(param)) param = list()
  if (class(e)=="call" | class(e)=="expression" | class(e)=="name") {
    e = eval(e,param,enclos = enclos)
  }

  if (is.function(e))
    stop("Not yet implemented for functions")

  if (length(e)==0 & !is.null(null.value))
    e = null.value
  return(e)

}


compute.x.trans.mat = function(x,g, add.own=TRUE, tdf=g$tdf, row= which(g$sdf$x == x)) {
  restore.point("compute.x.trans.mat")
  #if (x=="x0") stop()
  if (NROW(tdf)==0) {
    empty = matrix(0,0,1)
    colnames(empty) = x
    return(empty)
  }

  df = tdf[tdf$xs==x,]
  if (NROW(df)==0) {
    empty = matrix(0,0,1)
    colnames(empty) = x
    return(empty)
  }
  xd = unique(df$xd)

  a.grid = g$sdf$a.grid[[row]]

  actions = intersect(colnames(a.grid), colnames(df))

  # Transition matrix: rows action profiles, cols = destination cols
  mat = matrix(0,NROW(a.grid), length(xd))
  colnames(mat) = xd

  mat.cols = seq_along(xd)
  names(mat.cols) = xd

  inds = unique(df$.def.ind)
  ind = inds[1]

  # Needed for inner_join later
  a.grid$.a = seq_len(NROW(a.grid))
  for (ind in inds) {
    d = df[df$.def.ind == ind,,drop=FALSE]
    act = actions[as.vector(!is.na(d[1,actions]))]

    if (length(act)==0) {
      # Set same transition probability for all actions
      new.mat = matrix(d$prob, NROW(mat), NROW(d), byrow = TRUE)
      mat[, d$xd] = new.mat
    } else {
      # In general we have an n to m matching
      # between rows in d and rows in mat (a.grid)
      temp.df = inner_join(d, a.grid[,c(act,".a")], by=act)
      xd.cols = match(temp.df$xd, colnames(mat))
      grid = cbind(temp.df$.a,xd.cols)
      mat[grid] = temp.df$prob
    }
  }
  sumProbs = rowSums(mat)
  if (any(mat<0))
    stop(paste0("Have computed negative transition probabilities for state ", x))
  if (any(sumProbs-1>1e-10))
    stop(paste0("Have computed sum of transition probabilities larger than 1 for state ",x))

  if (add.own) {
    if (any(sumProbs<1)) {
      mat = cbind(.own = 1-sumProbs, mat)
      colnames(mat)[1] = x
    }
  }
  mat
}

