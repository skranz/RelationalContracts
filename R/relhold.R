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

  solve_x_repgame(g,"xL")

  g

}

get.x.df = function(x,g) {
  if (is.null(g$x.df)) return(NULL)

  left_join(quick_df(x=x), g$x.df, by="x")
}

get.def.x = function(x,g,x.df=g$x.df, sdf=g$sdf) {
  if (!is.null(x)) return(x)
  if (!is.null(x.df)) return(x.df$x)
  if (!is.null(sdf)) return(sdf$x)
  NULL
}

#' Compiles a relational contracting game
rel_compile = function(g,...) {
  restore.point("rel_compile")

  # 1. Create a data frame with all states
  #def = g$state_defs[[2]]

  if (!is.null(g$x_df_def)) {
    g$x.df = bind_rows(g$x_df_def)
  } else {
    g$x.df = NULL
  }


  li = lapply(g$state_defs, function(def) {
    A1 = eval.rel.expression(def$A1,g, null.value = "")
    A2 = eval.rel.expression(def$A2,g, null.value = "")
    na1 = prod(sapply(A1, length))
    na2 = prod(sapply(A2, length))

    a.grid = as_data_frame(expand.grid(c(A1,A2),stringsAsFactors = FALSE))
    if (length(def$x)==1) {
      state = quick_df(x=def$x,na1=na1,na2=na2, A1=list(A1),A2=list(A2),a.grid=list(a.grid))
    } else {
      stop("rel_state with multiple x not yet implemented.")
    }
  })

  # Compute states defined by A.fun
  def = g$state_fun_defs[[1]]
  li2 = lapply(g$state_fun_defs, function(def) {
    def$x = get.def.x(def$x,g)

    if (def$vectorized) {
      args = c(list(x=x, x.df=(get.x.df(x,g))),g$param, def$args)
      res = do.call(def$A.fun,args)

      na1 = sapply(res$A1, function(A1) prod(sapply(A1, length)))
      na2 = sapply(res$A2, function(A2) prod(sapply(A2, length)))
      state = quick_df(x=x,na1=na1,na2=na2,A1=res$A1,A2=res$A2)
    } else {
      A1 = vector("list", length(def$x))
      A2 = vector("list", length(def$x))
      na1 = na2 = rep(NA, length(def$x))
      for (row in seq_along(def$x)) {
        args = c(list(x.df=(get.x.df(def$x[row],g)),x=def$x[row]),g$param, def$args)
        res = do.call(def$A.fun, args)
        A1[row] = list(res$A1)
        A2[row] = list(res$A2)
        na1[row] = prod(sapply(res$A1, length))
        na2[row] = prod(sapply(res$A2, length))

      }
      state = quick_df(x=def$x,na1=na1,na2=na2,A1=A1,A2=A2)
    }

    state$a.grid = lapply(seq_len(NROW(state)), function(row) {
      a.grid = as_data_frame(expand.grid(c(state$A1[[row]],state$A2[[row]]),stringsAsFactors = FALSE))
    })
    state
  })

  sdf = bind_rows(li,li2)

  g$a.labs.df = bind_rows(lapply(seq_len(NROW(sdf)),function(row) {
    quick_df(x=sdf$x[row],a = seq_len(NROW(sdf$a.grid[[row]])), lab=make.state.lab.a(sdf[row,]))
  }))

  nx = NROW(sdf)


  # 2. Evaluate and store payoff matrices
  pi1 = vector("list",nx)
  pi2 = vector("list",nx)


  #def = g$payoff_defs[[1]]
  empty.x = sdf$x
  for (def in g$payoff_defs) {
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
  def = g$payoff_fun_defs[[1]]
  for (def in g$payoff_fun_defs) {
    for (x in def$x) {
      row = which(sdf$x == x)
      a.grid = sdf$a.grid[[row]]
      args = c(list(x=x, x.df=get.x.df(x,g), a.df = a.grid),g$param, def$args)
      res = do.call(def$pi.fun, args)
      pi1[row] = res["pi1"]
      pi2[row] = res["pi2"]
    }
  }


  sdf$pi1 = pi1
  sdf$pi2 = pi2

  # 3. Evaluate and store state transitions
  # We will store state transitions as a matrix
  # Rows correspond to action profiles and columns to destination states
  # We only include states that can be reached
  # Colnames are the destination states. Values are the transition
  # probabilities.
  # If we have a terminal state, we leave it empty

  # First create a data frame with all transitions in sparse format
  ind = 1
  li = lapply(seq_along(g$trans_defs), function(ind) {
    def = g$trans_defs[[ind]]
    if (is(def$prob,"formula")) {

      df = as_data_frame(def[setdiff(names(df),"prob")])
      env = as.environment(g$param)
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

  li2 = lapply(seq_along(g$trans_fun_defs), function(ind) {
    def = g$trans_fun_defs[[ind]]
    res = bind_rows(lapply(def$x, function(x) {
      row = which(sdf$x == x)
      a.grid = sdf$a.grid[[row]]
      args = c(list(x=x, x.df=get.x.df(x,g), a.df = a.grid),g$param, def$args)
      res = do.call(def$trans.fun, args)
    }))
    res$.def.ind = ind + length(g$trans_defs)
    res
  })

  tdf = bind_rows(li,li2) %>%
    filter(xs != xd) %>%
    unique()

  sdf$is_terminal = !(sdf$x %in% tdf$xs)

  # Result of repeated game assuming the state is fixed
  # This will be a full characterization for all
  # discount factors
  sdf$rep = vector("list",NROW(sdf))

  sdf$trans.mat = vector("list",NROW(sdf))


  ax.grid = bind_rows(lapply(seq_len(NROW(sdf)), function(row) {
    cbind(quick_df(.x=sdf$x[row],.a=seq_len(NROW(sdf$a.grid[[row]]))), sdf$a.grid[[row]])
  }))
  empty.action = sapply(ax.grid[3:NCOL(ax.grid)], function(vals) {
    all(vals == "" | is.na(vals))
  })
  g$ax.grid = ax.grid[c(".x",".a", names(empty.action)[!empty.action])]


  g$tdf = tdf
  g$sdf = sdf

  # Compute adjusted discount factor
  g$param$adj_delta = g$param$delta * (1-g$param$rho)

  g$is_compiled =TRUE


  # Compute all transition matrices
  for (row in 1:NROW(sdf)) {
    x = sdf$x[row]
    g$sdf$trans.mat[row] = list(compute.x.trans.mat(x=x,g=g))
  }



  g
}

compute.payoff.for.state = function(player=1,state, def, g) {
  restore.point("compute.payoff.matrix.for.state")
  args = c(as.list(state$a.grid[[1]]),g$param)
  pi.expr = def[[paste0("pi", player)]]
  pi.val = eval.rel.expression(pi.expr,param=args)
}

#' Creates a new relational contracting game
rel_game = function(name="Game",...) {
  g = list(name=name,param=NULL,state_defs=list(), payoff_defs=list(), trans_defs=list(),is_compiled=FALSE,...)
  class(g) = c("relgame","list")
  g
}


#' Add parameters to a relational contracting game
#'
#' @param g a relational contracting game created with rel_game
#' @param delta The discount factor
#' @param rho The negotiation probability
#' @param ... other parameters that can e.g. be used in payoff functions
#' @return Returns the updated game
rel_param = function(g, delta=non.null(param[["delta"]], 0.9), rho=non.null(param[["rho"]], 0), beta1=non.null(param[["beta1"]],1/2), param=g[["param"]],...) {
  restore.point("rel_param")
  param = list(delta=delta, rho=rho,beta1=beta1, beta2=1-beta1,...)
  g$param = param
  g
}



#' Add a state to a relational contracting game
#'
#' @param g a relational contracting game created with rel_game
#' @param x The name of the state
#' @param A1 The action set of player 1. Can be a numeric or character vector
#' @param A2 The action set of player 2. Can be a numeric or character vector
#' @return Returns the updated game
rel_state = function(g, x,A1=list(a1=""),A2=list(a2="")) {
  if (!is.list(A1)) A1 = list(a1=A1)
  if (!is.list(A2)) A2 = list(a2=A2)
  if (is.data.frame(x)) {
    x.df = x
    x = x.df$x
  } else {
    x.df = NULL
  }
  if (!is.null(x.df))
    g = add.to.rel.list(g,"x_df_def", x.df)


  add.to.rel.list(g, "state_defs",list(x=x, A1=A1,A2=A2) )
}

#' Add multiple states via functions
#'
#' @param g a relational contracting game created with rel_game
#' @param x The names of the states
#' @param A.fun A function that returns action sets
#' @param A.fun.vec Vectorized version
#' @param A2 The action set of player 2. Can be a numeric or character vector
#' @return Returns the updated game
rel_states_fun = function(g, x,A.fun=NULL, pi.fun=NULL, trans.fun=NULL, vectorized=FALSE, ...) {
  args=list(...)
  restore.point("rel_state_fun")
  if (is.data.frame(x)) {
    x.df = x
    x = x.df$x
  } else {
    x.df = NULL
  }
  if (!is.null(x.df))
    g = add.to.rel.list(g,"x_df_def", x.df)

  if (!is.null(A.fun)) {
    obj = list(x=x,A.fun=A.fun, args=args, vectorized=vectorized)
    g = add.to.rel.list(g, "state_fun_defs",obj)
  }
  if (!is.null(pi.fun)) {
    obj = list(x=x,pi.fun=pi.fun, args=args, vectorized=vectorized)
    g = add.to.rel.list(g, "payoff_fun_defs",obj)
  }
  if (!is.null(trans.fun)) {
    obj = list(x=x,trans.fun=trans.fun, args=args, vectorized=vectorized)
    g = add.to.rel.list(g, "trans_fun_defs",obj)
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
rel_payoff = function(g, x=NULL,pi1=NULL,pi2=NULL) {
  obj = list(x=x,pi1=pi1,pi2=pi2)
  add.to.rel.list(g, "payoff_defs",obj)
}

#' Version of rel_payoff that takes a function
rel_payoff_fun = function(g,pi.fun,x=NULL, vectorized=FALSE,...) {
  obj = list(x=x,pi.fun=pi.fun,type="fun", vectorized=vectorized)
  add.to.rel.list(g, "payoff_fun_defs",obj)
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
  add.to.rel.list(g, "trans_defs",obj)
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
  add.to.rel.list(g, "trans_fun_defs",obj)
}



print.relgame = function(g,...) {
  cat(paste0("\nRelational Contracting Game ", g$name))
  cat(paste0("\n",length(g$states), " states"))
  x = g
  class(x) = "list"
  print(x)
}

add.to.rel.list = function(g, var, obj) {
  g[[var]] = c(g[[var]],list(obj))
  g
}

eval.rel.expression = function(e,g=NULL, param=g$param, vectorized=FALSE, null.value = NULL, enclos=parent.frame()) {
  restore.point("eval.rel.expression")

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




compute.x.trans.mat = function(x,g, add.own=TRUE) {
  restore.point("compute.x.trans.mat")
  #if (x=="0_0") stop()
  df = g$tdf[g$tdf$xs==x,]
  if (NROW(df)==0)
    return(NULL)

  xd = unique(df$xd)

  row = which(g$sdf$x == x)

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

