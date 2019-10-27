# Helper functions to specify transition probabilities

examples.itrans = function() {
  dp = 0.1
  k.step=5
  x.max = 10
  ax.df = quick_df(x1=c(1,1,1,1), x2=c(0,0,0,0),i1=c(0,0,1,1),i2=c(0,1,0,1))
  ax.df = mutate(ax.df,
    i1.prob = i1 / (1+i1),
    i2.prob = i2 / (1+i2)
  )

  ax.df =

  trans = independent_transitions(ax.df,
    trans_var("nx1",default=x1,lower=0, upper=x.max,
      trans_val(x1+k.step, (1-dp)*i1.prob),
      trans_val(x1-k.step, dp*(1-i1.prob))
    ),
    trans_var("nx2",default=x2, lower=0, upper=x.max,
      trans_val(x2+k.step, (1-dp)*i2.prob),
      trans_val(x2-k.step, dp*(1-i2.prob))
    )
  )
  trans


  independent_transitions(


  )

  trans = independent_transitions(ax.df,
    itrans(g1=1,  (i1==1 & x1<x.max) * (1-dp)),
    itrans(g1=0,  (i1==1) * dp + (i1==0 | x1==x.max & i1==1)*(1-dp)),
    itrans(g1=-1, (i1==0) * dp),
    itrans(g2=1,  (i2==1 & x2<x.max) * (1-dp)),
    itrans(g2=0,  (i2==1) * dp + (i2==0 | x2==x.max & i2==1)*(1-dp)),
    itrans(g2=-1, (i2==0) * dp)
  )

  trans = independent_transitions(ax.df,
    itrans(g1=1,  (i1==1 & x1<x.max) * (1-dp)),
    itrans(g1=0,  NA),
    itrans(g1=-1, (i1==0 & x1>0) * dp),
    itrans(g2=1,  (i2==1 & x2<x.max) * (1-dp)),
    itrans(g2=0,  NA),
    itrans(g2=-1, (i2==0 & x2>0) * dp)
  )


  itrans(x=2, a*0.2)

}

#' Helper functions to specify state transitions
#'
#' To be used as argument of \code{\link{independent_transitions}}
#'
#' See vignette for examples
trans_var = function(var, ...,default=NULL, lower=NULL, upper=NULL,  vals.unique=TRUE) {
  val.df = bind_rows(list(...))
  default = substitute(default)
  list(var=var, default=default, lower=lower, upper=upper, val.df = val.df, vals.unique=vals.unique)
}

#' Helper functions to specify state transitions
#'
#' To be used as argument of \code{\link{trans_var}}
#'
#' See vignette for examples
trans_val = function(val, prob) {
  val = substitute(val)
  prob = substitute(prob)
  quick_df(val=list(val), prob=list(prob))
}

#' Helper function to specify state transitions
#'
#' See vignette for examples
independent_transitions = function(ax.df, ..., enclos=parent.frame(), remove.zero.prob=TRUE, prob.var = "prob") {
  var.defs = list(...)
  restore.point("independent_transitions")
  res = ax.df
  res[[prob.var]] = 1.
  def = var.defs[[2]]
  for (def in var.defs) {
    res$.row.id = seq_len(NROW(res))
    vals.unique = def$vals.unique
    var = def$var
    val.df = def$val.df
    sum.prob = rep(0,NROW(res))
    res.li = vector("list", NROW(val.df)+1*(!is.null(def$default)))
    for (row in seq_len(NROW(val.df))) {
      nres = res
      nres[[var]] = eval(val.df$val[[row]], res, enclos)
      new.prob = eval(val.df$prob[[row]],res, enclos)
      sum.prob = sum.prob + new.prob
      nres[[prob.var]] = res[[prob.var]]*new.prob
      if (remove.zero.prob) {
        nres = nres[nres[[prob.var]]>0,,drop=FALSE]
      }
      res.li[[row]] = nres
    }
    if (any(sum.prob<1)) {
      if (is.null(def$default)) {
        stop("For some ax combinations the transition probabilities don't add up to 1 and no default value is set.")
      }
      nres = res
      nres[[var]] = eval(def$default, res, enclos)
      new.prob = 1-sum.prob
      nres[[prob.var]] = res[[prob.var]]*new.prob
      if (remove.zero.prob) {
        nres = nres[nres[[prob.var]]>0,,drop=FALSE]
      }
      res.li[[length(res.li)]] = nres
    }
    res = bind_rows(res.li)
    # Set lower bound
    if (!is.null(def$lower)) {
      rows = which(res[[var]] < def$lower)
      if (length(rows)>0) {
        vals.unique=FALSE
        res[[var]][rows] = def$lower
      }
    }
    # Set upper bound
    if (!is.null(def$upper)) {
      rows = which(res[[var]] > def$upper)
      if (length(rows)>0) {
        vals.unique=FALSE
        res[[var]][rows] = def$upper
      }
    }

    # Sum probabilities of rows with same var value
    if (!vals.unique) {
      sum.res = res %>% group_by_at(c(".row.id", var)) %>%
        mutate(prob = sum(prob)) %>%
        slice(1L) %>%
        ungroup()
      res = sum.res
    }
  }
  res = res[,setdiff(colnames(res),".row.id")]
  res

}



itrans = function(...) {
  res = dplyr:::dots(...)
  quick_df(var = names(res)[1],val = eval(res[[1]]), prob=res[2])
}



compute.eq.trans.mat = function(g, ae = eq$ae, eq=g[["eq"]]) {
  restore.point("compute.eq.trans.mat")
  if (!is.null(g$ax.trans)) {
    ax = eq.a.to.ax(g,a=ae)
    g$ax.trans[ax,,drop=FALSE]
  } else {
    nx = NROW(g$sdf)
    mat = matrix(0,nx,nx)
    colnames(mat) = g$sdf$x
    for (xrow in 1:NROW(g$sdf)) {
      tm = g$sdf$trans.mat[[xrow]]
      if (NROW(tm)==0) {
        mat[xrow, colnames(tm)] = 1
      } else {
        mat[xrow, colnames(tm)] = tm[ae[xrow],]
      }
    }
    mat
  }

}



stationary.eq.distribution = function(g, eq=g[["eq"]], tol = 1e-10, start=rep(1/NROW(g$sdf), NROW(g$sdf)), iterations=200, ae = eq$ae) {
  restore.point("stationary.eq.distribution")
  mat = compute.eq.trans.mat(g, eq=eq, ae=ae)
  res = start
  for (i in 1:iterations) {
    nres = res %*% mat
    change = max(abs(res-nres))
    res = nres
  }
  res = as.vector(res)
  attr(res,"change") <- change
  res
}


