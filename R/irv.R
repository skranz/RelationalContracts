# Helper functions to specify transition probabilities

examples.joint_independent = function() {

}

#' Helper functions to specify state transitions
#'
#' To be used as argument of \code{\link{irv_joint_dist}}
#'
#' See vignette for examples
irv = function(var, ...,default=NULL, lower=NULL, upper=NULL,  vals.unique=TRUE) {
  val.df = bind_rows(list(...))
  default = substitute(default)
  list(var=var, default=default, lower=lower, upper=upper, val.df = val.df, vals.unique=vals.unique)
}

#' Helper functions to specify state transitions
#'
#' To be used as argument of \code{\link{irv}}
#'
#' See vignette for examples
irv_val = function(val, prob) {
  val = substitute(val)
  prob = substitute(prob)
  quick_df(val=list(val), prob=list(prob))
}

#' Helper function to specify state transitions
#'
#' See vignette for examples
irv_joint_dist = function(df, ..., enclos=parent.frame(), remove.zero.prob=TRUE, prob.var = "prob") {
  var.defs = list(...)
  restore.point("irv_joint_dist")
  res = df
  res[[prob.var]] = 1.
  def = var.defs[[1]]
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
        stop("For some rows of your data set the transition probabilities don't add up to 1 and no default value is set.")
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

