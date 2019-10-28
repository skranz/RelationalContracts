examples.eq_diagram = function() {




  x.max = 3
  x.df = as_data_frame(expand.grid(x1=0:x.max,x2=0:x.max))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)

  g = rel_game("Arms Race") %>%
    rel_param(delta=0.99, rho=0.9, c=0, k=0.1,x.max=x.max, success.prob=0.5) %>%
    rel_states(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun) %>%
    rel_compile() %>%
    rel_capped_rne(T=50)

  eq_diagram(g,t=1)

  rel_diagram(g)

  cat(rel.mermaid.code(g))


}

#' Draws a diagram of equilibrium state transition
#'
#' Draws an arrow from state x to state y if and
#' only if on the equilibrium path there is a positive
#' probability to directly transist from x to y.
#'
#' @param g The solved game object
#' @param show.own.loop Shall a loop from a state to itself be drawn if there is a positive probability to stay in the state? (Default=FALSE)
#' @param show.terminal.loop Only relevant if \code{show.own.loop = TRUE}. If still \code{show.terminal.loop=FALSE} omit loops in terminal state that don't transist to any other state.
#' @param use.x optionally a vector of state ids that shall only be shown.
#' @param just.eq.chain If TRUE only show states that can be reached with positive probability on the equilibrium path when starting from state x0.
#' @param x0 only relevant if \code{just.eq.chain=TRUE}. The ID of the x0 state. By default the first defined state.
#' @param label.fun An optional function that takes the equilibrium object and game and returns a character vector that contains a label for each state.
#' @param tooltip.fun Similar to \code{label.fun} but for the tooltip shown on a state.
#' @param return.dfs if TRUE don't show diagram but only return the relevant edge and node data frames that can be used to call \code{DiagrammeR::create_graph}. Useful if you want to manually customize graphs further.
eq_diagram = function(g,show.own.loop=FALSE, show.terminal.loop=FALSE, use.x=NULL, just.eq.chain=FALSE,x0=g$sdf$x[1], hide.passive.edge=TRUE,  label.fun=NULL, tooltip.fun=NULL, active.edge.color="#000077", passive.edge.color="#dddddd", passive.edge.width=1,  return.dfs=FALSE,eq = g[["eq"]]) {
  restore.point("eq_diagram")

  library(DiagrammeR)

  is.multi.stage = isTRUE(g$is.multi.stage)
  rne = eq

  sdf=g$sdf

  if (just.eq.chain) {
    chain.x = find.eq.chain.x(g,x0 = x0,eq = rne, t=NULL)
    if (!is.null(use.x)) {
      use.x = intersect(use.x, chain.x)
    } else {
      use.x = chain.x
    }
  }
  if (!is.null(use.x)) {
    rne = rne[rne$x %in% use.x,]
    sdf = sdf[sdf$x %in% use.x,]
  }
  n = NROW(sdf)
  if (is.null(label.fun)) {
    lab = paste0(sdf$x, " \n", round(rne$r1,2), " ", round(rne$r2,2))
  } else {
    lab = label.fun(rne, g)
  }
  if (is.null(tooltip.fun)) {
    tooltip = paste0(
      rne$x,
      "<br>ae: ",rne$ae.lab,
      "<br>a1: ", rne$a1.lab, "<br>a2: ", rne$a2.lab,
      "<br>v1=",round(rne$v1,2)," v2=",round(rne$v2,2),
      "<br>U=", round(rne$U,2))
  } else {
    tooltip = tooltip.fun(rne,g)
  }

  ndf = tibble(id=1:n, label=lab, type="node", shape="box", title=tooltip, x=rne$x)

  # Create edges
  tr = lapply(seq_len(NROW(sdf)), function(row) {
    trans.mat = sdf$trans.mat[[row]]
    x = sdf$x[[row]]

    rne.row = which(rne$x==x)[1]
    ae = rne$ae[[rne.row]]
    if (NROW(trans.mat)==0) {
      if (!show.terminal.loop)
        return(NULL)
      dest = x
      is.ae.dest = TRUE
    } else {
      dest = colnames(trans.mat)
      is.ae.dest = trans.mat[ae,]>0
    }
    dest.rows = match(dest, sdf$x)

    edges = quick_df(from=row, to=dest.rows, rel="", color=ifelse(is.ae.dest, active.edge.color,passive.edge.color), width=ifelse(is.ae.dest,2,passive.edge.width), hidden=!is.ae.dest & hide.passive.edge)

    if (just.eq.chain)
      edges = edges[is.ae.dest,,drop=FALSE]
    edges
  })
  edf = bind_rows(tr)
  if (!show.own.loop)
    edf = filter(edf, from != to)

  if (return.dfs) return(list(ndf=ndf, edf=edf, sdf=sdf, rne=rne))

  graph = create_graph(select(ndf,-x), edf)

  render_graph(graph, output="visNetwork")
}


#' Draws a diagram of equilibrium state transition
#'
#' Draws an arrow from state x to state y if and
#' only if on the equilibrium path there is a positive
#' probability to directly transist from x to y.
#'
#' @param g The solved game object
#' @param show.own.loop Shall a loop from a state to itself be drawn if there is a positive probability to stay in the state? (Default=FALSE)
#' @param show.terminal.loop Only relevant if \code{show.own.loop = TRUE}. If still \code{show.terminal.loop=FALSE} omit loops in terminal state that don't transist to any other state.
#' @param use.x optionally a vector of state ids that shall only be shown.
#' @param just.eq.chain If TRUE only show states that can be reached with positive probability on the equilibrium path when starting from state x0.
#' @param x0 only relevant if \code{just.eq.chain=TRUE}. The ID of the x0 state. By default the first defined state.
#' @param label.fun An optional function that takes the equilibrium object and game and returns a character vector that contains a label for each state.
#' @param tooltip.fun Similar to \code{label.fun} but for the tooltip shown on a state.
#' @param return.dfs if TRUE don't show diagram but only return the relevant edge and node data frames that can be used to call \code{DiagrammeR::create_graph}. Useful if you want to manually customize graphs further.
eq_diagram_xgroup = function(g,show.own.loop=FALSE, show.terminal.loop=FALSE, use.x=NULL, just.eq.chain=FALSE,x0=g$sdf$x[1], hide.passive.edge=TRUE,  label.fun=NULL, tooltip.fun=NULL, active.edge.color="#000077", passive.edge.color="#dddddd", passive.edge.width=1,  return.dfs=FALSE,eq = g[["eq"]]) {
  restore.point("eq_diagram_xgroup")

  library(DiagrammeR)

  is.multi.stage = isTRUE(g$is.multi.stage)
  rne = eq

  sdf=g$sdf
  sdf$xgroup = g$x.df$xgroup

  if (just.eq.chain) {
    chain.x = find.eq.chain.x(g,x0 = x0,eq = rne, t=NULL)
    if (!is.null(use.x)) {
      use.x = intersect(use.x, chain.x)
    } else {
      use.x = chain.x
    }
  }
  if (!is.null(use.x)) {
    rne = rne[rne$x %in% use.x,]
    sdf = sdf[sdf$x %in% use.x,]
  }

  sdf = sdf

  dupl = duplicated(sdf$xgroup)
  gsdf = sdf[!dupl,]

  n = NROW(gsdf)
  ndf = tibble(id=1:n, label=gsdf$xgroup, type="node", shape="box", title=gsdf$xgroup, xgroup=gsdf$xgroup)

  x_to_xgroup = sdf$xgroup
  names(x_to_xgroup) = sdf$x
  xgroups = gsdf$xgroup
  xgroups_num = seq_along(xgroups)
  names(xgroups_num) = xgroups


  # Create edges
  tr = lapply(seq_len(NROW(sdf)), function(row) {
    trans.mat = sdf$trans.mat[[row]]
    x = sdf$x[[row]]
    xgroup = sdf$xgroup[[row]]

    rne.row = which(rne$x==x)[1]
    ae = rne$ae[[rne.row]]
    if (NROW(trans.mat)==0) {
      if (!show.terminal.loop)
        return(NULL)
      dest = x
      is.ae.dest = TRUE
    } else {
      dest = colnames(trans.mat)
      is.ae.dest = trans.mat[ae,]>0
    }
    dest_group = x_to_xgroup[dest]
    dest_rows = xgroups_num[dest_group]
    source_group = x_to_xgroup[row]
    source_row = xgroups_num[source_group]

    edges = quick_df(from=source_row, to=dest_rows, rel="", color=ifelse(is.ae.dest, active.edge.color,passive.edge.color), width=ifelse(is.ae.dest,2,passive.edge.width), hidden=!is.ae.dest & hide.passive.edge, active=is.ae.dest*1)
    edges
  })
  edf = bind_rows(tr)

  edf = edf %>%
    group_by(from, to) %>%
    top_n(1,active) %>%
    slice(1) %>%
    select(-active)

  if (!show.own.loop)
    edf = filter(edf, from != to)

  if (just.eq.chain)
    edges = edges[is.ae.dest,,drop=FALSE]

  if (return.dfs) return(list(ndf=ndf, edf=edf, sdf=sdf, rne=rne))

  graph = create_graph(select(ndf,-xgroup), edf)

  render_graph(graph, output="visNetwork")
}


find.eq.chain.x = function(g, x0 = g$sdf$x[[1]], eq=g[["eq"]], t=1) {
  restore.point("find.eq.chain.x")


  if (has.col(eq,"t") & !is.null(t))
    eq = eq[eq$t==t,]

  used.x = NULL
  x = x0
  n = length(x)

  is.multi.stage = isTRUE(g$is.multi.stage)
  while(length(x)>length(used.x)) {
    cur.x = setdiff(x, used.x)[1]
    row = match(cur.x, g$sdf$x)
    trans.mat = g$sdf$trans.mat[[row]]

    ae = eq$ae[[row]]
    # No more transitions possible from current point
    if (NROW(trans.mat)==0) break

    xd = colnames(trans.mat)[trans.mat[ae,] > 0]
    used.x = c(used.x, cur.x)
    x = union(x,xd)
  }
  x
}

