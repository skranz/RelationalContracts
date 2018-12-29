examples.rel.mermaid.code = function() {


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

  rel.diagram(g)

  cat(rel.mermaid.code(g))


  x.max = 3
  x.df = as_data_frame(expand.grid(x1=0:x.max,x2=0:x.max))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)

  g = rel_game("Arms Race") %>%
    rel_param(delta=0.99, rho=0.9, c=0, k=0.1,x.max=x.max, success.prob=0.5) %>%
    rel_states_fun(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun) %>%
    rel_compile() %>%
    rel_capped_rne(T=50)

  rne.diagram(g,t=1)

  rel.diagram(g)

  cat(rel.mermaid.code(g))


}

spe.diagram = function(g,t=1, show.own.loop=FALSE, show.terminal.loop=FALSE, eq.field = "spe", use.x=NULL, just.eq.chain=FALSE, x0=g$sdf$x[1]) {
  rne.diagram(g,t,show.own.loop, show.terminal.loop,eq.field, use.x, just.eq.chain, x0)
}

rne.diagram = function(g,t=1, show.own.loop=FALSE, show.terminal.loop=FALSE, eq.field = "rne", use.x=NULL, just.eq.chain=FALSE, x0=g$sdf$x[1]) {
  restore.point("rne.diagram")

  library(DiagrammeR)


  rne = g[[eq.field]]

  if (has.col(rne,"t"))
    rne = rne[rne$t==t,]

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
  lab = paste0(sdf$x, " \n", round(rne$r1,2), " ", round(rne$r2,2))
  tooltip = paste0(
    rne$x,
    "<br>ae: ",rne$ae.lab,
    "<br>a1: ", rne$a1.lab, "<br>a2: ", rne$a2.lab,
    "<br>v1=",round(rne$v1,2)," v2=",round(rne$v2,2),
    "<br>U=", round(rne$U,2))

  ndf = data.frame(id=1:n, label=lab, type="node", shape="box", title=tooltip)

  # Create edges
  tr = lapply(seq_len(NROW(sdf)), function(row) {
    trans.mat = sdf$trans.mat[[row]]
    x = sdf$x[[row]]

    rne.row = which(rne$x==x)[1]
    ae = rne$ae[[rne.row]]
    if (is.null(trans.mat)) {
      if (!show.terminal.loop)
        return(NULL)
      dest = x
      is.ae.dest = TRUE
    } else {
      dest = colnames(trans.mat)
      is.ae.dest = trans.mat[ae,]>0
    }
    dest.rows = match(dest, sdf$x)

    edges = quick_df(from=row, to=dest.rows, rel="", color=ifelse(is.ae.dest, "#000077","#dddddd"), width=ifelse(is.ae.dest,2,1))

    if (just.eq.chain)
      edges = edges[is.ae.dest,,drop=FALSE]
    edges
  })
  edf = bind_rows(tr)
  if (!show.own.loop)
    edf = filter(edf, from != to)

  graph = create_graph(ndf, edf)

  #render_graph(graph,)
  render_graph(graph, output="visNetwork")
}


find.eq.chain.x = function(g, x0 = g$sdf$x[[1]], eq=first.non.null(g$spe, g$rne), t=1) {
  restore.point("find.eq.chain.x")


  if (has.col(eq,"t") & !is.null(t))
    eq = eq[eq$t==t,]


  tdf = g$tdf
  used.x = NULL
  x = x0
  n = length(x)

  while(length(x)>length(used.x)) {
    cur.x = setdiff(x, used.x)[1]
    row = match(cur.x, g$sdf$x)
    trans.mat = g$sdf$trans.mat[[row]]
    ae = eq$ae[[row]]
    xd = colnames(trans.mat)[trans.mat[ae,] > 0]
    used.x = c(used.x, cur.x)
    x = union(x,xd)
  }
  x
}


rel.diagram = function(g) {
  restore.point("rel.diagram")

  library(DiagrammeR)

  n = NROW(g$sdf)
  sdf=g$sdf
  ndf = data.frame(id=1:n, label=g$sdf$x, type="node", shape="box")

  # Create edges
  tr = lapply(seq_len(NROW(sdf)), function(row) {
    trans.mat = sdf$trans.mat[[row]]
    x = sdf$x[[row]]
    if (is.null(trans.mat)) {
      dest = x
    } else {
      dest = colnames(trans.mat)
    }
    dest.rows = match(dest, sdf$x)
    quick_df(from=row, to=dest.rows)
  })
  edf = bind_rows(tr)

  graph = create_graph(ndf, edf)

  render_graph(graph, output="visNetwork")
}


rel.mermaid.code = function(g, orient = c("LR","TD")[1]) {
  mm = paste0("graph ", orient)
  sdf = g$sdf
  x =g$sdf$x
  mm = c(mm, paste0(x,"((",x,"))", collapse=";"))

  prob.str = function(prob) {
    uprob = unique(prob)
    if (length(uprob)==1)
      return(paste0(round(uprob*100,1),"%"))
    return(paste0(round(range(prob*100),1),"%", collapse="-"))
  }

  tr = sapply(seq_len(NROW(sdf)), function(row) {
    trans.mat = sdf$trans.mat[[row]]
    x = sdf$x[[row]]
    if (is.null(trans.mat)) {
      dest = x
    } else {
      dest = colnames(trans.mat)
    }
    paste0(x,"-->", dest, collapse="\n")
  })
  mm = c(mm,tr)
  mm = paste0(mm, collapse="\n")
}
