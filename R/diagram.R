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


  x.max = 4
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


rne.diagram = function(g,t=1, show.own.loop=FALSE, show.terminal.loop=FALSE) {
  restore.point("rne.diagram")

  library(DiagrammeR)

  rne = g$rne
  if (has.col(rne,"t"))
    rne = rne[rne$t==t,]


  n = NROW(g$sdf)
  sdf=g$sdf
  lab = paste0(sdf$x, " \n", round(rne$r1,2), " ", round(rne$r2,2))

  ndf = data.frame(id=1:n, label=lab, type="node", shape="box")

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
    edges
  })
  edf = bind_rows(tr)
  if (!show.own.loop)
    edf = filter(edf, from != to)

  graph = create_graph(ndf, edf)

  #render_graph(graph,)
  render_graph(graph, output="visNetwork")
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
