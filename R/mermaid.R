examples.rel.mermaid.code = function() {

  g = rel_game("Weakly Monotone Vulnerability Paradox") %>%
    rel_param(delta=0.5, rho=0.1, c=0.5, xL=xL,xH=xH) %>%
    # Initial State
    rel_state("xL", A1=list(move=c("stay","vul")),A2=list(e=e.seq)) %>%
    rel_payoff("xL",pi1=~e, pi2=~ -c*e*e*(e>=0)) %>%
    rel_transition("xL","xH",move="vul") %>%
    # High vulnerability
    rel_state("xH", A1=NULL,A2=list(e=unique(c(-xH,e.seq)))) %>%
    rel_payoff("xH",pi1=~e, pi2=~ -c*e*e*(e>=0)) %>%
    rel_compile()

  cat(rel.mermaid.code(g))


  x.max = 3
  x.df = as_data_frame(expand.grid(x1=0:x.max,x2=0:x.max))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)

  g = rel_game("Arms Race") %>%
    rel_param(delta=0.995, rho=0.4, c=0, k=2,x.max=x.max, success.prob=0.5) %>%
    rel_states_fun(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun) %>%
    rel_compile()

  cat(rel.mermaid.code(g))


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
