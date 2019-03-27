blackmailing = function() {
  reveal = seq(0,1,by=0.4)
  #reveal = c(0,0.51)
  g = rel_game("Blackmailing with Endogenous Brinkmanship") %>%
    rel_param(delta=0.7, rho=0.5) %>%
    # Initial State
    rel_state("x0", A1=list(reveal=reveal),A2=NULL) %>%
    rel_payoff("x0",pi1=0, pi2=1) %>%
    rel_transition("x0","x1",reveal=reveal, prob=reveal) %>%
    # Evidence Revealed
    rel_state("x1", A1=NULL,A2=NULL,x.T="xT") %>%
    rel_payoff("x1",pi1=0, pi2=0) %>%
    rel_compile()

  g = g %>% rel_T_rne(T=83, save.history = TRUE, use.cpp=TRUE, T.rne=TRUE, tie.breaking = "slack", save.details = TRUE)

  get.spe(g)
  get.eq(g)
  hist = g$eq.history %>% filter(x=="x0")
  hist = add.action.details(g,hist)

  det = get.rne.details(g)

  det83 = det
  det81 = det
  det82 = det

  res = bind_rows(det81,det82,det83) %>% filter(x=="x0",reveal==0.4)

  which(hist$a2.reveal>0)

  animate.capped.rne.history(g,x=NULL)


}

