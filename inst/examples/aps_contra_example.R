aps.contra = function() {
  options(warn=1)
  g = rel_game("Principal-Agent") %>%
    # Initial State
    rel_state("x",A2=list(e=c(0,1)),pi1=~e, pi2=~ -0.5*e) %>%
    rel_after_cap_payoffs(U=0.5,v1=0,v2=0) %>%
    rel_compile() %>%
    rel_capped_rne(T=20,adjusted.delta = 0.48, rho=0.2, save.history = TRUE)

  (rep.df = g$rep.games.df)
  (rne=g$rne)

  animate_capped_rne_history(g)

  # An example without convergence but oscilation...
  g = rel_game("Principal-Agent Variation") %>%
    # Initial State
    rel_state("x0",A2=list(e=c(0,1)),pi1=~e, pi2=~ -0.5*e) %>%
    rel_state("x1",pi1=0.3, pi2=0.3) %>%
    rel_transition("x0","x1", e=1) %>%
    rel_transition("x1","x0") %>%
    rel_after_cap_payoffs("x0", U=1,v1=0,v2=0) %>%
    rel_after_cap_payoffs("x1", U=1,v1=0,v2=0) %>%
    rel_compile() %>%
    rel_capped_rne(T=30,adjusted.delta = 0.48, rho=0.4, save.history = TRUE)

  (rne=g$rne)
  hist = g$rne.history
  d = hist %>% filter(x=="x0")
  plot(d$t,d$U, type="l")
  animate_capped_rne_history(g,x=NULL)

  #animate_capped_rne_history(g,x=NULL, add.plot = xlim(0,0.5)+ylim(0,0.5))


  # An example without convergence but oscilation
  g = rel_game("Principal-Agent Variation") %>%
    # Initial State
    rel_state("x0",A2=list(e=c(0,1)),pi1=~e, pi2=~ -0.5*e) %>%
    rel_state("x1",pi1=0.3, pi2=0.3) %>%
    rel_transition("x0","x1", e=1) %>%
    rel_transition("x1","x0") %>%
    rel_after_cap_payoffs("x0", U=0.58,v1=0.1,v2=0.1) %>%
    rel_after_cap_payoffs("x1", U=0.58,v1=0.1,v2=0.1) %>%
    rel_compile() %>%
    rel_capped_rne(T=10,adjusted.delta = 0.48, rho=0.4, save.history = TRUE)
  animate_capped_rne_history(g,x=NULL)


}

