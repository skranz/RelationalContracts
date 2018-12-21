attrition.example = function() {
  g = rel_game("War of attrition") %>%
    # Initial State
    rel_state("x0", A1=c("F","C"),A2=c("F","C"),pi1=0,pi2=0) %>%
    rel_transition("x0","x1",a1="F",a2="C") %>%
    rel_transition("x0","x2",a1="C",a2="F") %>%
    rel_transition("x0","x12",a1="C",a2="C") %>%
    rel_state("x1",pi1=1, pi2=0) %>%
    rel_state("x2",pi1=0, pi2=1) %>%
    rel_state("x12",pi1=0.5, pi2=0.5) %>%
    rel_compile()

  g = rel_capped_rne(g, T=100, delta=0.9, rho=0.1)
  rne.diagram(g,t=90)

  g = rel_game("War of attrition. Extra First State") %>%
    # First Period
    rel_state("xS", A1=c("F","C"),A2=c("F","C"),pi1=0,pi2=0) %>%
    rel_transition("xS","x1",a1="F",a2="C") %>%
    rel_transition("xS","x2",a1="C",a2="F") %>%
    rel_transition("xS","x12",a1="C",a2="C") %>%
    rel_transition("xS","x0",a1="F",a2="F") %>%
    # Initial State
    rel_state("x0", A1=c("F","C"),A2=c("F","C"),pi1=0,pi2=0) %>%
    rel_transition("x0","x1",a1="F",a2="C") %>%
    rel_transition("x0","x2",a1="C",a2="F") %>%
    rel_transition("x0","x12",a1="C",a2="C") %>%
    rel_state("x1",pi1=1, pi2=0) %>%
    rel_state("x2",pi1=0, pi2=1) %>%
    rel_state("x12",pi1=0.5, pi2=0.5) %>%
    rel_compile()

  g = rel_capped_rne(g, T=100, delta=0.9, rho=0.1)
  rne.diagram(g,t=1)

  g = rel_rne(g, T=100, delta=0.9, rho=0.1)
  rne.diagram(g,t=1)


  g=g %>% rel_capped_rne(T=10, save.details = TRUE)
}
