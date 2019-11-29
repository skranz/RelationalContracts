# Simple Blackmailing Game from our Paper
blackmailing = function() {
  library(RelationalContracts)
  g = rel_game("Blackmailing Game") %>%
    rel_param(delta=0.9, rho=0.5) %>%
    # Initial State
    rel_state("x0", A1=list(a1=c("keep","reveal")),pi1=0, pi2=1) %>%
    rel_transition("x0","x1",a1="reveal", prob=1) %>%
    # Evidence Revealed
    rel_state("x1",pi1=0, pi2=0) %>%
    rel_compile()

  # Solve a T-RNE
  g = g %>% rel_T_rne(T=50, save.history = TRUE, tie.breaking = "slack")

  # In the paper we discussed how for T -> infinity
  # The T-RNE payoffs in state x0 should converge to
  # (0,1) i.e. the blackmailer can almost extract nothing

  # For T = 50 we are alreafy pretty close to this result
  get_eq(g)
  # We also see how with increasing T we converge towards
  # this payoff.
  animate_capped_rne_history(g,x=NULL)

  # However, there will be a catch because we only
  # solve the game numerical with imprecise floating
  # point arithmethics...

  # Solve for T=500
  g = g %>% rel_T_rne(T=500, save.history = TRUE, tie.breaking = "slack")

  # hist will contain for all values from T from 1 to 500
  # the corresponding RNE payoffs
  hist = get_T_rne_history(g) %>%
    filter(x=="x0") %>%
    mutate(T = 500-t+1)

  # Plot r1 as function of T
  library(ggplot2)
  ggplot(hist, aes(x=T, y=r1)) + geom_point()

  # We see that for T slightly above 200
  # again high levels of r1 can be implemented
  # That is because incentives constraints
  # are only checked approximately.
  #
  # This means at some point it will become
  # numerically again incentive compatible to
  # implement the punishment action.
  #
  # If we could exactly compute equilibria
  # payoffs would converge to (0,1) without those
  # jumps, however.
}
