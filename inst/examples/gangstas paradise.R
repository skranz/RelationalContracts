# Gangsta's paradise game from the paper

library(RelationalContracts)
g = rel_game() %>%
  # PD state
  rel_state("x0", A1=list(a1=c("C","D")), A2=list(a2=c("C","D")),
            pi1=case_when(
              a1 == "C" & a2== "C" ~ 2,
              a1 == "D" & a2== "C" ~ 3,
              a1 == "C" & a2== "D" ~ -10,
              a1 == "D" & a2== "D" ~ 0
            ),
            pi2=case_when(
              a1 == "C" & a2== "C" ~ 2,
              a1 == "D" & a2== "C" ~ -10,
              a1 == "C" & a2== "D" ~ 3,
              a1 == "D" & a2== "D" ~ 0
            )
  ) %>%
  rel_state("x.paradise", pi1=2, pi2=2) %>%
  rel_transition("x0","x.paradise",a1="C",a2="C") %>%
  rel_compile()

# Solve a T-RNE
g = g %>% rel_T_rne(T=99, delta=0.5, rho=1, save.history = TRUE, tie.breaking = "slack")

get_T_rne_history(g) %>%
  filter(x!="x.paradise", t <= 8) %>%
  arrange(t) %>%
  select(t,ae.lab,r1,r2,U,v1,v2)

# Check if we can have an RNE if we split the state

# We now split x0 into two states x0 and x1
# As long as paradise is not yet reached, we alternate
# each period between x0 and x1
g = rel_game() %>%
  # PD state
  rel_states(c("x0","x1"), A1=list(a1=c("C","D")), A2=list(a2=c("C","D")),
            pi1=case_when(
              a1 == "C" & a2== "C" ~ 2,
              a1 == "D" & a2== "C" ~ 3,
              a1 == "C" & a2== "D" ~ -10,
              a1 == "D" & a2== "D" ~ 0
            ),
            pi2=case_when(
              a1 == "C" & a2== "C" ~ 2,
              a1 == "D" & a2== "C" ~ -10,
              a1 == "C" & a2== "D" ~ 3,
              a1 == "D" & a2== "D" ~ 0
            )
  ) %>%
  rel_state("x.paradise", pi1=2, pi2=2) %>%
  rel_transition("x0","x1",a1="D") %>%
  rel_transition("x0","x1",a2="D") %>%
  rel_transition("x1","x0",a1="D") %>%
  rel_transition("x1","x0",a2="D") %>%
  rel_transition("x0","x.paradise",a1="C",a2="C") %>%
  rel_compile()

g = g %>% rel_T_rne(T=99, delta=0.5, rho=1, save.history = TRUE, tie.breaking = "slack")

get_T_rne_history(g) %>%
  filter((x=="x0" & t %% 2 == 1) | (x=="x1" & t %% 2 == 0), t <= 10) %>%
  arrange(t) %>%
  select(t,x,ae.lab,r1,r2,U,v1,v2) %>%
  mutate(across(r1:v2,~round(., digits=8)))

hist = get_T_rne_history(g) %>% filter(t<=100)
find.eq.action.cycles(g)
res = study_convergence(g, eq_li = hist)

# Solve the truncated games to check whether we also have an RNE
# with C|C in x0 and D|D in x1.

g = rel_spe(g, delta=0.5, rho=1, r1 = c(2,1,2), r2=c(2,1,2))
spe = get_eq(g)
spe %>%
  select(x,ae.lab,r1,r2,U,v1,v2)


# Cycles can also be longer

# CCD CCD
g = g %>% rel_T_rne(T=20, delta=0.9, rho=0.7, save.history = TRUE, tie.breaking = "slack")
get_T_rne_history(g) %>% filter(x=="x0", t <= 10)

# Here CDDD CDDD
g = g %>% rel_T_rne(T=20, delta=0.35, rho=1, save.history = TRUE, tie.breaking = "slack")
get_T_rne_history(g) %>% filter(x=="x0", t <= 10)


# For T = 50 we are already pretty close to this result
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
  ggplot(hist, aes(x=t, y=r1)) + geom_point()

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
