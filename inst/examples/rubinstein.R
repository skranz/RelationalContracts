# Alternating offer bargaining as in Rubinstein
#
# Two players decide how to split one unit of money
# by making alternative offers each period.
#
# Once an offer is accepted the game ends.
#
# Note: Rubinstein assumes that any real number between 0 and 1
# can be offered. Our model only allows a discrete actions space.
#
# If the offers grid is sufficiently fine and the discount factor not
# too large, we get a similar result
# as Rubinstein.
#
# Yet, if the offers grid is coarse and the discount factor large
# we rather get a folk theorem result that all possible splits
# can occur in equilibrium.
#
# I have not yet checked whether that is a true theoretical result
# or due to numerical approximation errors. (But I guess the former.)

rubinstein = function() {
  library(RelationalContracts)

  # Grid of possible offers
  offers = seq(0,1, by=0.02)

  # Data frames describing states in which player 1 or player 2
  # has to accept an offer.
  x.accept1.df = tibble(x=paste0("accept1_",offers), offered = offers)
  x.accept2.df = tibble(x=paste0("accept2_",offers), offered = offers)


  g = rel_game("Rubinstein Bargaining") %>%
    # Offer states
    rel_state("offer1",
      A1 = list(offer=offers),pi1=0, pi2=0
    ) %>%
    rel_state("offer2",
      A2 = list(offer=offers),pi1=0, pi2=0
    ) %>%
    # Acceptance states
    rel_states(x.accept1.df,
      A1 = list(accept = c("accept","reject")),
      pi1 = ~ offered*(accept=="accept"),
      pi2 = ~ (1-offered)*(accept=="accept")
    ) %>%
    rel_states(x.accept2.df,
      A2 = list(accept = c("accept","reject")),
      pi1 = ~ (1-offered)*(accept=="accept"),
      pi2 = ~ offered*(accept=="accept")
    ) %>%
    # Terminal state
    rel_state("x_end", pi1=0, pi2=0) %>%
    # State transitions
    rel_transition("offer1",x.accept2.df$x,offer=offers,prob = 1) %>%
    rel_transition("offer2",x.accept1.df$x,offer=offers,prob = 1) %>%
    rel_transition(x.accept2.df$x,"offer2",accept="reject",prob = 1) %>%
    rel_transition(x.accept1.df$x,"offer1",accept="reject",prob = 1) %>%
    rel_transition(x.accept2.df$x,"x_end",accept="accept",prob = 1) %>%
    rel_transition(x.accept1.df$x,"x_end",accept="accept",prob = 1) %>%
    rel_compile()

  g = rel_spe(g,delta=0.9)%>%
    rel_eq_as_discounted_sums()

  get_eq(g) %>%
    filter(x == "offer1" | x == "offer2") %>%
    select(x, ae.lab, U,v1,v2, a1.lab,a2.lab)

}

