library(RelationalContracts)
library(RSGSolve2)
g = rel_game("Mutual Gift Game with Vulnerability") %>%
  rel_param(delta=0.9) %>%
  rel_state("x0",
    # Both players can pick effort
    # on a grid between 0 and 1
    A1 = list(e1=c(0,1)),
    A2 = list(e2=c(-1,0,1)),
    # Stage game payoffs
    pi1 = ~ 2*(e2 - 0.5*e1^2),
    pi2 = ~ 2*(e1 - 0.5*pmax(e2,0)^2)
  ) %>%
  rel_compile()

cbind(g$ax.grid, g$ax.pi)

   C      D     P
C  1,1  -1,2   -3,2
D  2,-1  0,0   -2,0

library(RSGSolve2)
# Transform game in format used by RSGSolve
rsg = make.rsg.game(g)
rsg
# Solve SPE payoff set
rsg.sol = solveSG(rsg=rsg,delta=0.3,verbose = 0L,half.space.intersect = FALSE,pencil_algo = !TRUE)

# Show SPE payoff set
plot.rsg.payoff.set(rsg.sol)

