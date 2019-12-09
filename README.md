# Relational Contracts

Author: Sebastian Kranz, Ulm University

This package allows to characterize the equilibrium payoff set in repeated games (and more general stochastic games) that describe long term relationships between two parties.

It will be introduced in two blog posts.

### Installation

The package can be best installed using my Github hosted R repositorium by calling
```r
install.packages("RelationalContracts",repos = c("https://skranz-repo.github.io/drat/",getOption("repos")))
```

### Some Features

  - Solve the set of pure strategy subgame perfect equilibrium payoffs in two player repeated or stochastic games with transfers.
  - Describe simple equilibria that can implement such payoffs.
  - Account for hold-up concerns by solving for repeated negotion equilibria as defined [here](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2210603).
  - Can be combinded with the [RSGSolve](https://github.com/skranz/RSGSolve) package to solve for pure SPE payoff sets without assuming transfers using the pencil sharpening algorithm by Abreu, Brooks and Sannikov [see here](https://github.com/babrooks/SGSolve/)
  - Different tools for analysis like graphic representation of equilibrium state transitions as acyclic graphs. 

### Documentation

For detailed usage, see the [vignettes](https://skranz.github.io/RelationalContracts/articles/), and the examples in [inst/examples](https://github.com/skranz/RelationalContracts/tree/master/inst/examples)
