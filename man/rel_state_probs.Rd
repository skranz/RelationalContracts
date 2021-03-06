% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transitions.R
\name{rel_state_probs}
\alias{rel_state_probs}
\title{Compute the long run probability distribution over states if an
equilibrium is played for many periods.}
\usage{
rel_state_probs(
  g,
  x0 = c("equal", "first", "first.group")[1],
  start.prob = NULL,
  n.burn.in = 100,
  n.averaging = 100,
  tol = 1e-13,
  eq.field = "eq"
)
}
\arguments{
\item{g}{the game object for which an equilibrium has been solved}

\item{x0}{the initial state, by default the first state. If \code{initial} is not specified we asssume the game starts with probability 1 in the initial state. We have 3 reserved keywords: \code{x0="equal"} means all states are equally likely, \code{x0="first"} means the game starts in the first state, \code{x0="first.group"} means all states of the first \code{xgroup} are equally likely.}

\item{start.prob}{an optional vector of probabilities that specifies for each state the probability that the game starts in that state. Overwrites "x0" unless kept NULL.}

\item{n.burn.in}{Number of rounds before probabilities are averaged.}

\item{n.averaging}{Number of rounds for which probabilities are averaged.}

\item{tol}{Tolerance such that computation stops already in burn-in phase if transition probabilities change not by more than tol.}
}
\description{
Adds a column \code{state.prob} to the computed equilibrium data frame,
which you can retrieve by calling \code{get_eq}.
}
\details{
If the equilibrium strategy induces a unique stationary distribution over
the states, this distribution should typically be found (or at least approximated). Otherwise the result can depend on the parameters.

The initial distribution of states is determined by the parameters
\code{x0} or \code{start.prob}. We then multiply
the current probabilities susequently \code{n.burn.in} times with
the transitition matrix on the equilibrium path.
This yields the probability distribution over states assuming
the game is played for \code{n.burn.in} periods.

We then continue the process for \code{n.averaging} rounds, and return
the mean of the state probability vectors over these number of rounds.

If between two rounds in the burn-in phase the transitition probabilities
of no state pair change by more than the parameter \code{tol}, we
immediately stop and use the resulting probabilit vector.

Note that for T-RNE or capped RNE we always take the transition
probabilities of the first period, i.e. we don't increase the t in
the actual state definition.
}
