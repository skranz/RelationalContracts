% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spe.R
\name{rel_spe}
\alias{rel_spe}
\title{Finds an optimal simple subgame perfect equilibrium of g. From this the whole SPE payoff set can be deduced.}
\usage{
rel_spe(
  g,
  delta = g$param$delta,
  tol.feasible = 1e-10,
  no.exist.action = c("warn", "stop", "nothing"),
  verbose = FALSE,
  r1 = NULL,
  r2 = NULL,
  rho = g$param$rho,
  add.action.labels = TRUE,
  max.iter = 10000,
  first.best = FALSE
)
}
\arguments{
\item{g}{the game object}

\item{delta}{The discount factor. By default the discount factor specified in \code{g}.}

\item{tol.feasible}{Due to numerical inaccuracies, sometimes incentive constraints which theoretically should exactly hold, seem to be violated. To avoid this problem, we will consider all action profiles feasible whose incentive constraint is not violated by more then \code{tol.feasible}. This means we compute epsilon equilibria in which \code{tol.feasible} is the epsilon.}

\item{no.exist.action}{What shall be done if no pure SPE exists? Default is \code{no.exist.action = "warning"}, alternatives are \code{no.exist.action = "error"} or \code{no.exist.action = "nothing"}.}

\item{verbose}{if \code{TRUE} give more detailed information over the solution process.}

\item{r1}{(or \code{r2}) if not NULL we want to find a SPE in a truncated game. Then r1 and r2 need to specify for each state the exogenously fixed negotiation payoffs.}

\item{rho}{Only relevant if r1 and r2 are not null. In that case the negotiation probability.}
}
\description{
Finds an optimal simple subgame perfect equilibrium of g. From this the whole SPE payoff set can be deduced.
}
