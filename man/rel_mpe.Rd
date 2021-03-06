% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mpe.R
\name{rel_mpe}
\alias{rel_mpe}
\title{Tries to find a MPE by computing iteratively best replies}
\usage{
rel_mpe(
  g,
  delta = g$param$delta,
  static.eq = NULL,
  max.iter = 100,
  tol = 1e-08,
  a.init.guess = NULL
)
}
\arguments{
\item{g}{the game}

\item{delta}{the discount factor}

\item{max.iter}{maximum number of iterations}

\item{tol}{we finish if payoffs in a subsequent iteration don't change by more than tol}

\item{a.init.guess}{optionaly an initially guess of the action profiles. A vector of size nx (number of states) that describes for each state the integer index of the action profile. For a game g look at `g$ax.grid` to find the indeces of the desired action profiles.}
}
\description{
Returns a game object that contains the mpe.
Use the function get_mpe to retrieve a data frame that describes the MPE.
}
