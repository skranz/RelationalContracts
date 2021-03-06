% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relhold.R
\name{rel_change_param}
\alias{rel_change_param}
\title{Add parameters to a relational contracting game}
\usage{
rel_change_param(g, ...)
}
\arguments{
\item{g}{a relational contracting game created with rel_game}

\item{...}{other parameters that can e.g. be used in payoff functions}

\item{delta}{The discount factor}

\item{rho}{The negotiation probability}
}
\value{
Returns the updated game
}
\description{
Add parameters to a relational contracting game
}
