% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagram.R
\name{eq_diagram}
\alias{eq_diagram}
\title{Draws a diagram of equilibrium state transition}
\usage{
eq_diagram(
  g,
  show.own.loop = FALSE,
  show.terminal.loop = FALSE,
  use.x = NULL,
  just.eq.chain = FALSE,
  x0 = g$sdf$x[1],
  hide.passive.edge = TRUE,
  label.fun = NULL,
  tooltip.fun = NULL,
  active.edge.color = "#000077",
  passive.edge.color = "#dddddd",
  add.passive.edge = TRUE,
  passive.edge.width = 1,
  return.dfs = FALSE,
  eq = g[["eq"]],
  font.size = 24,
  font = paste0(font.size, "px Arial black")
)
}
\arguments{
\item{g}{The solved game object}

\item{show.own.loop}{Shall a loop from a state to itself be drawn if there is a positive probability to stay in the state? (Default=FALSE)}

\item{show.terminal.loop}{Only relevant if \code{show.own.loop = TRUE}. If still \code{show.terminal.loop=FALSE} omit loops in terminal state that don't transist to any other state.}

\item{use.x}{optionally a vector of state ids that shall only be shown.}

\item{just.eq.chain}{If TRUE only show states that can be reached with positive probability on the equilibrium path when starting from state x0.}

\item{x0}{only relevant if \code{just.eq.chain=TRUE}. The ID of the x0 state. By default the first defined state.}

\item{label.fun}{An optional function that takes the equilibrium object and game and returns a character vector that contains a label for each state.}

\item{tooltip.fun}{Similar to \code{label.fun} but for the tooltip shown on a state.}

\item{return.dfs}{if TRUE don't show diagram but only return the relevant edge and node data frames that can be used to call \code{DiagrammeR::create_graph}. Useful if you want to manually customize graphs further.}

\item{font.size}{The font size}
}
\description{
Draws an arrow from state x to state y if and
only if on the equilibrium path there is a positive
probability to directly transist from x to y.
}
