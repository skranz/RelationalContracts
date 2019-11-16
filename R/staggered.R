#' Aggregate equilibrium behavior in games with random active player
#'
#' Often it is useful to specify games such that players don't
#' move simultaneously but a random player ap is chosen to be active
#' in a given state.
#'
#' The active player in a state x is defined by the variable ap
#' in x.df and the original state by xgroup.
#'
#' This function aggregates equilibrium outcomes from x to xgroup.
#' For payoffs r1,r2,v1,v2 and U we take the mean over the payoffs
#' given the two possible actvive players.
#'
#' The columns move.adv1 and move.adv2 describe the difference in
#' negotiation payoffs of a player when is the active player who can
#' make a move compared to the other player being active.
#'
#' Finally we create action labels by combining the actions
#' chosen when a player is active.
#'
#' @param g the game object
#' @param eq the equilibrium, by default the last solved eq of g.
#' @param ap.col the column as a character in x.df that is the index of the active player. By default "ap".
eq_combine_xgroup = function(g, eq=g[["eq"]], ap.col=ifelse(has.col(eq,"ap"), "ap",NA)) {

  xgroups = unique(eq$xgroup)


  if (!has.col(eq,"stationary.prob"))
    eq$stationary.prob=NA
  if (is.na(ap.col)) {
    res = eq %>%
      group_by(xgroup) %>%
      summarize(
        r1 = mean(r1),
        r2 = mean(r2),
        v1 = mean(v1),
        v2 = mean(v2),
        U = mean(U),
        stationary.prob = sum(stationary.prob)
      ) %>%
      right_join(tibble(xgroup=xgroups), by="xgroup")
    return(res)
  }


  labs.df = g$a.labs.df
  eq = eq %>%
    left_join(select(labs.df, x=x,a=a, ae.lab1=lab1, ae.lab2=lab2), by=c(x="x", ae="a")) %>%
    left_join(select(labs.df, x=x,a=a, a1.lab1=lab1, a1.lab2=lab2), by=c(x="x", a1="a")) %>%
    left_join(select(labs.df, x=x,a=a, a2.lab1=lab1, a2.lab2=lab2), by=c(x="x", a2="a"))

  eq %>%
    group_by(xgroup) %>%
    arrange_at(ap.col) %>%
    summarize(
      move.adv1 = r1[1]-r1[2],
      move.adv2 = r2[2]-r2[1],
      r1 = mean(r1),
      r2 = mean(r2),
      ae.lab = paste0(ae.lab1[1], g$options$lab.player.sep, ae.lab2[2]),
      a1.lab = paste0(a1.lab1[1], g$options$lab.player.sep, a1.lab2[2]),
      a2.lab = paste0(a2.lab1[1], g$options$lab.player.sep, a2.lab2[2]),
      v1 = mean(v1),
      v2 = mean(v2),
      U = mean(U),
      stationary.prob = sum(stationary.prob)
    )  %>%
    right_join(tibble(xgroup=xgroups), by="xgroup")
}


