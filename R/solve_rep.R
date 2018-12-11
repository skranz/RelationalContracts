
# Solve the repeated game that would stay forever in state x
solve_x_repgame = function(g,x,  state = g$sdf[g$sdf$x == x,]) {
  restore.point("solve_x_repgame")

  g1 = matrix(state$pi1[[1]], state$na1,state$na2)
  g2 = matrix(state$pi2[[1]], state$na1,state$na2)
  lab.ai = make.state.lab.ai(state)

  rg = repgame::init.game(n=2, g1=g1,g2=g2,lab.ai = lab.ai)
  sol = repgame::solve.game(rg)

  beta1 = g$param$beta1
  beta2 = g$param$beta2

  opt.mat = sol$opt.mat

  U = opt.mat[,"Ue"]
  v1_rep = opt.mat[,"v1"]
  v2_rep = opt.mat[,"v2"]
  r1 = v1_rep + beta1 * (U-v1_rep-v2_rep)
  r2 = v2_rep + beta2 * (U-v1_rep-v2_rep)


  rg$lab.a
  res = data_frame(
    delta_min=opt.mat[,"delta"],
    delta_max=c(opt.mat[,"delta"][-1],1),
    r1 = r1,
    r2 = r2,
    U = U,
    v1_rep = v1_rep,
    v2_rep = v2_rep,
    strat.lab = rownames(opt.mat),
    ae = opt.mat[,"ae"],
    a1 = opt.mat[,"a1"],
    a2 = opt.mat[,"a2"],
  )
  res
}

make.state.lab.ai = function(state) {
  restore.point("make.state.lab.ai")
  A1 = state$A1[[1]]
  if (length(A1)==1) {
    a1.lab = as.character(A1[[1]])
  } else {
    A1.grid = expand.grid(A1)
    a1.lab = paste.matrix.cols(A1.grid, sep="_")
  }
  A2 = state$A2[[1]]
  if (length(A2)==1) {
    a2.lab = as.character(A2[[1]])
  } else {
    A2.grid = expand.grid(A2)
    a2.lab = paste.matrix.cols(A2.grid, sep="_")
  }
  list(a1.lab, a2.lab)
}

make.state.lab.a = function(state, sep=" ", empty.sep=TRUE) {
  restore.point("make.state.lab.ai")
  a.grid = state$a.grid[[1]]

  for (col in seq_len(NCOL(a.grid))) {
    if (is.character(a.grid[[col]])) {
      a.grid[[col]][a.grid[[col]]=="-"] = ""
    }
  }

  paste.df.cols(a.grid, sep=sep, empty.sep=TRUE)

}


