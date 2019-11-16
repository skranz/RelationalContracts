

add.rne.action.labels = function(g, rne) {
  restore.point("add.rne.action.labels")

  # Add some additional info
  if (NROW(rne)==NROW(g$sdf)) {
    xrow = 1:NROW(rne)
  } else {
    xrow = match(rne$x, g$sdf$x)
  }
  lag.cumsum.na = g$sdf$lag.cumsum.na

  if (!isTRUE(g$is.multi.stage)) {
    # No Multistage
    rows = lag.cumsum.na[xrow]+rne[["ae"]]
    rne$ae.lab = g$a.labs.df$lab[rows]
    rows = lag.cumsum.na[xrow]+rne[["a1"]]
    rne$a1.lab = g$a.labs.df$lab[rows]
    rows = lag.cumsum.na[xrow]+rne[["a2"]]
    rne$a2.lab = g$a.labs.df$lab[rows]
  } else {
    s.lag.cumsum.na = g$gs$sdf$lag.cumsum.na
    # Multistage
    rows = lag.cumsum.na[xrow]+rne[["ae"]]
    d.lab = g$a.labs.df$lab[rows]

    rows = s.lag.cumsum.na[xrow]+rne[["s.ae"]]
    s.lab = g$gs$a.labs.df$lab[rows]
    rne$ae.lab = paste0(s.lab," | ", d.lab)

    rows = lag.cumsum.na[xrow]+rne[["a1"]]
    d.lab = g$a.labs.df$lab[rows]
    rows = s.lag.cumsum.na[xrow]+rne[["s.a1"]]
    s.lab = g$gs$a.labs.df$lab[rows]
    rne$a1.lab = paste0(s.lab," | ", d.lab)

    rows = lag.cumsum.na[xrow]+rne[["a2"]]
    d.lab = g$a.labs.df$lab[rows]
    rows = s.lag.cumsum.na[xrow]+rne[["s.a2"]]
    s.lab = g$gs$a.labs.df$lab[rows]
    rne$a2.lab = paste0(s.lab," | ", d.lab)
  }
  rne
}

make.state.lab.ai = function(state, sep="_") {
  restore.point("make.state.lab.ai")
  A1 = state$A1[[1]]
  if (is.data.frame(A1)) {
    a1.lab = paste.df.cols(A1, sep=sep)
  } else if (length(A1)==1) {
    a1.lab = as.character(A1[[1]])
  } else {
    A1.grid = expand.grid(A1)
    a1.lab = paste.matrix.cols(A1.grid, sep=sep)
  }
  A2 = state$A2[[1]]
  if (is.data.frame(A2)) {
    a2.lab = paste.df.cols(A2, sep=sep)
  } else if (length(A2)==1) {
    a2.lab = as.character(A2[[1]])
  } else {
    A2.grid = expand.grid(A2)
    a2.lab = paste.matrix.cols(A2.grid, sep=sep)
  }
  list(a1.lab, a2.lab)
}

make.state.lab.a = function(state, action.sep = " ", player.sep=" | ", empty.sep=TRUE) {
  restore.point("make.state.lab.a")
  a.grid = state$a.grid[[1]]

  for (col in seq_len(NCOL(a.grid))) {
    if (is.character(a.grid[[col]])) {
      a.grid[[col]][a.grid[[col]]=="-"] = ""
    }
  }
  cols1 = names(state$A1[[1]])
  cols2 = names(state$A2[[1]])

  if (length(cols1)>0) {
    p1 = trimws(paste.df.cols(a.grid[,cols1,drop=FALSE], sep=action.sep, empty.sep=empty.sep))
  } else {
    p1 = rep("",NROW(a.grid))
  }
  if (length(cols2)>0) {
    p2 = trimws(paste.df.cols(a.grid[,cols2,drop=FALSE], sep=action.sep, empty.sep=empty.sep))
  } else {
    p2 = rep("",NROW(a.grid))
  }
  cbind(
    lab = paste0(p1, player.sep, p2),
    lab1 = p1,
    lab2 = p2
  )
}


old.make.state.lab.a = function(state, sep=" ", empty.sep=TRUE) {
  restore.point("make.state.lab.a")
  a.grid = state$a.grid[[1]]

  for (col in seq_len(NCOL(a.grid))) {
    if (is.character(a.grid[[col]])) {
      a.grid[[col]][a.grid[[col]]=="-"] = ""
    }
  }

  paste.df.cols(a.grid, sep=sep, empty.sep=empty.sep)
}
