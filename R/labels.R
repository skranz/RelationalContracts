

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

old.add.rne.action.labels = function(g, rne) {
  restore.point("add.rne.action.labels")

  # Add some additional info
  xrow = match(rne$x, g$sdf$x)


  if (!isTRUE(g$is.multi.stage)) {
    # No Multistage
    rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","ae"), cols2=c("x","a"))
    rne$ae.lab = g$a.labs.df$lab[rows]
    rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","a1"), cols2=c("x","a"))
    rne$a1.lab = g$a.labs.df$lab[rows]
    rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","a2"), cols2=c("x","a"))
    rne$a2.lab = g$a.labs.df$lab[rows]
  } else {
    # Multistage
    rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","ae"), cols2=c("x","a"))
    d.lab = g$a.labs.df$lab[rows]
    rows = match.by.cols(rne,g$gs$a.labs.df, cols1=c("x","s.ae"), cols2=c("x","a"))
    s.lab = g$gs$a.labs.df$lab[rows]
    rne$ae.lab = paste0(s.lab," | ", d.lab)

    rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","a1"), cols2=c("x","a"))
    d.lab = g$a.labs.df$lab[rows]
    rows = match.by.cols(rne,g$gs$a.labs.df, cols1=c("x","s.a1"), cols2=c("x","a"))
    s.lab = g$gs$a.labs.df$lab[rows]
    rne$a1.lab = paste0(s.lab," | ", d.lab)

    rows = match.by.cols(rne,g$a.labs.df, cols1=c("x","a2"), cols2=c("x","a"))
    d.lab = g$a.labs.df$lab[rows]
    rows = match.by.cols(rne,g$gs$a.labs.df, cols1=c("x","s.a2"), cols2=c("x","a"))
    s.lab = g$gs$a.labs.df$lab[rows]
    rne$a2.lab = paste0(s.lab," | ", d.lab)
  }
  rne
}


make.state.lab.ai = function(state) {
  restore.point("make.state.lab.ai")
  A1 = state$A1[[1]]
  if (is.data.frame(A1)) {
    a1.lab = paste.df.cols(A1, sep="_")
  } else if (length(A1)==1) {
    a1.lab = as.character(A1[[1]])
  } else {
    A1.grid = expand.grid(A1)
    a1.lab = paste.matrix.cols(A1.grid, sep="_")
  }
  A2 = state$A2[[1]]
  if (is.data.frame(A2)) {
    a2.lab = paste.df.cols(A2, sep="_")
  } else if (length(A2)==1) {
    a2.lab = as.character(A2[[1]])
  } else {
    A2.grid = expand.grid(A2)
    a2.lab = paste.matrix.cols(A2.grid, sep="_")
  }
  list(a1.lab, a2.lab)
}

make.state.lab.a = function(state, sep=" ", empty.sep=TRUE) {
  restore.point("make.state.lab.a")
  a.grid = state$a.grid[[1]]

  for (col in seq_len(NCOL(a.grid))) {
    if (is.character(a.grid[[col]])) {
      a.grid[[col]][a.grid[[col]]=="-"] = ""
    }
  }

  paste.df.cols(a.grid, sep=sep, empty.sep=empty.sep)

}
