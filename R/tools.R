match.by.cols = function(x.df, v.df, cols) {
  restore.point("match.by.cols")
  if (length(cols)==1) {
    return(match(x.df[[cols]], v.df[[cols]]))
  } else {
    x = paste.matrix.cols(x.df,cols, sep="§")
    v = paste.matrix.cols(v.df, cols, sep="§")
    match(x,v)
  }



}
