match.by.cols = function(x.df, v.df, cols) {
  restore.point("match.by.cols")
  if (length(cols)==1) {
    return(match(x.df[[cols]], v.df[[cols]]))
  } else {
    x = paste.df.cols(x.df,cols, sep="§")
    v = paste.df.cols(v.df, cols, sep="§")
    match(x,v)
  }



}

paste.df.cols = function (mat, cols = 1:NCOL(mat),sep="", empty.sep=FALSE, ...) {
  restore.point("paste.df.cols")
  if (NROW(cols) == 2) {
    sep1 = ifelse(!empty.sep | nchar(mat[[cols[1]]])>0 | nchar(mat[[cols[2]]])>0, sep,"")
    return(paste0(mat[[cols[1]]],sep1, mat[[cols[2]]], ...))
  } else if (NROW(cols) == 3) {
    sep1 = ifelse(!empty.sep | nchar(mat[[cols[1]]])>0 | nchar(mat[[cols[2]]])>0, sep,"")
    sep2 = ifelse(!empty.sep | nchar(mat[[cols[2]]])>0 | nchar(mat[[cols[3]]])>0, sep,"")
    return(paste0(mat[[cols[1]]],sep1, mat[[cols[2]]],sep2, mat[[cols[3]]],
          ...))
  } else {
      code = paste("mat[[", cols, "]]", collapse = ",sep,")
      code = paste("paste0(", code, ",sep=sep,...)", sep = "")
      return(eval(parse(text = code)))
  }
}

nlist = function (...) {
    li = list(...)
    li.names = names(li)
    names = unlist(as.list(match.call())[-1])
    if (!is.null(li.names)) {
        no.names = li.names == ""
        names(li)[no.names] = names[no.names]
    }
    else {
        names(li) = names
    }
    li
}
