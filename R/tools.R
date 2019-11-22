expand_grid = function(...,stringsAsFactors=FALSE) {
  expand.grid(...,stringsAsFactors = stringsAsFactors) %>%
    as_tibble()
}

# Compute best reply payoffs from a matrix
find.best.reply.payoffs = function(i,pi,na1,na2) {
  if (i==1) {
    # Player 1's actions are columns
    pi.mat = matrix(pi,na2,na1)
    c.short = rowMaxs(pi.mat)
    c = rep(c.short, times=na1)
  } else {
    # Player 2's actions are rows
    pi.mat = matrix(pi,na2,na1)
    c.short = colMaxs(pi.mat)
    c = rep(c.short, each=na2)
  }
  c
}

deparse1 = function (call, collapse = "")
{
    paste0(deparse(call, width = 500), collapse = collapse)
}
# Helper function for trans.mat multiplication
trans.mat.mult = function(trans.mat, vec) {
  if (NROW(trans.mat)==0) {
    return(vec)
  }
  as.vector(trans.mat %*% vec)
}


factor.cols.as.strings = function(df) {
  #mutate_if(df, is.factor, as.character)
  factor.cols = sapply(df, is.factor)
  if (sum(factor.cols)>0)
    df[factor.cols] = lapply(df[factor.cols], as.character)
  df
}

# Expand grid with 2 arguments. One argument can be itself a data.frame
expand.grid2 = function(A,B) {
  if (!is.data.frame(A) & !is.data.frame(B)) {
    expand.grid(c(A,B), stringsAsFactors = FALSE)
  } else {
    if (!is.data.frame(A)) A = expand.grid(A, stringsAsFactors = FALSE)
    if (!is.data.frame(B)) B = expand.grid(B, stringsAsFactors = FALSE)
    inds = expand.grid(a=seq_len(NROW(A)),b=seq_len(NROW(B)), stringsAsFactors = FALSE)
    bind_cols(A[inds$a,,drop=FALSE], B[inds$b,,drop=FALSE])
  }
}


quick_df = function(...) {
  as_data_frame(list(...))
}

non.null = function(a,b) {
  if(is.null(a)) return(b)
  a
}

first.non.null = function(...) {
  args = list(...)
  for (val in args) {
    if (!is.null(val)) return(val)
  }
  return(NULL)

}

has.cols = function(x, cols) {
  all(cols %in% colnames(x))
}


has.col = function(x, col) {
  col %in% colnames(x)
}

match.by.cols = function(x.df, v.df, cols=NULL, cols1=cols, cols2=cols) {
  restore.point("match.by.cols")
  if (length(cols)==1) {
    return(match(x.df[[cols1]], v.df[[cols2]]))
  } else {
    x = paste.df.cols(x.df,cols1, sep="§")
    v = paste.df.cols(v.df, cols2, sep="§")
    match(x,v)
  }



}

paste.df.cols = function (mat, cols = 1:NCOL(mat),sep="", empty.sep=FALSE, ...) {
  restore.point("paste.df.cols")
  if (NROW(cols) == 2) {
    if (empty.sep) {
      sep1 = ifelse(!empty.sep | nchar(mat[[cols[1]]])>0 | nchar(mat[[cols[2]]])>0, sep,"")
      return(paste0(mat[[cols[1]]],sep1, mat[[cols[2]]], ...))
    } else {
      return(paste0(mat[[cols[1]]],mat[[cols[2]]],sep=sep, ...))
    }
  } else if (NROW(cols) == 3) {
    if (empty.sep) {
      sep1 = ifelse(!empty.sep | nchar(mat[[cols[1]]])>0 | nchar(mat[[cols[2]]])>0, sep,"")
      sep2 = ifelse(!empty.sep | nchar(mat[[cols[2]]])>0 | nchar(mat[[cols[3]]])>0, sep,"")
      return(paste0(mat[[cols[1]]],sep1, mat[[cols[2]]],sep2, mat[[cols[3]]],
          ...))
    } else {
      return(paste0(mat[[cols[1]]],mat[[cols[2]]],mat[[cols[3]]],sep=sep, ...))
    }
  } else {
      if (is.character(cols))
        cols = match(cols, colnames(mat))
      code = paste("mat[[", cols, "]]", collapse = ",sep,")
      code = paste("paste0(", code, ",sep=sep,...)", sep = "")
      return(eval(parse(text = code)))
  }
}

advanced.paste.matrix.cols = function (mat, cols = 1:NCOL(mat),sep="", empty.sep=FALSE, ...) {
  restore.point("paste.df.cols")
  if (NROW(cols) == 2) {
    if (empty.sep) {
      sep1 = ifelse(!empty.sep | nchar(mat[,cols[1]])>0 | nchar(mat[,cols[2]])>0, sep,"")
      return(paste0(mat[,cols[1]],sep1, mat[,cols[2]], ...))
    } else {
      return(paste0(mat[,cols[1]],mat[,cols[2]],sep=sep, ...))
    }
  } else if (NROW(cols) == 3) {
    if (empty.sep) {
      sep1 = ifelse(!empty.sep | nchar(mat[,cols[1]])>0 | nchar(mat[,cols[2]])>0, sep,"")
      sep2 = ifelse(!empty.sep | nchar(mat[,cols[2]])>0 | nchar(,mat[cols[3]])>0, sep,"")
      return(paste0(mat[,cols[1]],sep1, mat[,cols[2]],sep2, mat[,cols[3]],
          ...))
    } else {
      return(paste0(mat[,cols[1]],mat[,cols[2]],mat[,cols[3]],sep=sep, ...))
    }
  } else {
      if (is.character(cols))
        cols = match(cols, colnames(mat))
      code = paste("mat[,", cols, "]", collapse = ",sep,")
      code = paste("paste0(", code, ",sep=sep,...)", sep = "")
      return(eval(parse(text = code)))
  }
}


paste.matrix.cols = function (mat, cols = 1:NCOL(mat), ...) {
    if (NROW(cols) == 2) {
        return(paste(mat[, cols[1]], mat[, cols[2]], ...))
    }
    else if (NROW(cols) == 3) {
        return(paste(mat[, cols[1]], mat[, cols[2]], mat[, cols[3]],
            ...))
    }
    else {
        code = paste("mat[,", cols, "]", collapse = ",")
        code = paste("paste(", code, ",...)", sep = "")
        return(eval(parse(text = code)))
    }
}

approxeq = function (a, b, tol = 0.001) {
    isTRUE(all.equal(a, b, tol = tol, check.attributes = FALSE))
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
