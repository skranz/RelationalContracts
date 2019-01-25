# Find SPE of truncated games with fixed r

examples.spe.trunc = function() {
  g = rel_game("Principal-Agent Variation") %>%
    rel_state("x0",A2=list(e=c(0,1)),pi1=~ e, pi2=~ -0.5*e) %>%
    rel_state("x1",pi1=0.3, pi2=0.3) %>%
    rel_transition("x0","x1", e=1) %>%
    rel_transition("x1","x0") %>%
    rel_compile() %>%
    rel.prepare.for.spe()

  trunc_policy_iteration(g$ax.trans, g$ax.pi$Pi,g$sdf$na.vec, delta=0.8, rho=0.1, r=c(5,-2))

  g$ax.pi

  animate.capped.rne.history(g,x=NULL)

}


trunc_policy_iteration = function(T, Pi,na.vec, delta,  rho=0, r=0, oldp=NULL, V = NULL, tol = 1e-8, use.cpp=TRUE, ax.x=NULL) {
  restore.point("trunc_policy_iteration")

  nax = length(na.vec)
  nx = NROW(na.vec)
  Q = rep(-Inf,nax);


  if (!use.cpp & is.null(ax.x))
    ax.x = sizes.to.chunk.inds(na.vec)


  # Default policy is the one that maximizes immediate rewards
  if (is.null(oldp)) {
    oldp = which.chunk.maxs(Pi, na.vec, ax.x, use.cpp)  # na.vec describes the row indices
  }

  p = oldp
  iter = 0;
  done = 0;

  if (any(!is.finite(Pi[p]))) {
    stop("Even under optimal policy value is -Inf")
    return(NULL)
  }

  while (TRUE) {
    iter = iter + 1;

    # Determine Value
		Tp = T[p,,drop=FALSE]
    Pip = Pi[p]

    if (rho > 0) {
      # With negotiations
      # V = (1-delta)*Pi + delta*T%*%((1-rho)V+rho*r)
      # (I-delta*(1-rho)*T) %*% V = (1-delta)*Pi + delta*rho*T %*% r  =>
      V = solve(
        diag(nx)-delta*(1-rho)*Tp, # matrix
        (1-delta)*Pip + delta*rho* Tp %*% r # constant
      )
    } else {
      # No negotiations
      # V = (1-delta)*Pi + delta*T*V
      # (I-delta*T) %*% V = (1-delta)*Pi
      V = solve(diag(nx)-delta*Tp, (1-delta)*Pip)
    }


    # Get value for every (a,x) pair
    oldQ = Q

    if (rho >0) {
  		Q = (1-delta)*Pi + delta * T %*% ((1-rho)*V+rho*r);
    } else {
  		Q = (1-delta)*Pi + delta * T %*% V;
    }
    # Get optimal policy
    p = which.chunk.maxs(Q, na.vec, ax.x, use.cpp=use.cpp)

    #print(V)
    #names(V) = names(p)= m$x.lab;
    #rbind(V,label.ax(m,p),oldV,label.ax(m,oldp))

    if (identical(p, oldp) | approxeq(Q, oldQ, tol)) {
      # if we just compare p and oldp, it might oscillate due to ties
      # However, it may converge faster than Q
      break();
    }
    oldp = p;
    oldQ = Q;
  }
  #print(paste(iter-1, " policy iterations"))
  return(list(p=p,V=as.numeric(V)))
}

which.chunk.maxs = function(vec, sizes=NULL,chunk.inds=NULL, use.cpp=TRUE) {
  if (!use.cpp) return(r.which.chunk.maxs(vec, sizes,chunk.inds))
  c_which_chunk_maxs(vec, sizes)
}

r.which.chunk.maxs = function(vec, sizes, chunk.inds=sizes.to.chunk.inds(sizes)) {
  res = unlist(tapply(vec, chunk.inds, which.max))
  res = res+c(0,cumsum(sizes)[-length(sizes)])
  res
}


sizes.to.chunk.inds = function(sizes) {
  chunk.ind = unlist(lapply(seq_along(sizes), function(i) rep(i,sizes[i]) ))
}
