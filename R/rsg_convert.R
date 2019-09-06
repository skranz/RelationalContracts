make.rsg.game = function(g, delta=g$param$delta) {
  restore.point("make.rsg.game")
  nx = NROW(g$sdf)
  xrow = 1
  states = lapply(1:nx, function(xrow) {
    sdf = g$sdf[xrow,]

    # tax = as.vector(t(matrix(which(m$ax[,"x"]==x), numActions[1], numActions[2])))

    ax = which(g$ax.grid$x == sdf$x)
    # Transpose action profile order to account for
    # differing order of player 1 and 2 in RSGSolve
    mat.ax = t(matrix(ax, sdf$na1, sdf$na2))
    ax = as.vector(mat.ax)

    list(
      numActions = c(sdf$na1,sdf$na2),
      payoffs = cbind(g$ax.pi$pi1[ax],g$ax.pi$pi2[ax]),
      transition = g$ax.trans[ax,,drop=FALSE]
    )
  })
  rsg = list(
    delta = delta,
    numPlayers = 2,
    numStates = nx,
    states = states
  )
  return(rsg)
}

plot.rsg.payoff.set = function(rsg.sol, xrow = 1, all.iter=FALSE,  main=paste0("SPE payoffs without transfers"), type="l", col = "black", lwd=1, lty=1, add=FALSE, fill="#dddddd") {
  restore.point("plot.rsg.payoff.set")
  state = xrow

  #old.par = par()
  par(mar=c(4,4,1,1))

  if (all.iter){
    ipoints = rsg.sol$ipoints[[state]]
    xlim = range(ipoints[,1])
    ylim = range(ipoints[,2])
  } else {
    xlim = range(rsg.sol$points[[state]][,1])
    ylim = range(rsg.sol$points[[state]][,2])
  }
  if (!add)
    plot(xlim,ylim, col="white", type="s", xlim=xlim,ylim=ylim,xlab="u1",ylab="u2")
  if (all.iter) {
    points(ipoints,col="grey",lty=2)
  }

  if (!is.null(rsg.sol)) {
    if (!is.null(fill)) {
      polygon(
        x=c(rsg.sol$points[[state]][,1]),
        y=c(rsg.sol$points[[state]][,2]),
        border=NA,
        #border=if (black.border) "black" else col,
        col=fill
      )
    }
    lines(rsg.sol$points[[state]], col=col,lwd=lwd, type=type ,lty=lty)
  }
  #suppressWarnings(par(old.par))
}
