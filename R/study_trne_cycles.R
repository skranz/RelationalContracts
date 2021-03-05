study_convergene_example = function() {
  # An arms race
  plot.arms.race = function(g, eq=get_eq(g)) {
    restore.point("plot.arms.race")

    res = eq_diagram(g,eq = eq, return.dfs = TRUE, passive.edge.width = 0)
    edf = res$edf
    ndf = left_join(res$ndf, eq, by="x")

    if (has.col(ndf, "ae.a1")) {
      ndf = ndf %>% mutate(
        conflict = ae.a1 == "a" | ae.a2 == "a",
        shape = ifelse(conflict, "box","circle"),
        type = ifelse(conflict, "conflict_node", "peace_node"),
        label = paste0(x1, " ",x2),
        font = "30px Arial black"
      )
    }
    library(visNetwork)
    visNetwork(ndf, edf) %>%
    visPhysics(barnesHut=list(gravitationalConstant=-500))

    graph = create_graph(ndf, edf)
    res = render_graph(graph, output="visNetwork")
  }

  # Action space for each state
  A.fun = function(x1,x2, x.max, can.destroy=TRUE,...) {
    restore.point("A.fun")
    list(
      A1=list(
        a1=c("w", if (x1>0) "a"),
        i1=c(if (x1>0 & can.destroy) "d","",if (x1<x.max) "b")
      ),
      A2=list(
        a2=c("w", if (x2>0) "a"),
        i2=c(if (x2>0 & can.destroy) "d","",if (x2<x.max) "b")
      )
    )
  }

  # State transition function
  trans.fun = function(ax.df,x.max, sp,d.factor=1, d.exp=1,...) {
    restore.point("trans.fun")

    # Compute probability to destroy
    # a weapon unit of other player
    # if one attacks
    ax.df = mutate(ax.df,
      dp1 = d.factor*(x1 / x.max)^d.exp,
      dp2 = d.factor*(x2 / x.max)^d.exp
    )


    tr = irv_joint_dist(ax.df,
      irv("b1",default=0,
        irv_val(1, (i1=="b")*sp),
        irv_val(-1, (i1=="d")*1)
      ),
      irv("b2",default=0,
        irv_val(1, (i2=="b")*sp),
        irv_val(-1, (i2=="d")*1)
      ),
      irv("d1",default=0,
        irv_val(1, (a2=="a")*dp2)
      ),
      irv("d2",default=0,
        irv_val(1, (a1=="a")*dp1)
      )
    )

    tr %>%
      mutate(
        new_x1 = pmax(x1+b1-d1,0),
        new_x2 = pmax(x2+b2-d2,0),
        xd = paste0(new_x1,"_",new_x2),
        xs=x
      ) %>%
      group_by(xs,xd,i1,i2,a1,a2) %>%
      summarize(prob = sum(prob))
  }

  # Maximum size of weapons arsenal
  x.max = 2
  x.df = tidyr::expand_grid(x1=0:x.max,x2=0:x.max) %>%
    mutate(x = paste0(x1,"_", x2))


  # Article version Attempt
  g = rel_game("Arms Race") %>%
    rel_param(delta=0.99, rho=0.65, c.a=0.05,c.i=0.01, c.x=0.2,x.max=x.max, sp=0.08, d.factor=0, d.exp=1, can.destroy=TRUE) %>%
    rel_states(x.df,A.fun=A.fun, trans.fun=trans.fun,
      pi1 = -c.a*(a1=="a")*x1 - (a2=="a")*x2 - c.i*(i1=="b")-c.x*x1,
      pi2 = -c.a*(a2=="a")*x2 - (a1=="a")*x1 - c.i*(i2=="b")-c.x*x2
    )

  g = g %>% rel_T_rne(T=1000,save.history = TRUE)

  trne_cycle_info(g)

  hist = get_T_rne_history(g)
  res = find.eq.action.cycles(g, hist)
  res

  eqs = res$eqs
  x.fluc = res$x.fluc

  plot.eqs(g,eqs,"ae.group.ind", plot.arms.race)
  eqs =

  animate_capped_rne_history(g,hist = ,x="3_3")


}

trne_cycle_info = function(g, hist=get_T_rne_history(g), topn=1, max.len=100) {
  restore.point("trne_cycle_info")
  # Groups of unique actions
  key.df = hist %>%
    group_by(t) %>%
    summarize(eq.key = paste0(ae,",",a1,",",a2, collapse="|"))

  find.cycles(key.df$eq.key, max.len=max.len, topn=topn)

}

find.cycles = function(v, max.len = 100, topn=1)  {
  restore.point("find.cycles")
  x = match(v, unique(v))
  max.len = min(max.len, floor(NROW(v)/2))
  num.cycles = sapply(1:max.len, num.cycles.of.length, x=x)

  res = data.frame(max.t = num.cycles*(1:max.len), cycle.len=1:max.len, num.cycles =num.cycles, T=length(v)) %>% arrange(-max.t, cycle.len) %>% filter(num.cycles > 1)

  if (!is.null(topn)) {
    res = res[1:topn,]
  }
  res

}

num.cycles.of.length = function(cycle.len,x) {
  restore.point("num.cycles.of.length")
  nrow = floor(length(x) / cycle.len)
  if (nrow==0) return(0)

  if (cycle.len == 1) {
    str = x
  } else {
    m = matrix(x[1:(nrow*cycle.len)], ncol=cycle.len,byrow = TRUE)
    str = paste.matrix.cols(m)
  }
  if (length(str)==1) return(1)

  differ = which(str[1:(nrow-1)] != str[2:nrow])[1]
  if (is.na(differ)) return(nrow)
  differ
}


study_convergence = function(g, eq_li = NULL) {
  restore.point("study_convergence")
  if (is.data.frame(eq_li)) {
    eqs = eq_li
  } else if (is.list(eq_li)) {
    eqs = bind_rows(seq_along(eq_li), function(i) {
      eq.df = eq_li[[i]]
      eq.df$eq.ind = i
    })
  } else {
    stop("eq_li must be a data frame of one or several equilibria or a list of equilibria.")
  }

  if (!has.col(eqs,"eq.ind")) {
    nx = n_distinct(eqs$x)
    neq = NROW(eqs) / nx
    eqs$eq.ind = rep(1:neq,each=nx)
  }

  # Add labs
  eqs = add.eqs.labs(g,eqs)

  # Groups of unique actions
  key.df = eqs %>%
    group_by(eq.ind) %>%
    summarize(eq.key = paste0(ae,",",a1,",",a2, collapse="|"))
  keys = unique(key.df$eq.key)
  key.df$a.group.ind = match(key.df$eq.key, keys)

  eqs = left_join(eqs, select(key.df, eq.ind, a.group.ind))

  # Groups of unique equilibrium actions
  key.df = eqs %>%
    group_by(eq.ind) %>%
    summarize(eq.key = paste0(ae, collapse="|"))
  keys = unique(key.df$eq.key)
  key.df$ae.group.ind = match(key.df$eq.key, keys)

  eqs = left_join(eqs, select(key.df, eq.ind, ae.group.ind))

  plot(eqs$a.group.ind)




  x.fluc = eqs %>%
    group_by(x) %>%
    summarize(
      U.min = min(U),
      U.max = max(U),
      U.diff = U.max-U.min,
      u.ae = list(unique(ae.lab)),
      u.a1 = list(unique(a1.lab)),
      u.a2 = list(unique(a2.lab)),
      num.ae = length(u.ae[[1]]),
      num.a1 = length(u.ae[[1]]),
      num.a2 = length(u.ae[[1]]),
      num.a.max = max(num.ae, num.a1, num.a2),
      r1.min = min(r1),
      r1.max = max(r1),
      r2.min = min(r2),
      r2.max = max(r2)
    ) %>%
    arrange(desc(U.diff))

  list(eqs=eqs,x.fluc=x.fluc)
}

plot.eqs = function(g, eqs, unique.col="eq.ind", plot.fun=eq_diagram, show.fun = print) {
  dupl = duplicated(eqs[,c("x",unique.col)])
  eqs = eqs[!dupl,]


  for (key in unique(eqs[[unique.col]])) {
    eq = eqs[eqs[[unique.col]]==key,]
    show.fun(plot.fun(g=g,eq=eq))
  }
}


add.eqs.labs = function(g, eqs) {
  lab.df = g$a.labs.df[,1:3]
  if (is.null(lab.df)) {
    stop("g$a.labs.df is not defined.")
  }
  eqs.xa = select(eqs,x,a=ae)

  joined.lab.df = left_join(eqs.xa, lab.df, by=c("x","a"))
  eqs$ae.lab = joined.lab.df$lab

  eqs.xa$a = eqs$a1
  joined.lab.df = left_join(eqs.xa, lab.df, by=c("x","a"))
  eqs$a1.lab = joined.lab.df$lab

  eqs.xa$a = eqs$a2
  joined.lab.df = left_join(eqs.xa, lab.df, by=c("x","a"))
  eqs$a2.lab = joined.lab.df$lab

  eqs
}
