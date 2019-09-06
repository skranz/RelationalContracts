

arms.race.example = function() {

  A.fun = function(x1,x2, x.max,...) {
    restore.point("A.fun")
    list(
      A1=list(h1=0:x1, i1=c(if (x1>0) "d","",if (x1<x.max) "b")),
      A2=list(h2=0:x2, i2=c(if (x2>0) "d","",if (x2<x.max) "b"))
    )
  }

  vec.pi.fun = function(ax.df,ch,cb,cx,...) {
    restore.point("pi.fun")
    transmute(ax.df,
      x = x,
      pi1 = -ch*h1 - h2 - cb*(i1=="b")-cx*x1,
      pi2 = -ch*h2 - h1 - cb*(i2=="b")-cx*x2
    )
  }

  trans.fun = function(x,x1,x2,a.df,x.max, success.prob,fixed.states = FALSE,...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()

    a.df = unique(select(a.df, i1,i2))
    sp = success.prob

    trans = rbind(
      # (1,0)
      mutate(a.df, g1=1,g2=0,
        prob= (i1=="b")*sp* ( (i2=="b")*(1-sp)+(i2==""))),
      # (0,1)
      mutate(a.df, g1=0,g2=1,
        prob= (i2=="b")*sp* ( (i1=="b")*(1-sp)+(i1==""))),
      # (1,1)
      mutate(a.df, g1=1,g2=1,
        prob= (i1=="b")*sp*(i2=="b")*sp),
      # (-1,0)
      mutate(a.df, g1=-1,g2=0,
        prob= (i1=="d")*( (i2=="b")*(1-sp)+(i2==""))),
      # (0,-1)
      mutate(a.df, g1=0,g2=-1,
        prob= (i2=="d")*( (i1=="b")*(1-sp)+(i1==""))),
      # (-1,-1)
      mutate(a.df, g1=-1,g2=-1,
        prob= (i1=="d")*(i2=="d")),
      # (-1,1)
      mutate(a.df, g1=-1,g2=1,
        prob= (i1=="d")*( (i2=="b")*sp)),
      # (1,-1)
      mutate(a.df, g1=1,g2=-1,
        prob= (i2=="d")*( (i1=="b")*sp))
    ) %>%
      filter(prob > 0 )

    trans = mutate(trans,
        nx1 = pmin(x.max,pmax(x1+g1,0)),
        nx2 = pmin(x.max,pmax(x2+g2,0)),
        xd = paste0(nx1,"_",nx2),
        xs=x
      ) %>%
      filter(xs != xd) %>%
      select(xs,xd,i1,i2,prob)
    trans
  }


  x.max = 3
  x.df = as_data_frame(expand.grid(x1=0:x.max,x2=0:x.max))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)

  g = rel_game("Arms Race") %>%
    rel_param(delta=0.99, rho=0.7, ch=0.1,cb=0.1, cx=0.13,x.max=x.max, success.prob=1) %>%
    rel_states(x.df,A.fun=A.fun, vec.pi.fun=vec.pi.fun, trans.fun=trans.fun) %>%
    rel_compile() %>%
    rel_capped_rne(T=1000)

  rne = get.eq(g)
  #g = rel_rne_from_eq_actions(g, iterations=10)

  rne.diagram(g, show.own.loop = !TRUE, just.eq.chain = !TRUE)
  rne.diagram(g, show.own.loop = TRUE, just.eq.chain = !TRUE, label.fun = function(rne,...)  paste0(rne$x1, " ", rne$x2))

  res = rne.diagram(g, show.own.loop = FALSE, just.eq.chain = !TRUE, return.dfs = TRUE, label.fun = function(rne,...)  paste0(rne$x1, " ", rne$x2), passive.edge.color = "#FFFFFF")
  ndf = res$ndf; edf = res$edf
  ndf$value = 5
  render_graph(create_graph(ndf, edf), output="visNetwork")


  compare.eq(rne2,rne3)

  res = bind_rows(rne1, rne2, rne3)

  #rne = g$eq %>% filter(t<max(g$eq$t), t==1)
  #rne

  de = get.rne.details(g)
  d = filter(de, t==1, x=="0_0")


  d = rne %>%
    filter(can.ae==2) %>%
    mutate(iv1 =  0 + (i1=="b") - (i1=="d"),iv2 = 0 + (i2=="b") - (i2=="d"))

  library(ggplot2)
  ggplot(d, aes(x=t,y=iv1)) + geom_point(size=1.5, color="red", alpha=0.5) + facet_grid(x1~x2, labeller=label_both) + geom_point(aes(x=t,y=iv2), size=1.5, color="blue", alpha=0.5) + theme_bw()

  de = rne %>% filter(x %in% c("3_0"), t==5) %>%
    filter(h1==0, h2==0)
  de

  View(rne)
}

# Arms race with possibility to destroy other player's weapons
# The probability to destroy weapons increases in the own arsenal

arms.race.destroy.example = function() {

  A.fun = function(x1,x2, x.max,...) {
    restore.point("A.fun")
    list(
      A1=list(h1=unique(c(0,x1)), i1=c(if (x1>0) "d","",if (x1<x.max) "b")),
      A2=list(h2=unique(c(0,x2)), i2=c(if (x2>0) "d","",if (x2<x.max) "b"))
    )
  }

  vec.pi.fun = function(ax.df,ch,cb,cx,...) {
    restore.point("pi.fun")
    transmute(ax.df,
      x = x,
      pi1 = -ch*h1 - h2 - cb*(i1=="b")-cx*x1,
      pi2 = -ch*h2 - h1 - cb*(i2=="b")-cx*x2
    )
  }

  vec.trans.fun = function(ax.df,x.max, success.prob,fixed.states = FALSE,d.factor=1, d.exp=1,...) {
    restore.point("vec.trans.fun")
    #if (x=="0_0") stop()

    ax.df = mutate(ax.df,
      dp1 = d.factor*(x1 / x.max)^d.exp,
      dp2 = d.factor*(x2 / x.max)^d.exp
    )
    sp = success.prob

    tr = independent.transitions(ax.df,
      trans_var("b1",default=0,
        trans_val(1, (i1=="b")*sp),
        trans_val(-1, (i1=="d")*1)
      ),
      trans_var("b2",default=0,
        trans_val(1, (i2=="b")*sp),
        trans_val(-1, (i2=="d")*1)
      ),
      trans_var("d1",default=0,
        trans_val(1, (h2>0)*dp2)
      ),
      trans_var("d2",default=0,
        trans_val(1, (h1>0)*dp1)
      )
    )

    trans = tr %>%
      mutate(g1 = b1-d1, g2=b2-d2) %>%
      mutate(
        nx1 = pmin(x.max,pmax(x1+g1,0)),
        nx2 = pmin(x.max,pmax(x2+g2,0)),
        xd = paste0(nx1,"_",nx2),
        xs=x
      ) %>%
      group_by(xs,x1,x2,xd,i1,i2,h1,h2) %>%
      summarize(prob = sum(prob)) %>%
      ungroup() %>%
      filter(xs != xd) %>%
      select(xs,xd,h1,h2,i1,i2,prob)
    trans
  }


  x.max = 3
  x.df = as_tibble(expand.grid(x1=0:x.max,x2=0:x.max))
  x.df$x = paste0(x.df$x1,"_", x.df$x2)

  g = rel_game("Arms Race") %>%
    rel_param(delta=0.9, rho=0.4, ch=0.1,cb=0.1, cx=0.01,x.max=x.max, success.prob=0.5, d.factor=0.8, d.exp=1) %>%
    rel_states(x.df,A.fun=A.fun, vec.pi.fun=vec.pi.fun, vec.trans.fun=vec.trans.fun) %>%
    rel_compile() %>%
    rel_capped_rne(T=1000)

  rne = get.eq(g)

  df.li = rne.diagram(g, show.own.loop = !TRUE, just.eq.chain = !TRUE, return.dfs = TRUE, passive.edge.width = 0)
  edf = df.li$edf
  ndf = left_join(df.li$ndf, rne, by="x")
  ndf = ndf %>% mutate(
    conflict = ae.h1 != 0 | ae.h2 != 0,
    shape = ifelse(conflict, "box","circle"),
    type = ifelse(conflict, "conflict_node", "peace_node")
  )
  graph = create_graph(ndf, edf)
  render_graph(graph, output="visNetwork")



  rne.diagram(g, show.own.loop = !TRUE, just.eq.chain = TRUE)
