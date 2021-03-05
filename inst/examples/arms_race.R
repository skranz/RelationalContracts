arms.race.example = function() {
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
  x.max = 3
  x.df = tidyr::expand_grid(x1=0:x.max,x2=0:x.max) %>%
    mutate(x = paste0(x1,"_", x2))


  # Article version Attempt
  g = rel_game("Arms Race") %>%
    rel_param(delta=0.99, rho=0.65, c.a=0.05,c.i=0.01, c.x=0.2,x.max=x.max, sp=0.08, d.factor=0, d.exp=1, can.destroy=TRUE) %>%
    rel_states(x.df,A.fun=A.fun, trans.fun=trans.fun,
      pi1 = -c.a*(a1=="a")*x1 - (a2=="a")*x2 - c.i*(i1=="b")-c.x*x1,
      pi2 = -c.a*(a2=="a")*x2 - (a1=="a")*x1 - c.i*(i2=="b")-c.x*x2
    )

  g.fb = g %>% rel_first_best()
  g1 = g %>% rel_T_rne(T=1000)
  g2 = g %>% rel_change_param(d.factor = 0.5) %>%
    rel_T_rne(T=1000)

  res = plot.arms.race(g1)%>%
    visPhysics(barnesHut=list(gravitationalConstant=-500))

  plot.arms.race(g2)

  plot.arms.race(g.fb)

  g1 = g %>%
    rel_options(lab.action.sep = "") %>%
    rel_T_rne(T=1000,save.details = TRUE)

  det = g1 %>%
    get_rne_details(x="1_0") %>%
    filter(i1!="d", i2!="d")

  det = g1 %>%
    get_rne_details(x="2_2")



  # Article version alternative: set sp 0.08 and 1. For g2 keep sp on 0.08
  g = rel_game("Arms Race") %>%
    rel_param(delta=0.99, rho=0.65, c.a=0.05,c.i=0.01, c.x=0.3,x.max=x.max, sp=0.08, d.factor=0, d.exp=1, can.destroy=TRUE) %>%
    rel_states(x.df,A.fun=A.fun, trans.fun=trans.fun,
      pi1 = -c.a*(a1=="a")*x1 - (a2=="a")*x2 - c.i*(i1=="b")-c.x*x1,
      pi2 = -c.a*(a2=="a")*x2 - (a1=="a")*x1 - c.i*(i2=="b")-c.x*x2
    )

  g1 = g %>% rel_T_rne(T=1000)
  g2 = g %>% rel_change_param(d.factor = 0.5) %>%
    rel_T_rne(T=1000)

  plot.arms.race(g1)%>%
    visPhysics(barnesHut=list(gravitationalConstant=-500))

  plot.arms.race(g2)


  # 2nd Attempt
  g = rel_game("Arms Race") %>%
    rel_param(delta=0.99, rho=0.65, c.a=0.05,c.i=0.01, c.x=0.3,x.max=x.max, sp=0.08, d.factor=0, d.exp=1) %>%
    rel_states(x.df,A.fun=A.fun, trans.fun=trans.fun,
      pi1 = -c.a*(a1=="a")*x1 - (a2=="a")*x2 - c.i*(i1=="b")-c.x*x1,
      pi2 = -c.a*(a2=="a")*x2 - (a1=="a")*x1 - c.i*(i2=="b")-c.x*x2
    )

  g1 = g %>% rel_T_rne(T=1000)
  g1b = g %>% rel_change_param(sp=1) %>% rel_T_rne(T=1000)
  g2 = g %>% rel_change_param(d.factor = 0.5) %>% rel_T_rne(T=1000)

  plot.arms.race(g1)%>%
    visPhysics(barnesHut=list(gravitationalConstant=-500))
  plot.arms.race(g1b)%>%
    visPhysics(barnesHut=list(gravitationalConstant=-500))

  plot.arms.race(g2)

  g %>% rel_T_rne(T=1000, tie.breaking = "max_r1") %>% plot.arms.race()
  g %>% rel_T_rne(T=1000, tie.breaking = "max_r2") %>% plot.arms.race()

  # 3rd Attempt
  g = rel_game("Arms Race") %>%
    rel_param(delta=0.95, rho=0.6, c.a=0.05,c.i=0.01, c.x=0.5,x.max=x.max, sp=0.2, d.factor=0, d.exp=1) %>%
    rel_states(x.df,A.fun=A.fun, trans.fun=trans.fun,
      pi1 = -c.a*(a1=="a")*x1 - (a2=="a")*x2 - c.i*(i1=="b")-c.x*x1,
      pi2 = -c.a*(a2=="a")*x2 - (a1=="a")*x1 - c.i*(i2=="b")-c.x*x2
    )

  g1 = g %>% rel_T_rne(T=1000)
  g2 = g %>% rel_change_param(d.factor = 0.8) %>% rel_T_rne(T=1000)

  plot.arms.race(g1)
  plot.arms.race(g2)

  # Question: Why do we destroy and build in state (3,1)?
  g1 = g %>%
    rel_options(lab.action.sep = "") %>%
    rel_T_rne(T=1000,save.details = TRUE)

  det = g1 %>%
    get_rne_details(x="1_1") %>%
    filter(i1!="d", i2!="d")
  det

  eq = get_eq(g1)


}

plot.arms.race = function(g, eq=get_eq(g)) {
  restore.point("plot.arms.race")

  res = eq_diagram(g,eq = eq, return.dfs = TRUE, passive.edge.width = 0)
  edf = res$edf
  ndf = left_join(res$ndf, eq, by="x")
  ndf = ndf %>% mutate(
    conflict = ae.a1 == "a" | ae.a2 == "a",
    shape = ifelse(conflict, "box","circle"),
    type = ifelse(conflict, "conflict_node", "peace_node"),
    label = paste0(x1, " ",x2),
    font = "30px Arial black"
  )
  library(visNetwork)
  visNetwork(ndf, edf) %>%
  visPhysics(barnesHut=list(gravitationalConstant=-500))

  graph = create_graph(ndf, edf)
  render_graph(graph, output="visNetwork")
}

plot.turns.arms.race = function(g, eq=get_eq(g), simple.label=TRUE) {

  eq_group = eq %>%
    group_by(x1,x2, xgroup) %>%
    summarize(
      attack1 = any(ae.a1=="a", na.rm=TRUE),
      attack2 = any(ae.a2=="a", na.rm=TRUE),
      b1 = any(ae.i1=="b", na.rm=TRUE),
      b2 = any(ae.i2=="b", na.rm=TRUE),
      d1 = any(ae.i1=="d", na.rm=TRUE),
      d2 = any(ae.i2=="d", na.rm=TRUE),
      r1 = round(mean(r1),1),
      r2 = round(mean(r2),1)
    )

  res = eq_diagram_xgroup(g,return.dfs = TRUE, passive.edge.width = 0)
  edf = res$edf

  ndf = left_join(res$ndf, eq_group, by="xgroup")
  ndf = ndf %>% mutate(
    conflict = attack1 | attack2,
    shape = ifelse(conflict, "box","circle"),
    type = ifelse(conflict, "conflict_node", "peace_node"),
    title = paste0(xgroup,"<br>",
      ifelse(attack1,"a","w"), ifelse(b1,"b","") , ifelse(d1,"d",""),
      "_",
      ifelse(attack2,"a","w"),ifelse(b2,"b","") , ifelse(d2,"d",""),
      "<br>",
      r1, "_",r2
    )

  )
  if (!simple.label) {
    ndf = ndf%>% mutate(
      label = paste0(xgroup,"\n",
        ifelse(attack1,"a",""), ifelse(b1,"b","") , ifelse(d1,"d",""),
        " ",
        ifelse(attack2,"a",""),ifelse(b2,"b","") , ifelse(d2,"d",""),
        "\n",
        r1, " ",r2
      )
    )
  } else {
    ndf = ndf%>% mutate(
      label = paste0(x1," ",x2),
      font = "30px Arial black"
    )
  }

  graph = create_graph(ndf, edf)
  render_graph(graph, output="visNetwork")
}
