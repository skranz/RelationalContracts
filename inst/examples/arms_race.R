

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

g1 = g %>% rel_T_rne(T=1000)
g2 = g %>% rel_change_param(d.factor = 0.5) %>%
  rel_T_rne(T=1000)

plot.arms.race(g1)%>%
  visPhysics(barnesHut=list(gravitationalConstant=-500))

plot.arms.race(g2)


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
View(det)


eq = get_eq(g1)


}



old.arms.race.example = function() {

  A.fun = function(x1,x2, x.max,...) {
    restore.point("A.fun")
    list(
      A1=list(h1=0:x1, i1=c(if (x1>0) "d","",if (x1<x.max) "b")),
      A2=list(h2=0:x2, i2=c(if (x2>0) "d","",if (x2<x.max) "b"))
    )
  }

  pi.fun = function(ax.df,ch,cb,cx,...) {
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
    rel_states(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun) %>%
    rel_compile() %>%
    rel_capped_rne(T=1000)

  rne = get_eq(g)
  #g = rel_rne_from_eq_actions(g, iterations=10)

  eq_diagram(g, show.own.loop = !TRUE, just.eq.chain = !TRUE)
  eq_diagram(g, show.own.loop = TRUE, just.eq.chain = !TRUE, label.fun = function(rne,...)  paste0(rne$x1, " ", rne$x2))

  res = eq_diagram(g, show.own.loop = FALSE, just.eq.chain = !TRUE, return.dfs = TRUE, label.fun = function(rne,...)  paste0(rne$x1, " ", rne$x2), passive.edge.color = "#FFFFFF")
  ndf = res$ndf; edf = res$edf
  ndf$value = 5
  render_graph(create_graph(ndf, edf), output="visNetwork")


  compare_eq(rne2,rne3)

  res = bind_rows(rne1, rne2, rne3)

  #rne = g$eq %>% filter(t<max(g$eq$t), t==1)
  #rne

  de = get_rne_details(g)
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

old.arms.race.destroy.example = function() {

  A.fun = function(x1,x2, x.max,...) {
    restore.point("A.fun")
    list(
      A1=list(h1=unique(c(0,x1)), i1=c(if (x1>0) "d","",if (x1<x.max) "b")),
      A2=list(h2=unique(c(0,x2)), i2=c(if (x2>0) "d","",if (x2<x.max) "b"))
    )
  }

  pi.fun = function(ax.df,ch,cb,cx,...) {
    restore.point("pi.fun")
    transmute(ax.df,
      x = x,
      pi1 = -ch*h1 - h2 - cb*(i1=="b")-cx*x1,
      pi2 = -ch*h2 - h1 - cb*(i2=="b")-cx*x2
    )
  }

  trans.fun = function(ax.df,x.max, success.prob,fixed.states = FALSE,d.factor=1, d.exp=1,...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()

    ax.df = mutate(ax.df,
      dp1 = d.factor*(x1 / x.max)^d.exp,
      dp2 = d.factor*(x2 / x.max)^d.exp
    )
    sp = success.prob

    tr = independent_transitions(ax.df,
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
    rel_states(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun) %>%
    rel_compile() %>%
    rel_capped_rne(T=1000)

  rne = get_eq(g)

  df.li = eq_diagram(g, show.own.loop = !TRUE, just.eq.chain = !TRUE, return.dfs = TRUE, passive.edge.width = 0)
  edf = df.li$edf
  ndf = left_join(df.li$ndf, rne, by="x")
  ndf = ndf %>% mutate(
    conflict = ae.h1 != 0 | ae.h2 != 0,
    shape = ifelse(conflict, "box","circle"),
    type = ifelse(conflict, "conflict_node", "peace_node")
  )
  graph = create_graph(ndf, edf)
  render_graph(graph, output="visNetwork")



  eq_diagram(g, show.own.loop = !TRUE, just.eq.chain = TRUE)
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

speed.test = function() {
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


arms.speed.test = function(x.max=3, T=1000) {
  # Maximum size of weapons arsenal
  x.df = tidyr::expand_grid(x1=0:x.max,x2=0:x.max) %>%
    mutate(x = paste0(x1,"_", x2))


  before.comp = Sys.time()
  # Article version Attempt
  g = rel_game("Arms Race") %>%
    rel_param(delta=0.99, rho=0.65, c.a=0.05,c.i=0.01, c.x=0.2,x.max=x.max, sp=0.08, d.factor=0, d.exp=1, can.destroy=TRUE) %>%
    rel_states(x.df,A.fun=A.fun, trans.fun=trans.fun,
      pi1 = -c.a*(a1=="a")*x1 - (a2=="a")*x2 - c.i*(i1=="b")-c.x*x1,
      pi2 = -c.a*(a2=="a")*x2 - (a1=="a")*x1 - c.i*(i2=="b")-c.x*x2
    ) %>%
    rel_compile()

  after.comp = Sys.time()

  before.T.rne = Sys.time()
  g1 = g %>% rel_T_rne(T=T)
  after.T.rne = Sys.time()

  before.capped = Sys.time()
  g1 = g %>% rel_capped_rne(T=T)
  after.capped = Sys.time()

  tibble(time=Sys.time(),x.max = x.max,T=T, comp=as.double(after.comp-before.comp), T.rne = as.double(after.T.rne-before.T.rne), capped=as.double(after.capped-before.capped))
}

file = "D:/libraries/RelationalHoldup/speed.csv"
con = file(file, "at")

for (x.max in 1:15) {
  for (T in c(1000,2000)) {
    cat("x.max = ",x.max, " T = ", T,"\n")
    res = arms.speed.test(x.max=x.max,T=T)
    writeLines(paste0(unlist(res), collapse=","), con)
  }
}
close(con)

dat = read.csv(file)

dat = dat %>% mutate(num.x = (x.max+1)^2)

library(tidyr)
df = dat %>%
  pivot_longer(comp:capped,names_to = "measure",values_to = "seconds")

library(ggplot2)
ggplot(df, aes(y=seconds,x=num.x, color=measure)) + geom_point() + facet_grid(measure ~ T) + geom_smooth(method="lm")

summary(lm(T.rne ~ 0+T + num.x, data=dat))
summary(lm(capped ~ 0+T + num.x, data=dat))
summary(lm(comp ~ 0+T + num.x, data=dat))

}
