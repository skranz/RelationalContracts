cournot.capacity.example = function() {
  A.fun = function(k1,k2, k.max, k.min, q.step, ...) {
    restore.point("A.fun")
    list(
      A1=list(q1=seq(0, k1, by=q.step), i1= if (k1<k.max) c("", "b") else ""),
      A2=list(q2=seq(0, k2, by=q.step), i2= if (k2<k.max) c("","b")  else "")
    )
  }

  pi.fun = function(a.df,ic,...) {
    restore.point("pi.fun")
    transmute(a.df,
      pi1 = q1*(100-q1-q2) - ic*(i1=="b"),
      pi2 = q2*(100-q1-q2) - ic*(i2=="b")
    )
  }

  trans.fun = function(x, k1,k2,a.df, k.max, k.min,...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()
    a.df = unique(select(a.df, i1,i2))
    sp = 1

    trans = rbind(
      # (1,0)
      mutate(a.df, g1=1,g2=0,
        prob= (i1=="b")*sp* ( (i2=="b")*(1-sp)+(i2==""))),
      # (0,1)
      mutate(a.df, g1=0,g2=1,
        prob= (i2=="b")*sp* ( (i1=="b")*(1-sp)+(i1==""))),
      # (1,1)
      mutate(a.df, g1=1,g2=1,
        prob= (i1=="b")*sp*(i2=="b")*sp)
    ) %>%
      filter(prob > 0 )

    trans = mutate(trans,
        nk1 = pmin(k.max,pmax(k1+g1*k.step,k.min)),
        nk2 = pmin(k.max,pmax(k2+g2*k.step,k.min)),
        xd = paste0(nk1,"_",nk2),
        xs=x
      ) %>%
      filter(xs != xd) %>%
      select(xs,xd,i1,i2,prob)
    trans
  }


  ic = 500
  k.min = 30;k.max = 60;k.step = 10
  q.step = 1
  k.seq = seq(k.min,k.max,by=k.step)

  x.df = as_data_frame(expand.grid(k1=k.seq,k2=k.seq))
  x.df$x = paste0(x.df$k1,"_", x.df$k2)

  g = rel_game("Cournot with Capacity") %>%
    rel_param(delta=0.9, rho=0.7, c=0,k.min=k.min, k.max=k.max, k.step=k.step, q.step=q.step, ic=ic) %>%
    rel_states_fun(x.df,A.fun=A.fun, pi.fun=pi.fun, trans.fun=trans.fun) %>%
    rel_compile() %>%
    rel_capped_rne(T=50)

  rne.diagram(g,t=48)

  g=g %>% rel_capped_rne(T=10, save.details = TRUE)
}
