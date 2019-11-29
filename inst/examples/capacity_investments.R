examples.capacity.cournot.staggered = function() {
  pbounded = function(min,x,max) {
    pmax(min,pmin(x,max))
  }

  # Action space for each state
  A.fun = function(x1,x2,ap, x.max,...) {
    restore.point("A.fun")
    list(
      A1 = list(i1=c(0,if (ap==1) c(-1,1))),
      A2 = list(i2=c(0,if (ap==2) c(-1,1)))
    )
  }

  static.A.fun = function(x1,x2,q.step=1, x.max,...) {
    restore.point("A.fun")
    list(
      A1 = list(q1=seq(0,x1, by=q.step)),
      A2 = list(q2=seq(0,x2, by=q.step))
    )
  }

  pi.fun = function(ax.df, c.i=10,c.x=0,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = -c.i*(i1==1)-c.x*x1,
      pi2 = -c.i*(i2==1)-c.x*x2
    )
  }
  static.pi.fun = function(ax.df,x.max,a=x.max, b=1,c1=0,c2=0,...) {
    restore.point("static.pi.fun")
    res = mutate(ax.df,
      Q = q1+q2,
      p = a-b*Q,
      pi1 = (p-c1)*q1,
      pi2 = (p-c2)*q2
    )
    attr(res,"extra.col") = TRUE
    # dat = res %>% filter(x1==5, x2==5)
    res
  }

  trans.fun = function(ax.df,x.max,dep.prob=0,i.prob=1,x.min=0, ...) {
    restore.point("trans.fun")
    #if (x=="0_0") stop()
    tr = ax.df %>% irv_joint_dist(
      irv("new_ap",irv_val(1, 0.5), irv_val(2, 0.5)),
      irv("suc_1",default=0,
        irv_val(-1, (i1==-1)),
        irv_val(1, (i1==1)*i.prob)
      ),
      irv("suc_2",default=0,
        irv_val(-1, (i2==-1)),
        irv_val(1, (i2==1)*i.prob)
      ),
      irv("dep_1",default=0,
        irv_val(1, dep.prob)
      ),
      irv("dep_2",default=0,
        irv_val(1, dep.prob)
      )
    ) %>%
    filter(prob > 0)


    tr = tr %>%
      mutate(
        nx1 = pbounded(0,x1+suc_1-dep_1,x.max),
        nx2 = pbounded(0,x2+suc_2-dep_2,x.max),
        xd = paste0(new_ap,"_",nx1,"_",nx2),
        xs=x
      ) %>%
      group_by(ap,xs,xd, i1,i2) %>%
      summarize(prob=sum(prob))

  }


  x.min=0; x.max = 5; a = x.max; q.step = 0.1
  x.seq = seq(x.min,x.max, by=1)
  library(tidyr)
  x.df = expand_grid(ap=c(1:2),x1=x.seq,x2=x.seq) %>%
    mutate(
      x = paste0(ap,"_",x1,"_", x2),
      xgroup = paste0(x1,"_",x2)
    )

  g = rel_game("Cournot with Capacity Constraint") %>%
    rel_param(x.min=x.min,x.max=x.max,dep.prob=0,a=x.max,b=1, c.i=1,c.x=0, i.prob = 0.1, q.step=q.step) %>%
    rel_states(x.df, A.fun = A.fun,static.A.fun=static.A.fun,pi.fun=pi.fun,static.pi.fun = static.pi.fun, trans.fun=trans.fun) %>%
    rel_compile()

  diagnose_transitions(g)

  g = g %>%  rel_T_rne(T=1000, delta=0.9, rho=1, save.details = !TRUE) %>%
    rel_state_probs(x0="first.group")

  eq = get_eq(g)
  geq = eq_combine_xgroup(g)

  eq_diagram_xgroup(g, font.size=30)

}


