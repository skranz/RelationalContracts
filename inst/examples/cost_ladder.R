examples.cost.ladder.cournot.staggered = function() {
  pbounded = function(min,x,max) {
    pmax(min,pmin(x,max))
  }

  # Action space for each state
  A.fun = function(x1,x2,ap, x.max,...) {
    restore.point("A.fun")
    list(
      A1 = list(i1=c(0,if (ap==1) 1)),
      A2 = list(i2=c(0,if (ap==2) 1))
    )
  }

  # A cournot duopoly
  # with endogenous marginal cost
  pi.fun = function(ax.df, i.cost=10,...) {
    restore.point("pi.fun")
    mutate(ax.df,
      pi1 = -i.cost*i1,
      pi2 = -i.cost*i2
    )
  }
  static.pi.fun = function(ax.df,x.max,a=x.max, b=1,...) {
    restore.point("static.pi.fun")
    res = mutate(ax.df,
      Q = q1+q2,
      p = a-b*Q,
      pi1 = (p-x1)*q1,
      pi2 = (p-x2)*q2
    )
    attr(res,"extra.col") = TRUE
    # dat = res %>% filter(x1==5, x2==5)
    res
  }

  trans.fun = function(ax.df,x.max,dep.prob=0,i.prob=1,alpha=1,x.min=0, ...) {
    restore.point("trans.fun")

    tr = ax.df %>% irv_joint_dist(
      irv("new_ap",irv_val(1, 0.5), irv_val(2, 0.5)),
      irv("suc_1",default=0,
        irv_val(1, i1*i.prob)
      ),
      irv("suc_2",default=0,
        irv_val(1, i2*i.prob)
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
        nx1 = pbounded(0,x1-suc_1+dep_1,x.max),
        nx2 = pbounded(0,x2-suc_2+dep_2,x.max),
        xd = paste0(new_ap,"_",nx1,"_",nx2),
        xs=x
      ) %>%
      group_by(ap,xs,xd, i1,i2) %>%
      summarize(prob=sum(prob))

  }


  x.min=0; x.max = 5; a = x.max
  q.seq=seq(0,a, length=21)
  x.seq = seq(x.max,x.min, by=-1)
  library(tidyr)
  x.df = expand_grid(ap=c(1:2),x1=x.seq,x2=x.seq) %>%
    mutate(
      x = paste0(ap,"_",x1,"_", x2),
      xgroup = paste0(x1,"_",x2)
    )

  g = rel_game("Cournot with Cost Ladder") %>%
    rel_param(x.min=x.min,x.max=x.max,dep.prob=0,a=x.max,b=1, i.cost=4,i.prob = 1, alpha=1) %>%
    rel_states(x.df, A.fun = A.fun,static.A1 = list(q1=q.seq), static.A2 = list(q2=q.seq),pi.fun=pi.fun, trans.fun=trans.fun, static.pi.fun = static.pi.fun) %>%
    rel_compile()

  diagnose_transitions(g)

  g = g %>%  rel_T_rne(T=10000, delta=0.9, rho=0.6, save.details = !TRUE) %>%
    rel_state_probs(x0="equal")


  eq = get_eq(g)


  geq = eq_combine_xgroup(g)

  eq_diagram_xgroup(g, font.size=30)

}
