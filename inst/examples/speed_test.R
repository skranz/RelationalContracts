# Speed test with an arms race example
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

for (x.max in 1:20) {
  for (T in c(1000)) {
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
  rename(build_game = comp, solve_T_RNE = T.rne) %>%
  pivot_longer(build_game:capped,names_to = "measure",values_to = "seconds")

df = filter(df, x.max <= 15, measure != "capped")
library(ggplot2)
ggplot(df, aes(y=seconds,x=num.x, color=measure)) + geom_point() + facet_grid(measure ~ .) + geom_smooth(method="lm") + xlab("Number of states") + ylab("Seconds")
ggsave("D:/libraries/RelationalHoldup/timing.PNG")


summary(lm(T.rne ~ 0+ num.x, data=dat))
summary(lm(capped ~ 0 + num.x, data=dat))
summary(lm(comp ~ 0 + num.x, data=dat))

}
