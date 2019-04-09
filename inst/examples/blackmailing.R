
brinkmanship.fixed.point = function() {

  rho = 0.7
  delta = 0.8
  beta1 = 0.6

  phi.star = (1-delta)*(1-rho) / (rho*beta1+(1-delta)*(1-rho))
  phi.star
  phi = 0.1

  g = rel_game("Blackmailing with Endogenous Brinkmanship") %>%
    rel_param(delta=delta, rho=rho, phi=phi) %>%
    # Initial State
    rel_state("x0", A1=list(reveal=c(0,phi,1)),A2=NULL) %>%
    rel_payoff("x0",pi1=0, pi2=1) %>%
    rel_transition("x0","x1",reveal=c(phi,1), prob=c(phi,1)) %>%
    # Evidence Revealed
    rel_state("x1", A1=NULL,A2=NULL) %>%
    rel_payoff("x1",pi1=0, pi2=0) %>%
    rel_compile()

  r1.seq = seq(0,1,by=0.05)

  li = lapply(r1.seq, function(r1) {
    r1 = 0.2
    r2 = 1-r1


    g = rel_spe(g, rho = rho, r1=c(r1,0),r2=c(r2,0))

    r1_inp = r1
    get.spe(g) %>%
      filter(x=="x0") %>%
      transmute(rho=rho,r1_inp = r1_inp, r1_out=r1, U=U, v2=v2)
  })


}




brinkmanship = function() {
  reveal = seq(0,1,by=0.4)
  #reveal = c(0,0.51)
  g = rel_game("Blackmailing with Endogenous Brinkmanship") %>%
    rel_param(delta=0.7, rho=0.5) %>%
    # Initial State
    rel_state("x0", A1=list(reveal=reveal),A2=NULL) %>%
    rel_payoff("x0",pi1=0, pi2=1) %>%
    rel_transition("x0","x1",reveal=reveal, prob=reveal) %>%
    # Evidence Revealed
    rel_state("x1", A1=NULL,A2=NULL,x.T="xT") %>%
    rel_payoff("x1",pi1=0, pi2=0) %>%
    rel_compile()

  g = g %>% rel_T_rne(T=83, save.history = TRUE, use.cpp=TRUE, T.rne=TRUE, tie.breaking = "slack", save.details = TRUE)

  get.spe(g)
  get.eq(g)
  hist = g$eq.history %>% filter(x=="x0")
  hist = add.action.details(g,hist)

  det = get.rne.details(g)

  det83 = det
  det81 = det
  det82 = det

  res = bind_rows(det81,det82,det83) %>% filter(x=="x0",reveal==0.4)

  which(hist$a2.reveal>0)

  animate.capped.rne.history(g,x=NULL)


}
