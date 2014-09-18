# Evaluate initial parameter values without the effect of climate
transitions = paste(data$t0,data$t1,sep = "")
sum_transitions = table(transitions)
tot_transitions = table(data$t0)

eps_mn = (sum_transitions[3]+sum_transitions[9])/2/length(transitions)

thetac_mn = sum_transitions[6]/tot_transitions[3]
thetad_mn = sum_transitions[7]/tot_transitions[2]

betac_mn = sum_transitions[7]*(tot_transitions[1]+tot_transitions[3])/tot_transitions[3]/sum(tot_transitions)
betad_mn = sum_transitions[7]*(tot_transitions[3]+tot_transitions[3])/tot_transitions[3]/sum(tot_transitions)

phic_mn = sum_transitions[10]/tot_transitions[4]
phid_mn = sum_transitions[11]/tot_transitions[4]

logit_eps_mn = log(eps_mn/(1-eps_mn))

logit_thetac_mn = log(thetac_mn/(1-thetac_mn))
logit_thetad_mn = log(thetad_mn/(1-thetad_mn))
 
logit_betac_mn = log(betac_mn/(1-betac_mn))
logit_betad_mn = log(betad_mn/(1-betad_mn))

# List initial parameters
par = list()
par$ac0 = 10
par$act1 = 0
par$act2 = 0
par$act3 = 0
par$acp1 = 0
par$acp2 = 0
par$acp3 = 0

par$ad0 = 10
par$adt1 = 0
par$adt2 = 0
par$adt3 = 0 
par$adp1 = 0
par$adp2 = 0 
par$adp3 = 0

par$bc0 = logit_betac_mn
par$bct1 = 0
par$bct2 = 0
par$bct3 = 0 
par$bcp1 = 0
par$bcp2 = 0 
par$bcp3 = 0

par$bd0 = logit_betad_mn
par$bdt1 = 0
par$bdt2 = 0 
par$bdt3 = 0
par$bdp1 = 0
par$bdp2 = 0 
par$bdp3 = 0

par$tc0 = logit_thetac_mn
par$tct1 = 0
par$tct2 = 0 
par$tct3 = 0
par$tcp1 = 0
par$tcp2 = 0 
par$tcp3 = 0

par$td0 = logit_thetad_mn
par$tdt1 = 0
par$tdt2 = 0 
par$tdt3 = 0 
par$tdp1 = 0
par$tdp2 = 0
par$tdp3 = 0

par$e0 = logit_eps_mn
par$et1 = 0
par$et2 = 0
par$et3 = 0 
par$ep1 = 0
par$ep2 = 0
par$ep3 = 0

# Lower bounds
par_lo = list()
par_lo$ac0 = -5
par_lo$act1 = -20
par_lo$act2 = -10
par_lo$act3 = -10
par_lo$acp1 = -10
par_lo$acp2 = -10 
par_lo$acp3 = -10

par_lo$ad0 = -5
par_lo$adt1 = -10
par_lo$adt2 = -10
par_lo$adt3 = -10
par_lo$adp1 = -10
par_lo$adp2 = -10
par_lo$adp3 = -10

par_lo$bc0 = -20
par_lo$bct1 = -10
par_lo$bct2 = -10
par_lo$bct3 = -10
par_lo$bcp1 = -10
par_lo$bcp2 = -10
par_lo$bcp3 = -10

par_lo$bd0 = -5
par_lo$bdt1 = -10
par_lo$bdt2 = -10
par_lo$bdt3 = -10
par_lo$bdp1 = -10
par_lo$bdp2 = -10
par_lo$bdp3 = -10

par_lo$tc0 = -5
par_lo$tct1 = -10
par_lo$tct2 = -10
par_lo$tct3 = -10
par_lo$tcp1 = -10
par_lo$tcp2 = -10
par_lo$tcp3 = -10

par_lo$td0 = -5
par_lo$tdt1 = -10
par_lo$tdt2 = -10
par_lo$tdt3 = -10
par_lo$tdp1 = -10
par_lo$tdp2 = -10
par_lo$tdp3 = -10

par_lo$e0 = -20
par_lo$et1 = -10
par_lo$et2 = -10
par_lo$et3 = -10
par_lo$ep1 = -10
par_lo$ep2 = -10
par_lo$ep3 = -10

# Upper bounds
par_hi = list()
par_hi$ac0 = 5
par_hi$act1 = 10
par_hi$act2 = 10
par_hi$act3 = 10
par_hi$acp1 = 10
par_hi$acp2 = 20
par_hi$acp3 = 10

par_hi$ad0 = 5
par_hi$adt1 = 10
par_hi$adt2 = 10
par_hi$adt3 = 10
par_hi$adp1 = 10
par_hi$adp2 = 10
par_hi$adp3 = 10

par_hi$bc0 = 5
par_hi$bct1 = 10
par_hi$bct2 = 10
par_hi$bct3 = 10
par_hi$bcp1 = 10
par_hi$bcp2 = 10
par_hi$bcp3 = 10

par_hi$bd0 = 5
par_hi$bdt1 = 10
par_hi$bdt2 = 10
par_hi$bdt3 = 10
par_hi$bdp1 = 10
par_hi$bdp2 = 10
par_hi$bdp3 = 10

par_hi$tc0 = 5
par_hi$tct1 = 10
par_hi$tct2 = 10
par_hi$tct3 = 10
par_hi$tcp1 = 10
par_hi$tcp2 = 10
par_hi$tcp3 = 10

par_hi$td0 = 5
par_hi$tdt1 = 10
par_hi$tdt2 = 10
par_hi$tdt3 = 10
par_hi$tdp1 = 10
par_hi$tdp2 = 10
par_hi$tdp3 = 10

par_hi$e0 = 5
par_hi$et1 = 10
par_hi$et2 = 10
par_hi$et3 = 10
par_hi$ep1 = 10
par_hi$ep2 = 10
par_hi$ep3 = 10
