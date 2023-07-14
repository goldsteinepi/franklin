#################
# An ounce of prevention is worth a pound of cure
# Citation: Goldstein ND, LeVasseur MT. One Ounce of COVID-19 Prevention is Worth 0.46 Pounds of Cure. Manuscript in preparation.
# 10/12/22 -- Neal Goldstein
#################

#vaccination (assumed for Pfizer shot: https://labeling.pfizer.com/ShowLabeling.aspx?id=16351&format=pdf)
comirnaty = 0.3 #ml
comirnaty_doses = 2
saline_density = 1.0046 #g/mL https://www.answers.com/chemistry/What_is_the_density_of_normal_saline
prevention_oz = comirnaty*comirnaty_doses*0.03519508*saline_density
comirnaty_efficacy = 0.95 #study 2 endpoint
comirnaty_efficacy_lo = 0.903
comirnaty_efficacy_hi = 0.976

#paxlovid (https://labeling.pfizer.com/ShowLabeling.aspx?id=16474)
nirmatrelvir = 300 #mg
ritonavir = 100 #mg
paxlovid_dose = 2*5 #bd for 5 days
cure_lb = (nirmatrelvir+ritonavir)*paxlovid_dose*2.204623e-6
paxlovid_efficacy = 0.88 #primary endpoint in mITT1
paxlovid_efficacy_lo = 0.75
paxlovid_efficacy_hi = 0.94

#how many cases can be prevented with 1 oz
ppl_prev = round(1/prevention_oz*comirnaty_efficacy)
ppl_prev_lo = round(1/prevention_oz*comirnaty_efficacy_lo)
ppl_prev_hi = round(1/prevention_oz*comirnaty_efficacy_hi)

#how many people can be 'cured' with 1 lb?
ppl_cure = round(1/cure_lb*paxlovid_efficacy)
ppl_cure_lo = round(1/cure_lb*paxlovid_efficacy_lo)
ppl_cure_hi = round(1/cure_lb*paxlovid_efficacy_hi)


### ANALYSIS 1: how many pounds of cure is worth 1 oz of prevention? ###

sen = seq(0.1,1,by=0.01)
ppl_cure_sen = round(sen/cure_lb*paxlovid_efficacy)
ppl_cure_sen_lo = round(sen/cure_lb*paxlovid_efficacy_lo)
ppl_cure_sen_hi = round(sen/cure_lb*paxlovid_efficacy_hi)

#when does cure > prevention
indx = min(which(ppl_cure_sen>ppl_prev))
indx_lo = min(which(ppl_cure_sen_hi>ppl_prev_lo))
indx_hi = min(which(ppl_cure_sen_lo>ppl_prev_hi))
sen[indx]
sen[indx_lo]
sen[indx_hi]

#plot results
plot(x=ppl_cure_sen, y=sen, xlab="Cases hypothetically cured", ylab="Pounds of antiviral", type="l", lwd=3)
polygon(x=c(min(ppl_cure_sen_hi),max(ppl_cure_sen_hi),max(ppl_cure_sen_lo),min(ppl_cure_sen_lo)), y=c(min(sen),max(sen),max(sen),min(sen)), col="gray", border=NA)
#lines(x=ppl_prev_sen_lo, y=sen)
#lines(x=ppl_prev_sen_hi, y=sen)
lines(x=ppl_cure_sen, y=sen, lwd=3)
lines(x=c(ppl_prev,ppl_prev), y=c(min(sen),sen[indx]), lty=2)
lines(x=c(0,ppl_prev), y=c(sen[indx],sen[indx]), lty=2)
lines(x=c(ppl_prev_lo,ppl_prev_lo), y=c(min(sen),sen[indx_lo]), lty=3)
lines(x=c(0,ppl_prev_lo), y=c(sen[indx_lo],sen[indx_lo]), lty=3)
lines(x=c(ppl_prev_hi,ppl_prev_hi), y=c(min(sen),sen[indx_hi]), lty=3)
lines(x=c(0,ppl_prev_hi), y=c(sen[indx_hi],sen[indx_hi]), lty=3)
#abline(v=ppl_prev, lty=2)
#abline(v=ppl_prev_lo, lty=3)
#abline(v=ppl_prev_hi, lty=3)


### ANALYSIS 2: how many ounces of prevention is worth 1 lb of cure? ###

sen = seq(0.5,3.5,by=0.1)
ppl_prev_sen = round(sen/prevention_oz*comirnaty_efficacy)
ppl_prev_sen_lo = round(sen/prevention_oz*comirnaty_efficacy_lo)
ppl_prev_sen_hi = round(sen/prevention_oz*comirnaty_efficacy_hi)

#when does prevention > cure
indx = min(which(ppl_prev_sen>ppl_cure))
indx_lo = min(which(ppl_prev_sen_hi>ppl_cure_lo))
indx_hi = min(which(ppl_prev_sen_lo>ppl_cure_hi))
sen[indx]
sen[indx_lo]
sen[indx_hi]

#plot results
plot(x=ppl_prev_sen, y=sen, xlab="Cases hypothetically prevented", ylab="Ounces of vaccine", type="l", lwd=3)
polygon(x=c(min(ppl_prev_sen_hi),max(ppl_prev_sen_hi),max(ppl_prev_sen_lo),min(ppl_prev_sen_lo)), y=c(min(sen),max(sen),max(sen),min(sen)), col="gray", border=NA)
#lines(x=ppl_prev_sen_lo, y=sen)
#lines(x=ppl_prev_sen_hi, y=sen)
lines(x=ppl_prev_sen, y=sen, lwd=3)
lines(x=c(ppl_cure,ppl_cure), y=c(min(sen),sen[indx]), lty=2)
lines(x=c(0,ppl_cure), y=c(sen[indx],sen[indx]), lty=2)
lines(x=c(ppl_cure_lo,ppl_cure_lo), y=c(min(sen),sen[indx_lo]), lty=3)
lines(x=c(0,ppl_cure_lo), y=c(sen[indx_lo],sen[indx_lo]), lty=3)
lines(x=c(ppl_cure_hi,ppl_cure_hi), y=c(min(sen),sen[indx_hi]), lty=3)
lines(x=c(0,ppl_cure_hi), y=c(sen[indx_hi],sen[indx_hi]), lty=3)
#abline(v=ppl_cure, lty=2)
#abline(v=ppl_cure_lo, lty=3)
#abline(v=ppl_cure_hi, lty=3)
