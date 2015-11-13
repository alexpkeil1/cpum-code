######################################################################################################################
# Author: Alex Keil
# Program: cpum_ci_20151031.R
# Language: R
# Date: October 31, 2015 at 6:30:54 PM
# Project: cumulative 
# Tasks:
# Data in: 
# Data out: 
# Description:
# Keywords:
# Released under the GNU General Public License: http://www.gnu.org/copyleft/gpl.html
######################################################################################################################

library(haven)
library(survival)
library(splines)
library(ggplot2)
library(data.table)
library(lubridate)
an <- read_sas("~/EpiProjects/CPUM/data/an0001.sas7bdat")

#variables to keep
keep <- c("id", "dob", "eligdate", "datein", "dateout", "eligage", "agein", "ageout", "wlm", "cumwlm", "cumwlm2lag", "cumwlm5lag", "cumwlm10lag",
          "cumwlm20lag", "d_any", "d_lc", "d_nonlc", "cohort", "priorwlm", "race", "smoke3_1", "smoke3_2", "py", 
          "atwork", "cumyrsexp", "atwork2lag", "atwork5lag", "leftwork", "BL_cumwlm", "BL_cumyrsexp")
#restrict to nonwhites
switch=0
dim(datf <- an[!is.na(an$smoke3_2), keep])
dim(dat <- an[an$race==2 & !is.na(an$smoke3_2), keep])



#quick stats for comparison with natural course, before and after changing data
mean(tapply(dat$cumwlm, dat$id, max)) # [1] 416.8733 [1] 467.9895
mean(tapply(dat$atwork, dat$id, sum)) # [1] 6.197381 [1] 7.002601

#full data
mean(tapply(datf$cumwlm, datf$id, max)) # [1] 416.9027
mean(tapply(datf$atwork, datf$id, sum)) # [1] 6.064743



mnx <- (tapply(datf$wlm, round(datf$ageout), mean))

plot(names(mnx), mnx)

sf <- survfit(Surv(agein, ageout, d_any)~1, data=dat)
sflc <- survfit(Surv(agein, ageout, d_lc)~1, data=dat)
sfnlc <- survfit(Surv(agein, ageout, d_nonlc)~1, data=dat)
sfw <- survfit(Surv(agein, ageout, leftwork)~1, data=dat)
surv_time <- sf$time
surv_cox <- cumprod(1-ifelse(sf$n.risk>0, sf$n.event/sf$n.risk, 0))
#dat[dat$ageout %in% surv_time[sf$n.risk==0],]
cilc_cox <- cumsum(ifelse(sflc$n.risk>0, sflc$n.event/sflc$n.risk, 0)*c(1, surv_cox[-length(surv_cox)]))
cinlc_cox <- cumsum(ifelse(sfnlc$n.risk>0, sfnlc$n.event/sfnlc$n.risk, 0)*c(1, surv_cox[-length(surv_cox)]))
hazlw_cox <- (ifelse(sfw$n.risk>0, sfw$n.event/sfw$n.risk, 0))
chazlw_cox <- cumsum(ifelse(sfw$n.risk>0, sfw$n.event/sfw$n.risk, 0))
#method 2
sbh <- basehaz(coxph(Surv(agein, ageout, d_any)~1, data=dat))
sbh_lc <- basehaz(coxph(Surv(agein, ageout, d_lc)~1, data=dat, ties="breslow"))
sbh_nlc <- basehaz(coxph(Surv(agein, ageout, d_nonlc)~1, data=dat, ties="breslow"))
sbh$s <- exp(-(sbh$hazard))
sbh$lc_ci <- cumsum(diff(c(0, sbh_lc$hazard[]))*sbh$s)
sbh$nlc_ci <- cumsum(diff(c(0, sbh_nlc$hazard[]))*sbh$s)

#by date
sbd <- basehaz(coxph(Surv(datein, dateout, d_any)~1, data=dat))
sbd_lc <- basehaz(coxph(Surv(datein, dateout, d_lc)~1, data=dat, ties="breslow"))
sbd_nlc <- basehaz(coxph(Surv(datein, dateout, d_nonlc)~1, data=dat, ties="breslow"))
sbd$s <- exp(-(sbd$hazard))
sbd$lc_ci <- cumsum(diff(c(0, sbd_lc$hazard[]))*sbd$s)
sbd$nlc_ci <- cumsum(diff(c(0, sbd_nlc$hazard[]))*sbd$s)

sbdf <- basehaz(coxph(Surv(datein, dateout, d_any)~1, data=datf))
sbd_lcf <- basehaz(coxph(Surv(datein, dateout, d_lc)~1, data=datf, ties="breslow"))
sbd_nlcf <- basehaz(coxph(Surv(datein, dateout, d_nonlc)~1, data=datf, ties="breslow"))
sbdf$s <- exp(-(sbdf$hazard))
sbdf$lc_ci <- cumsum(diff(c(0, sbd_lcf$hazard[]))*sbdf$s)
sbdf$nlc_ci <- cumsum(diff(c(0, sbd_nlcf$hazard[]))*sbdf$s)



ggplot() + 
  geom_line(aes(x,y, linetype="NA"), data=data.frame(x=sbdf$time,y=1-sbdf$s)) +
  geom_line(aes(x,y, linetype="Everyone"), data=data.frame(x=sbd$time,y=1-sbd$s)) 

#nc other causes
ncnlc <- c(0.00542484, 0.0111253, 0.0170517, 0.0232732, 0.029624, 0.0360452, 0.0425475, 0.0491374, 0.0557311, 0.0623543, 0.0689935, 0.0755977, 0.0822149, 0.0888678, 0.0955409, 0.102275, 0.109067, 0.115879, 0.122774, 0.129766, 0.136836, 0.143899, 0.151019, 0.158198, 0.165441, 0.172786, 0.180173, 0.187613, 0.195139, 0.202764, 0.210492, 0.218359, 0.226375, 0.234558, 0.242921, 0.251477, 0.260241, 0.269242, 0.27848, 0.287965, 0.297719, 0.307766, 0.318095, 0.328739, 0.339721, 0.351054, 0.362716, 0.374756, 0.38716, 0.399951, 0.413159, 0.426811, 0.440964, 0.455574, 0.4707, 0.486397, 0.502553, 0.519149, 0.536152, 0.553712, 0.571731, 0.59024, 0.60906, 0.628145, 0.647404, 0.666528, 0.685626, 0.70445, 0.722999, 0.740901, 0.757911, 0.774084, 0.789214)
nclc <- c(2.07E-05, 4.53E-05, 7.75E-05, 0.000117471, 0.000166813, 0.000228157, 0.000303372, 0.000399142, 0.000517486, 0.000670822, 0.000863065, 0.00110364, 0.00140225, 0.00176982, 0.00221871, 0.0027666, 0.0034191, 0.00417715, 0.00506221, 0.0060791, 0.00722209, 0.00846888, 0.00983393, 0.0113173, 0.0129153, 0.0146362, 0.0164462, 0.0183419, 0.0203377, 0.0224254, 0.0246045, 0.0268821, 0.0292503, 0.0317095, 0.0342526, 0.0368778, 0.039579, 0.0423553, 0.045196, 0.0480935, 0.0510435, 0.0540406, 0.057065, 0.0601205, 0.0632025, 0.0663024, 0.0694, 0.0725033, 0.0755927, 0.0786774, 0.0817536, 0.0847856, 0.0878041, 0.0908131, 0.0937815, 0.0967265, 0.0995787, 0.102195, 0.104726, 0.107241, 0.109679, 0.111981, 0.114139, 0.116217, 0.118188, 0.120016, 0.121721, 0.12335, 0.124966, 0.126414, 0.127759, 0.129043, 0.130233)
ncall <- nclc+ncnlc
nct <- 17+1:73

ggplot() + 
  geom_line(aes(x,y, linetype="all"), data=data.frame(x=nct,y=ncnlc)) +
  geom_line(aes(x,y, linetype="other"), data=data.frame(x=sbh$time,y=sbh$nlc_ci))

ggplot() + 
  geom_line(aes(x,y, linetype="all"), data=data.frame(x=nct,y=nclc)) +
  geom_line(aes(x,y, linetype="other"), data=data.frame(x=sbh$time,y=sbh$lc_ci))

ggplot() + 
  geom_line(aes(x,y, linetype="all"), data=data.frame(x=nct,y=ncall)) +
  geom_line(aes(x,y, linetype="other"), data=data.frame(x=sbh$time,y=1-sbh$s))



ggplot() + 
  geom_line(aes(x,y, linetype="all"), data=data.frame(x=sbh$time,y=1-sbh$s)) +
  geom_line(aes(x,y, linetype="other"), data=data.frame(x=sbh$time,y=sbh$nlc_ci)) +
  geom_line(aes(x,y, linetype="LC"), data=data.frame(x=sbh$time,y=sbh$lc_ci)) 

ggplot() + 
  geom_line(aes(x,y, linetype="NA"), data=data.frame(x=sbdf$time,y=sbdf$nlc_ci)) +
  geom_line(aes(x,y, linetype="Everyone"), data=data.frame(x=sbd$time,y=sbd$nlc_ci)) 


sum(sflc$n.event)

sum(dat$d)/length(unique(dat$id))

#fulldata
sf <- survfit(Surv(agein, ageout, d_any)~1, data=datf)
sflc <- survfit(Surv(agein, ageout, d_lc)~1, data=datf)
sfnlc <- survfit(Surv(agein, ageout, d_nonlc)~1, data=datf)
sfw <- survfit(Surv(agein, ageout, leftwork)~1, data=datf)
surv_timefull <- sf$time
surv_coxfull <- cumprod(1-ifelse(sf$n.risk>0, sf$n.event/sf$n.risk, 0))
#dat[dat$ageout %in% surv_time[sf$n.risk==0],]
cilc_coxfull <- cumsum(ifelse(sflc$n.risk>0, sflc$n.event/sflc$n.risk, 0)*c(1, surv_coxfull[-length(surv_coxfull)]))
cinlc_coxfull <- cumsum(ifelse(sfnlc$n.risk>0, sfnlc$n.event/sfnlc$n.risk, 0)*c(1, surv_coxfull[-length(surv_coxfull)]))
#method 2
sbh_full <- basehaz(coxph(Surv(agein, ageout, d_any)~1, data=datf))
sbh_lc_full <- basehaz(coxph(Surv(agein, ageout, d_lc)~1, data=datf, ties="breslow"))
sbh_full$s <- exp(-(sbh_full$hazard))
sbh_full$lc_ci <- cumsum(diff(c(0, sbh_lc_full$hazard))*sbh_full$s)

# ci at age 90
mean(sbh_full[sbh_full$time>89.9 & sbh_full$time<90.1,]$lc_ci)
max(sbdf[]$lc_ci) #by time, does not give same answer

ggplot() + geom_line(aes(x,y, color="Everyone, NA"), data=data.frame(x=surv_timefull,y=cilc_coxfull)) +
  geom_line(aes(x,y, color="Everyone, AJ"), data=data.frame(x=sbh_full$time,y=sbh_full$lc_ci)) +
  geom_line(aes(x,y, color="Native Americans, NA est"), data=data.frame(x=surv_time,y=cilc_cox)) +
  geom_line(aes(x,y, color="Native Americans, AJ est"), data=data.frame(x=sbh$time,y=sbh$lc_ci)) +
  coord_cartesian(xlim=c(19, 91), ylim=c(-0.01, .2)) + 
 theme_bw()



#quick parametric fit, just age
summary(fit <- glm(d_lc ~ ageout + I(ageout^2)+ I(ageout^3)+ I(ageout^4), data=dat[dat$race==2,], family=binomial))
summary(fit2 <- glm(d_any ~ ageout + I(ageout^2)+ I(ageout^3)+ I(ageout^4), data=dat[dat$race==2,], family=binomial))
newdat <- data.frame(ageout=18:90)
newdat$surv <- exp(-cumsum(predict(fit2, newdat=newdat, type="response")))
newdat$hazlc <- predict(fit, newdat=newdat, type="response")
newdat$ci_lc <- cumsum(c(1, newdat$surv[-length(newdat$surv)])*newdat$hazlc)
plot(newdat$ageout, newdat$ci_lc, type="l")
                   
ggplot() + geom_line(aes(ageout,ci_lc, color="Native Americans, Parametric age"), data=newdat) +
  geom_line(aes(x,y, color="Native Americans, NA est"), data=data.frame(x=surv_time,y=cilc_cox)) +
  geom_line(aes(x,y, color="Native Americans, AJ est"), data=data.frame(x=sbh$time,y=sbh$lc_ci)) +
  coord_cartesian(xlim=c(19, 91), ylim=c(-0.01, .1)) + 
 theme_bw()
          
         
# DF=4
# agerange = 18:90
# sv <- ns(dat$ageout, DF)
# for(i in 1:DF) dat[,paste0("sv",i)] <- sv[,i]
# mod1 <- glm(d_lc ~  I(ageout-mean(ageout)) +   I((ageout-mean(ageout))*(ageout-mean(ageout))) + I((ageout-mean(ageout))*(ageout-mean(ageout))*(ageout-mean(ageout))) + I((ageout-mean(ageout))*(ageout-mean(ageout))*(ageout-mean(ageout))*(ageout-mean(ageout)))+ I((ageout-mean(ageout))*(ageout-mean(ageout))*(ageout-mean(ageout))*(ageout-mean(ageout))*(ageout-mean(ageout))), family=binomial, data=dat[dat$ageout<89,])
# mod2 <- glm(d_nonlc ~ I(ageout-mean(ageout)) + I((ageout-mean(ageout))*(ageout-mean(ageout))) + I((ageout-mean(ageout))*(ageout-mean(ageout))*(ageout-mean(ageout))) + I((ageout-mean(ageout))*(ageout-mean(ageout))*(ageout-mean(ageout))*(ageout-mean(ageout))), family=binomial, data=dat)
# mod3 <- glm(leftwork ~ I(ageout-52.1289), family=binomial, data=dat[dat$atwork==1 | dat$leftwork==1,])
# 
# summary(mod4 <- glm(log(wlm) ~ I(ageout-52.1289), family=gaussian, data=dat[dat$wlm>0,]))
# plot(predict(mod4), log(dat[dat$wlm>0,]$wlm))
# plot(log(dat[dat$wlm>0,]$ageout), log(dat[dat$wlm>0,]$wlm))
# newdat <- data.frame(agerange, ns(agerange, df=DF, knots=attr(sv, "knots"), Boundary.knots = attr(sv, "Boundary.knots")))
# names(newdat) <- c("ageout", paste0("sv", 1:DF))
# newdat$h_lc <- predict(mod1, newdata = newdat, type="response")
# newdat$h_nlc <- predict(mod2, newdata = newdat, type="response")
# newdat$h_lw <- predict(mod3, newdata = newdat, type="response")
# newdat$h_all <- newdat$h_lc + newdat$h_nlc
#   lsurv <- c(1, cumprod(1-newdat$h_all)[-length(18:80)])
# newdat$surv <- cumprod(1-newdat$h_all)
# newdat$ci_lc <- cumsum(newdat$h_lc*lsurv)
# newdat$ci_nlc <- cumsum(newdat$h_nlc*lsurv)
# newdat$chaz_lw <- cumsum(newdat$h_lw)
# 
# #cumulative incidence, lung cancer
# 
# ggplot() + geom_step(aes(x, y), data=data.frame(x=surv_time, y=cilc_cox)) + 
#   geom_smooth(aes(x, y), data=data.frame(x=surv_time, y=cilc_cox)) + 
#   geom_line(aes(ageout, ci_lc), data=newdat, colour="blue")
# 
# 
# #cumulative incidence, all other causes
# plot(surv_time, cinlc_cox, type="l", xlim=c(min(agerange), max(agerange)))
# lines(newdat$ageout, newdat$ci_nlc, col='blue')
# 
# plot(surv_time, surv_cox, type="l", xlim=c(min(agerange), max(agerange)))
# lines(newdat$ageout, newdat$surv, type="l", col="blue")
# min(surv_cox[surv_time<=90])
# 
# #work
# plot(surv_time, chazlw_cox, type="l", xlim=c(min(agerange), max(agerange)))
# lines(newdat$ageout, newdat$chaz_lw, type="l", col="blue")
# 
# plot(surv_time, hazlw_cox, type="l", xlim=c(min(agerange), max(agerange)))
# lines(newdat$ageout, newdat$h_lw, type="l", col="blue")
# 


#reading in optim data
read_optim <- function(file){
 r2 <- fread(file, data.table=FALSE, stringsAsFactors=FALSE,sep=",", header=TRUE, nrows=2, skip=26)
 len <- length(names(r2)[grep("berkson", names(r2))])
 if(len>0) {
   dropped <- c(paste0("berkson.", 1:len), paste0("truewlm.", 1:len), paste0("truecumwlm2lag.", 1:len), paste0("truecumwlm2lagcen.", 1:len))
   r2 <- r2[, -(which(names(r2) %in% dropped))]
 }
 r2 <- r2[, grep("cilc", names(r2))]
 outdat <- data.frame(nc=t(r2[,grep( "_nc", names(r2))]), int24=t(r2[,grep( "_1", names(r2))]), int12=t(r2[,grep( "_2", names(r2))]), int4=t(r2[,grep( "_3", names(r2))]))
 outdat$age <- 18:90
 outdat
}
f1 <- "/Users/akeil/EpiProjects/CPUM/output/cpum_bayesgf_20151101_optim.csv"
f2 <- "/Users/akeil/EpiProjects/CPUM/output/cpum_bayesgf_20151029_optim.csv"
f3 <- "/Users/akeil/EpiProjects/CPUM/output/cpum_bayesgf_20151029_optim_full.csv"
f4 <- "/Users/akeil/EpiProjects/CPUM/output/cpum_bayesgf_20151101_optim_full.csv"

rdat1 <- read_optim(f1)
rdat2 <- read_optim(f2)
rdat3 <- read_optim(f3)
rdat4 <- read_optim(f4)

rdat2$nc[73] #native americans
rdat3$nc[73] #everyone


#comparing races
ggplot() + geom_line(aes(age, nc, colour="All races"), data=rdat3) +
  geom_line(aes(age, nc, colour="Native Americans"), data=rdat2) + theme_bw() + scale_color_discrete(name="Nat Course")

#comparing with edwards et al 2014
ggplot() + geom_step(aes(x, y, colour="Obs"), data=data.frame(x=surv_timefull[surv_timefull<90], y=cilc_coxfull[surv_timefull<90])) + 
  geom_line(aes(age, nc, colour="Nat. course"), data=rdat3) +
  geom_line(aes(age, int24, colour="<2WL"), data=rdat3) +
  geom_line(aes(age, int4, colour="<0.33WL"), data=rdat3) + theme_bw() + scale_color_discrete(name="Intervention")
rdat3$nc
rdat3$int4

#no measurement error correction
ggplot() + geom_step(aes(x, y, colour="Obs"), data=data.frame(x=surv_time[surv_time<90], y=cilc_cox[surv_time<90])) + 
  geom_line(aes(age, nc, colour="Nat. course"), data=rdat2) +
  geom_line(aes(age, int24, colour="<2WL"), data=rdat2) +
  geom_line(aes(age, int4, colour="<0.33WL"), data=rdat2) + theme_bw() + scale_color_discrete(name="Intervention")

#measurement error correction
ggplot() + geom_step(aes(x, y, colour="Obs"), data=data.frame(x=surv_time[surv_time<90], y=cilc_cox[surv_time<90])) + 
  geom_line(aes(age, nc, colour="Nat. course, corrected"), data=rdat1) +
  geom_line(aes(age, int24, colour="<2WL, corrected"), data=rdat1) +
  #geom_line(aes(age, int24, colour="<2WL"), data=rdat2) +
  geom_line(aes(age, int4, colour="<0.33WL, corrected"), data=rdat1) + theme_bw() + scale_color_discrete(name="Intervention")

#comparing correction
ggplot() + geom_step(aes(x, y, colour="Obs"), data=data.frame(x=surv_time[surv_time<90], y=cilc_cox[surv_time<90])) + 
  geom_line(aes(age, int24, colour="<2WL", linetype="Corrected"), data=rdat1) +
  geom_line(aes(age, int24, colour="<2WL", linetype="Uncorrected"), data=rdat2) +
  geom_line(aes(age, int4, colour="<0.33WL", linetype="Uncorrected"), data=rdat2) + 
  geom_line(aes(age, int4, colour="<0.33WL", linetype="Corrected"), data=rdat1) + 
  theme_bw() + scale_color_discrete(name="Intervention")+ scale_linetype_discrete(name="")

#full data correction
ggplot() + geom_step(aes(x, y, colour="Obs"), data=data.frame(x=surv_time[surv_time<90], y=cilc_cox[surv_time<90])) + 
  geom_line(aes(age, nc, colour="Nat. course", linetype="Corrected"), data=rdat4) +
  geom_line(aes(age, int24, colour="<2WL", linetype="Corrected"), data=rdat4) +
  geom_line(aes(age, int4, colour="<0.33WL", linetype="Corrected"), data=rdat4) + 
  theme_bw() + scale_color_discrete(name="Intervention")+ scale_linetype_discrete(name="")


######info from variational inference runs
###### does not seem to do well for the bayesian g-formula

read_variational <- function(file){
 f <- fread(file, data.table=FALSE, stringsAsFactors=FALSE,sep=",", header=TRUE)
 len <- length(names(f)[grep("berkson", names(f))])
 if(len>0) {
   dropped <- c(paste0("berkson.", 1:len), paste0("truewlm.", 1:len), paste0("truecumwlm2lag.", 1:len), paste0("truecumwlm2lagcen.", 1:len))
   f <- f[, -(which(names(f) %in% dropped))]
 }
 f <- f[, grep("cilc", names(f))]
 
 rd <- f[, grep("cilc_nc", names(f))]-f[, grep("cilc_33", names(f))]
 names(rd) <- paste0("rd", 1:73)
 f <- cbind(f, rd)
 med <- apply(f, 2, median)
 lcl <- apply(f, 2, function(x) quantile(x, 0.025))
 ucl <- apply(f, 2, function(x) quantile(x, 0.975))
 outdat <- data.frame(
                      rd=(med[grep( "rd", names(med))]), 
                      nc=(med[grep( "_nc", names(med))]), 
                      int24=(med[grep( "_1", names(med))]), 
                      int12=(med[grep( "_2", names(med))]), 
                      int4=(med[grep( "_3", names(med))]),
                      rdcl=(lcl[grep( "rd", names(lcl))]), 
                      ncl=(lcl[grep( "_nc", names(lcl))]), 
                      int24l=(lcl[grep( "_1", names(lcl))]), 
                      int12l=(lcl[grep( "_2", names(lcl))]), 
                      int4l=(lcl[grep( "_3", names(lcl))]),
                      rdcu=(ucl[grep( "rd", names(ucl))]), 
                      ncu=(ucl[grep( "_nc", names(ucl))]), 
                      int24u=(ucl[grep( "_1", names(ucl))]), 
                      int12u=(ucl[grep( "_2", names(ucl))]), 
                      int4u=(ucl[grep( "_3", names(ucl))])
                      )
 outdat$age <- 18:90
 outdat
}

 f1a <- "/Users/akeil/EpiProjects/CPUM/output/cpum_bayesgf_20151029_variational.csv"
 f1b <- "/Users/akeil/EpiProjects/CPUM/output/cpum_bayesgf_20151101_variational.csv"
 
 vdat <- read_variational(f1a)
 vdat2 <- read_variational(f1b)
 
 head(vdat)
 
 #no measurement error correction
 ggplot() + geom_step(aes(x, y, colour="Obs"), data=data.frame(x=surv_time[surv_time<90], y=cilc_cox[surv_time<90])) + 
   geom_line(aes(age, nc, colour="Nat. Course", linetype="Uncor"), data=vdat) +
   geom_ribbon(aes(x=age, ymin=ncl, ymax=ncu, fill="Nat. Course"), data=vdat, alpha=0.5) +
   geom_line(aes(age, int4, colour="<0.33 WLM", linetype="Uncor"), data=vdat) +
   geom_ribbon(aes(x=age, ymin=int4l, ymax=int4u, fill="<0.33 WLM"), data=vdat, alpha=0.5)
  
 #corrected for measurement error
 ggplot() + geom_step(aes(x, y, colour="Obs"), data=data.frame(x=surv_time[surv_time<90], y=cilc_cox[surv_time<90])) + 
   geom_line(aes(age, nc, colour="Nat. Course", linetype="Cor"), data=vdat2) +
   geom_ribbon(aes(x=age, ymin=ncl, ymax=ncu, fill="Nat. Course"), data=vdat2, alpha=0.5) +
   geom_line(aes(age, int4, colour="<0.33 WLM", linetype="Cor"), data=vdat2) +
   geom_ribbon(aes(x=age, ymin=int4l, ymax=int4u, fill="<0.33 WLM"), data=vdat2, alpha=0.5)
  
#no measurement error correction
c(vdat$nc[73], vdat$ncl[73], vdat$ncu[73])
c(vdat$int4[73], vdat$int4[73], vdat$int4[73])
c(vdat$rd[73], vdat$rdcl[73], vdat$rdcu[73])

#measurement error correction
c(vdat2$nc[73], vdat2$ncl[73], vdat2$ncu[73])
c(vdat2$int4[73], vdat2$int4[73], vdat2$int4[73])
c(vdat2$rd[73], vdat2$rdcl[73], vdat2$rdcu[73])
 