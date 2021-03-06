######################################################################################################################
# Author: Alex Keil
# Program: cpum_bayesgf_20151029.R
# Language: R
# Date: Thursday, October 29, 2015 at 3:30:54 PM
# Project: 
# Tasks:
# Data in: 
# Data out: 
# Description:
# Keywords:
# Released under the GNU General Public License: http://www.gnu.org/copyleft/gpl.html
#changes: 11/5/2015 calculate exposure only from baseline
#         11/6/2015 using different data
######################################################################################################################

source("~/Documents/programming_examples/R/_mine/da_spline_ak.R", echo=TRUE)

library(haven)
library(rstan)
#library(gnm)

#an2 <- read_sas("~/EpiProjects/CPUM/data/cpum_gf01.sas7bdat")
an <- read_sas("~/EpiProjects/CPUM/data/an0001.sas7bdat")


#variables to keep
#restrict to nonwhites
switch=0 #indicator to help avoid running a problem below with defining exposures after baseline if running multiple times
#dim(dat <- an[an$race==2 & !is.na(an$smoke3_2),])
dim(dat <- an[!is.na(an$smoke3_2),]) #keep everybody with non-missing smoking data


dat$maxage = pmin(90, ceiling(dat$ageatadmincens))
dat$minage = pmax(18, ceiling(dat$eligage)) #should use new version of eligibility age (maybe this is already done?)
dat$cohort_1 = (dat$cohort<=2)
dat$cohort_2 = (dat$cohort>2)
dat$yearout = round(dat$dateout)
dat$py <- dat$ageout-dat$agein

dat$ageoutstd <- (dat$ageout-mean(dat$ageout))/sd(dat$ageout)
head(sp <- rcs(dat$ageoutstd, nk = 5))
dat$ageoutstd_sp1 <- sp[,1]
dat$ageoutstd_sp2 <- sp[,2]
dat$ageoutstd_sp3 <- sp[,3]
dat$dateoutstd <- (dat$dateout-mean(dat$dateout))/sd(dat$dateout)
head(sp <- rcs(dat$dateoutstd, nk = 5))
dat$dateoutstd_sp1 <- sp[,1]
dat$dateoutstd_sp2 <- sp[,2]
dat$dateoutstd_sp3 <- sp[,3]

dat$cumwlm2lagstd <- (dat$cumwlm2lag-mean(dat$cumwlm2lag))/sd(dat$cumwlm2lag)
head(sp <- rcs(dat$cumwlm2lagstd, nk = 4))
dat$cumwlm2lagstd_sp1 <- sp[,1]
dat$cumwlm2lagstd_sp2 <- sp[,2]
#plus code for the full version
cumx2lagcen <- NULL
rcs_stan(cumx2lagcen, nk=4, altvar=dat$cumwlm2lagstd)


dat$wlm_2_10 = pmax(0, dat$cumwlm2lag-dat$cumwlm10lag)

keep2 <- c("id","minage", "maxage", "agein", "ageout", "dateout", "wlm", "cumwlm", "wlm_2_10", "cumwlm2lag",
          "d_nonlc", "d_lc", "cohort_1", "smoke3_2", 
          "atwork", "atwork2lag", "cumyrsexp", "cumyrsexp2lag", "leftwork",
          "BL_cumwlm", "BL_cumyrsexp", "priorwlm", "cumwlm10lag", "py", 
          "ageoutstd", "ageoutstd_sp1", "ageoutstd_sp2", "ageoutstd_sp3",
          "dateoutstd", "dateoutstd_sp1", "dateoutstd_sp2", "dateoutstd_sp3",
          "cumwlm2lagstd", "cumwlm2lagstd_sp1", "cumwlm2lagstd_sp2"
          )

standat <- as.list(dat[, keep2]) #ok
standat$N <- length(unique(dat$id)) #ok
standat$minT <- 18 # inference time
standat$maxT <- 90 # inference time
standat$J <- length(standat$minT:standat$maxT) # inference time
#standat$obs <- standat$N*standat$J #inference time
standat$obscomplete <- dim(dat)[1]

#fulldata
length(standat$firstid <- cumsum(c(1, tapply(dat$id, dat$id, length)))[-(standat$N+1)]) #of ids
length(minagei <- tapply(dat$minage, dat$id, min))#of ids
length(maxagei <- tapply(dat$maxage, dat$id, min))#of ids
length(minyeari <- tapply(dat$year, dat$id, min))#of ids
length(standat$id_full <- rep(1:standat$N, maxagei-minagei+1)) #time at entry up to end of follow-up
idxyrs <- unlist(tapply(standat$id_full, standat$id_full, function(x) 0:(length(x)-1))) #list of 1:potential censoring date
length(standat$age_full <- idxyrs + rep(minagei, maxagei-minagei+1)) #time at entry up to end of follow-up
length(standat$date_full <- idxyrs + rep(minyeari, maxagei-minagei+1)) #time at entry up to end of follow-up
standat$age_fullstd <- (standat$age_full-mean(dat$ageout))/sd(dat$ageout)
standat$date_fullstd <- (standat$date_full-mean(dat$dateout))/sd(dat$dateout)
head(sp <- rcs(standat$age_fullstd, nk = 5))
standat$age_fullstd_sp1 <- sp[,1]
standat$age_fullstd_sp2 <- sp[,2]
standat$age_fullstd_sp3 <- sp[,3]
head(sp <- rcs(standat$date_fullstd, nk = 5))
standat$date_fullstd_sp1 <- sp[,1]
standat$date_fullstd_sp2 <- sp[,2]
standat$date_fullstd_sp3 <- sp[,3]


length(standat$cohort_full <- rep(tapply(dat$cohort, dat$id, min), maxagei-minagei+1)) 
length(standat$cohort_1_full <- rep(tapply(dat$cohort_1, dat$id, min), maxagei-minagei+1)) 
#length(standat$smoke3_1_full <- rep(tapply(dat$smoke3_1, dat$id, min), maxagei-minagei+1)) 
length(standat$smoke3_2_full <- rep(tapply(dat$smoke3_2, dat$id, min), maxagei-minagei+1))
length(standat$minage_full <- rep(minagei, maxagei-minagei+1))
length(standat$maxage_full <- rep(maxagei, maxagei-minagei+1))
length(standat$BL_cumwlm_full <- rep(tapply(dat$BL_cumwlm, dat$id, min), maxagei-minagei+1))
length(standat$BL_cumyrsexp_full <- rep(tapply(dat$BL_cumyrsexp, dat$id, min), maxagei-minagei+1))
length(standat$priorwlm_full <- rep(tapply(dat$priorwlm, dat$id, min), maxagei-minagei+1))

(standat$obs <- length(standat$age_full)) #inference time

rowMax <- max(sapply(standat, length)) 
flatstandat <- do.call(cbind, lapply(standat, function(x){ 
     length(x) <- rowMax 
     x 
 })) 
#
  attach(standat)
  if(rowMax==196312){
    print("full data")
    stan_rdump(as.list(names(standat)), "~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_datafull.standata")
    write.csv(flatstandat, "~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_datafull.csv")
    }
    if(rowMax==37610){
    print("N.A. data")
    stan_rdump(as.list(names(standat)), "~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata")
    write.csv(flatstandat, "~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.csv")
    }
    if(rowMax!=37610 & rowMax!=196312) print("something is wrong")
  detach(standat)

#inits for err model
runif2 <- function(N, lower, upper)  runif(N)*(upper-lower) + lower
inits <- list()
#inits$lnrr <- runif(1, .1, .9);
inits$err <- runif(1, .1, .3);
inits$a0 <- c(runif2(1, -2, 2))
#inits$d0 <- c(runif2(1, -2, 2))
#inits$a <- runif2(10, -2, 2)
#inits$d <- runif2(10, -2, 2)
attach(inits)
stan_rdump(as.list(names(inits)), "~/EpiProjects/CPUM/inits/cpum_err_20151105_inits.txt")
detach(inits)

#err model to test fit
dat$cumwlm2lag100 = dat$cumwlm2lag
dat$py10k = dat$py/10000
err.form =  as.formula(d_lc ~ -1 + Mult(Exp(1), (Const(1) + cumwlm2lag100)), offset(py10k))
summary(err.mod <- gnm(err.form, start = (startvals <- runif(2, 0, 1)), family=poisson(link="identity"), data=dat))

tail(sort(dat$ageout-dat$agein))

#checking that frequentist models will run (everyone)
summary(glm(leftwork ~ BL_cumwlm + BL_cumyrsexp + cohort_1 + cumyrsexp + cumwlm2lag + ageout + I(ageout^2)+ I(ageout^3) + dateout + I(dateout^2), data=dat[dat$atwork==1 | dat$leftwork==1,], family=binomial))
summary(mxf <- glm(log(wlm/100) ~ BL_cumwlm + BL_cumyrsexp + cohort_1 + cumyrsexp + cumwlm2lag + ageout + I(ageout^2)+ I(ageout^3) + dateout + I(dateout^2), data=dat[dat$atwork==1 & dat$wlm>0,], family=gaussian))
summary(glm(d_lc ~ BL_cumwlm + BL_cumyrsexp + cohort_1 + cumyrsexp+ I(cumyrsexp^2) + atwork2lag + cumwlm2lag + ageout + I(ageout^2)+ I(ageout^3)+ I(ageout^4) + dateout + I(dateout^2) + I(dateout^3), data=dat[!is.na(dat$smoke3_2)], family=binomial))
summary(glm(d_nonlc ~ BL_cumwlm + BL_cumyrsexp + cohort_1 + cumyrsexp+ I(cumyrsexp^2) + atwork2lag + cumwlm2lag + ageout + I(ageout^2)+ I(ageout^3)+ I(ageout^4) + dateout + I(dateout^2) + I(dateout^3), data=dat[!is.na(dat$smoke3_2)], family=binomial))

#checking that frequentist models will run (NA)
summary(glm(leftwork ~ BL_cumwlm + I(BL_cumwlm^2) + I(BL_cumwlm^3) + BL_cumyrsexp + I(BL_cumyrsexp^2) + I(BL_cumyrsexp^3) + cumyrsexp+ I(cumyrsexp^2) + I(cumyrsexp^3) + cumwlm2lag + I(cumwlm2lag^2) + I(cumwlm2lag^3) + ageout + I(ageout^2) + dateout + I(dateout^2), data=dat[dat$dateout<1977.5 & (dat$atwork==1 | dat$leftwork==1) & dat$race==2 & !is.na(dat$smoke3_2),], family=binomial))
summary(mx <- glm(log(wlm/1000) ~ py + BL_cumwlm + I(BL_cumwlm^2) + I(BL_cumwlm^3) + I(BL_cumwlm^4) + BL_cumyrsexp + I(BL_cumyrsexp^2) + I(BL_cumyrsexp^3) + cumyrsexp+ I(cumyrsexp^2) + I(cumyrsexp^3) + cumwlm2lag + I(cumwlm2lag^2) + I(cumwlm2lag^3)+ I(cumwlm2lag^4) + ageout + I(ageout^2) + dateout + I(dateout^2)+ I(dateout^3), data=dat[dat$atwork==1 & dat$wlm>0 & dat$race==2 & !is.na(dat$smoke3_2),], family=gaussian))
summary(glm(d_lc ~ I(ageout*dateout) + BL_cumwlm + BL_cumyrsexp + cumyrsexp + I(cumyrsexp^2) + cumwlm2lag + I(cumwlm2lag^2) + ageoutstd + ageoutstd_sp1 + ageoutstd_sp2 + ageoutstd_sp3 + dateoutstd + dateoutstd_sp1 + dateoutstd_sp2 + dateoutstd_sp3, data=dat[dat$race==2 & !is.na(dat$smoke3_2),], family=binomial))
summary(glm(d_nonlc ~ BL_cumwlm + BL_cumyrsexp  + cumwlm2lag  + ageoutstd + ageoutstd_sp1 + ageoutstd_sp2 + ageoutstd_sp3 + dateoutstd + dateoutstd_sp1 + dateoutstd_sp2 + dateoutstd_sp3, data=dat[dat$race==2 & !is.na(dat$smoke3_2),], family=binomial))


summary(mod <- glm(d_lc ~ BL_cumwlm + BL_cumyrsexp + cumyrsexp + I(cumyrsexp^2) + cumwlm2lag + I(cumwlm2lag^2) + ageout + I(ageout^2)+ I(ageout^3) + dateout + I(dateout^2), data=dat[dat$race==2 & !is.na(dat$smoke3_2),], family=binomial))


plot(mx)

(3688.1 + 8827.4 + 774.32 + 3652.1)/-2

# source("/Users/akeil/Documents/programming_examples/STAN/make_stan_terms.R")
# 
# #outcome models (keep it simple for now!)
# terms <- c("BL_cumwlmcen", "BL_cumyrsexpcen", "cohort_1", "cumyrsexpcen", "atwork2lag", "cumwlm2lagcen","ageoutcen") 
# intterms <- unlist(strsplit(intlist <- mkints("ageoutcen", "ageoutcen"), " "))
# intterms2 <- unlist(strsplit(intlist <- mkints("atwork", "cumwlm2lagcen"), " "))
# intterms3 <- trimints(intterms2, intterms2)
# intterms3 <- c(trimints(intterms, intterms), intterms2)
# mkmodidx("b", c(terms, intterms3), 1, indexed=FALSE)
# mkmodidx("b", c(terms, intterms3), 1, index="n", indexed=TRUE)
# mkmodidx("c", c(terms, intterms3), 1, indexed=FALSE)
# mkmodidx("c", c(terms, intterms3), 1, index="n", indexed=TRUE)
# 
# #exposure
# terms <- c("BL_cumwlmcen", "BL_cumyrsexpcen", "cohort_1", "cumyrsexpcen", "cumwlm2lagcen","ageoutcen") 
# intterms <- unlist(strsplit(intlist <- mkints("ageoutcen", "ageoutcen"), " "))
# intterms2 <- trimints(terms, intterms)
# mkmodidx("a", c(terms, intterms2), 1, indexed=FALSE)
# mkmodidx("a", c(terms, intterms2), 1, index="n", indexed=TRUE)
# 
# #leaving work
# terms <- c("BL_cumwlmcen", "BL_cumyrsexpcen", "cohort_1", "cumyrsexpcen", "cumwlm2lagcen","ageoutcen") 
# intterms <- unlist(strsplit(intlist <- mkints("", ""), " "))
# intterms2 <- trimints(terms, intterms)
# mkmodidx("g", c(terms, intterms2), 1, indexed=TRUE)
# mkmodidx("g", c(terms, intterms2), 1, index="n", indexed=TRUE)
# 


#standard stan model
# modfile <- "~/EpiProjects/CPUM/code/cpum_bayesgf_20151029.stan" # no measurement error correction
# #modfile <- "~/EpiProjects/CPUM/code/cpum_bayesgf_20151101.stan" #measurement error correction
# mod <- paste(readLines(modfile), collapse="\n")
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
# #options(error=recover)
# fit <- stan(model_code=mod, data=standat, iter=1000, 
#             warmup=1000, save_dso=FALSE, verbose=FALSE, pars=c("a", "b", "c", "g", "cilc_nc", "cilc_2", "cilc_1", "cilc_33", "meanCumX", "meanWkyrs"),
#             sample_file = gsub(".stan", "_R.csv", modfile), refresh=1,
#             chains=1)
# print(fit)
# plot(fit)