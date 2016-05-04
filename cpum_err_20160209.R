######################################################################################################################
# Author: Alex Keil
# Program: cpum_err_20160209.R
# Language: R
# Date: Tuesday, February 9, 2015 at 6 PM
# Project: 
# Tasks:
# Data in: 
# Data out: 
# Description:
# Keywords:
# Released under the GNU General Public License: http://www.gnu.org/copyleft/gpl.html
######################################################################################################################

source("~/Documents/programming_examples/R/_mine/da_spline_ak.R", echo=TRUE)

library(haven)
#library(rstan)
library(gnm)

#an2 <- read_sas("~/EpiProjects/CPUM/data/cpum_gf01.sas7bdat")
an <- read_sas("~/EpiProjects/CPUM/data/an0001.sas7bdat")

#dim(dat <- an[an$race==2 & !is.na(an$smoke3_2),])
dim(dat <- an[!is.na(an$smoke3_2),]) #keep everybody with non-missing smoking data

dat$cumwlm2lag100 = dat$cumwlm2lag/100
dat$cumwlm2lag100r = round(dat$cumwlm2lag100)
dat$py10k = dat$py/10000
errmod <- formula(d_lc ~ -1 + Mult(offset(py10k), Exp(1), Const(1) + cumwlm2lag100))
m1 <- gnm(errmod, start=c(-1, 0),family=poisson(link="log"), data=dat) 
m2 <- gnm(errmod, start=c(2, .8), family=poisson(link="identity"), data=dat)
#not working with identity link in poisson (update - it works, had a typo in the word 'family')
m1$coefficients; m1$family 
m2$coefficients; m2$family 
