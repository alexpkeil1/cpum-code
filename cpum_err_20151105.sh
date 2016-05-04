#!/bin/bash -v

export ROOT=cpum_err_20151105
#compile model code
cd ~/EpiProjects/CPUM/code/
mv models/$ROOT . 
cd ~/repo/cmdstan/  #killdevil, softlinked to ~/repo/cmdstan on local
make ~/EpiProjects/CPUM/code/$ROOT
cd ~/EpiProjects/CPUM/code/
mv $ROOT models/
rm $ROOT.o $ROOT.d $ROOT.hpp $ROOT.cpp #clean up
cd models

#Bayesian
./$ROOT \
    method=sample num_samples=1000 num_warmup=1000 save_warmup=0\
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata \
	output file=~/EpiProjects/CPUM/output/${ROOT}_sample.csv \
	random seed=${RANDOM} 
~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_sample.csv
# seems to diverge and take a long time without setting priors on the err
#full data err = .5949
#err, NA 1.22

#Variational
./$ROOT \
    method=variational iter=20000 elbo_samples=3000 grad_samples=3\
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata \
	output file=~/EpiProjects/CPUM/output/${ROOT}_variational.csv \
	random seed=${RANDOM} 
~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_variational.csv
#err, varies by seed



./$ROOT \
    method=optimize \
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata \
	init=~/EpiProjects/CPUM/inits/cpum_err_20151105_inits.txt  \
	output file=~/EpiProjects/CPUM/output/${ROOT}_optim.csv \
	random seed=${RANDOM} 
~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_optim.csv
#crude model
#full data err = .5949, sas MCMC gives 0.5903, sas NLMIXED gives 0.5877, gnm (R) gives 0.5877
#err, NA = 0.8513, sas MCMC gives , sas NLMIXED =  gives 0.8471



./$ROOT \
    method=sample num_samples=1000 num_warmup=250 save_warmup=1\
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_datafull.standata \
	init=~/EpiProjects/CPUM/inits/cpum_err_20151105_inits.txt  \
	output file=~/EpiProjects/CPUM/output/${ROOT}_sample_full.csv \
	       diagnostic_file=~/EpiProjects/CPUM/output/${ROOT}_dx.csv  \
	random seed=${RANDOM} 
~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_sample_full.csv
#full data err  0.5949


