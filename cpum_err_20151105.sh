#!/bin/bash -v

export ROOT=cpum_err_20151105
#compile model code
cd ~/repo/cmdstan/  #killdevil, softlinked to ~/repo/cmdstan on local
make ~/EpiProjects/CPUM/code/$ROOT
cd ~/EpiProjects/CPUM/code/
#mv $ROOT models/
#rm $ROOT.o $ROOT.d $ROOT.hpp $ROOT.cpp #clean up
#cd models



./$ROOT \
    method=optimize \
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_datafull.standata \
	init=~/EpiProjects/CPUM/inits/cpum_err_20151105_inits.txt  \
	output file=~/EpiProjects/CPUM/output/${ROOT}_optim.csv \
	random seed=${RANDOM} 
~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_optim.csv




./$ROOT \
    method=sample num_samples=1000 num_warmup=250 save_warmup=1\
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_datafull.standata \
	init=~/EpiProjects/CPUM/inits/cpum_err_20151105_inits.txt  \
	output file=~/EpiProjects/CPUM/output/${ROOT}_sample_full.csv \
	       diagnostic_file=~/EpiProjects/CPUM/output/${ROOT}_dx.csv  \
	random seed=${RANDOM} 
~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_sample_full.csv
#err  0.635


./$ROOT \
    method=sample num_samples=1000 num_warmup=250 save_warmup=1\
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata \
	init=~/EpiProjects/CPUM/inits/cpum_err_20151105_inits.txt  \
	output file=~/EpiProjects/CPUM/output/${ROOT}_sample.csv \
	random seed=${RANDOM} 
~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_sample.csv
#err 0.254