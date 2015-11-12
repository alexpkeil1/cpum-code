#!/bin/bash -v
#script to run stan via command line (CmdStan)
#if running this
#full data ~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_datafull.standata
#na only   ~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata

#export ROOT=cpum_bayesgf_20151029
export ROOT=cpum_bayesgf_20151029_time
#export ROOT=cpum_bayesgf_20151101
#export ROOT=cpum_err_20151105
#compile model code
cd ~/repo/cmdstan/  #killdevil, softlinked to ~/repo/cmdstan on local
make ~/EpiProjects/CPUM/code/$ROOT
cd ~/EpiProjects/CPUM/code/
#rm $ROOT.o $ROOT.d $ROOT.hpp $ROOT.cpp #clean up



###optimizing
###### testing below here (will give initial central estimates for posterior predictive quantities, too!)
###### does not seem to work well with nuanced bayes, but good for checking whether or not things 
######  are adequately debugged to give an answer
./$ROOT \
    method=optimize \
    algorithm=lbfgs iter=10000 save_iterations=0\
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata \
	output file=~/EpiProjects/CPUM/output/${ROOT}_optim.csv  \
	random seed=${RANDOM}  
~/repo/cmdstan/bin/stansummary ~/EpiProjects/CPUM/output/${ROOT}_optim.csv
#cilc_nc[73]     .27
#after removing cohort_1 : 0.25
#after removing cohort_1, cal time: 0.17
#after removing cohort_1, cal time, cuml^2: 0.27
#after removing cuml, cuml^2, lhatlag2:   .21
#after removing cumx:  .18
#remove time-fixed: .12
#remove time-fixed, cal time: 0.13
#after fixing centering issue: .26
#after fixing employment status post 1977: 0.25
#giving baseline, exposure, work time more polynomial terms: 0.11 (lp =  -8563.5)
#further refine using frequentist model specific deviances: 
#time only: 0.092



./$ROOT \
    method=optimize \
    algorithm=lbfgs iter=10000 save_iterations=0\
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_datafull.standata \
	output file=~/EpiProjects/CPUM/output/${ROOT}_optim_full.csv  \
	random seed=${RANDOM} 
~/repo/cmdstan/bin/stansummary ~/EpiProjects/CPUM/output/${ROOT}_optim_full.csv
#cilc_nc[73]    .20
#after removing cohort_1    .21
#after removing cohort_1, cal time:  0.21
#after removing cohort_1, cal time, cuml^2: .24  
#after removing cuml, cuml^2, lhatlag2:   0.19
#after removing cumx: .19
#remove time-fixed: .19
#remove time-fixed, cal time: 0.19
#after fixing centering issue: 0.17
#after fixing employment status post 1977: 0.18
#giving baseline, exposure, work time more polynomial terms: 0.21 (lp = -40642)
#
#time only: 0.16



###variational inference - bayesian posterior approximation
./$ROOT \
    method=variational \
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata \
	output file=~/EpiProjects/CPUM/output/${ROOT}_variational.csv  \
	random seed=${RANDOM}  
~/repo/cmdstan/bin/stansummary ~/EpiProjects/CPUM/output/${ROOT}_variational.csv




#MCMC sampling
export i=1
#run model
#for i in {1..1}
#do
./$ROOT \
    method=sample num_samples=5000 num_warmup=10000 save_warmup=1 \
	id=$i \
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata \
	output file=~/EpiProjects/CPUM/output/${ROOT}_samples${i}.csv  \
	random seed=${RANDOM}  
#done
	
echo "end of chains"
#summarize all variables
~/repo/cmdstan/bin/stansummary ~/EpiProjects/CPUM/output/${ROOT}_samples*.csv
 