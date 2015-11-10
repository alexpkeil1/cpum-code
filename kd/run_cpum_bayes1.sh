#!/bin/bash -v
#script to run stan via command line (CmdStan)

export ROOT=cpum_bayesgf_20151029

#compile model code
cd ~/repo/cmdstan/  #killdevil, softlinked to /Applications/cmdstan on local
make ~/EpiProjects/CPUM/code/$ROOT
cd ~/EpiProjects/CPUM/code/

./$ROOT \
    method=sample num_samples=10000 num_warmup=2000 save_warmup=0 \
    data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata \
	output file=/lustre/scr/a/k/akeil/bgf/measurement/${ROOT}_samples${RANDOM}.csv  \
	random seed=11232 
#done
	
echo "end of chains"
#summarize all variables
~/repo/cmdstan/bin/stansummary ~/EpiProjects/CPUM/output/${ROOT}_samples*.csv
