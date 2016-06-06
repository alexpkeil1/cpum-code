#!/bin/bash

export ROOT=cpum_err_20151105
#compile model code
cd ~/EpiProjects/CPUM/code/
mv models/$ROOT . 
cd ~/repo/cmdstan/  #killdevil, softlinked to ~/repo/cmdstan on local
make -j 4 ~/EpiProjects/CPUM/code/$ROOT 
cd ~/EpiProjects/CPUM/code/
mv $ROOT models/
rm $ROOT.o $ROOT.d $ROOT.hpp $ROOT.cpp #clean up
cd models

if [ -n "${1}" ]; then
 echo "Running with arguments $1, $2, $3"
 export ARG1="$1" # which program
 export ARG2=$2   # number of total samples
 export ARG3=$3   # number of warmup samples
else
 export ARG1='optimize'
 export ARG2=2000
fi

#Bayesian
if [ $ARG1 = 'bayes' ]; then
    echo "Running Bayesian inference with Stan"
    for i in {1..5}
    do
    ./$ROOT \
        method=sample num_samples=$ARG2 num_warmup=$ARG3 save_warmup=0\
        data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata \
        output file=~/EpiProjects/CPUM/output/${ROOT}_sample${i}.csv \
        random seed=${RANDOM} &
    done
    wait
    ~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_sample*.csv
    ~/Documents/programming_examples/python/summarize_stan.py "~/EpiProjects/CPUM/output/${ROOT}_sample*.csv" 4 err
    # seems to diverge and take a long time without setting priors on the err
    #full data err = .5949
    #err, NA 1.22
elif [ $ARG1 = 'variational' ]; then
    echo "Running variational inference with Stan"
    #Variational
    ./$ROOT \
        method=variational iter=$ARG2 elbo_samples=$ARG3 grad_samples=3\
        data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata \
        output file=~/EpiProjects/CPUM/output/${ROOT}_variational.csv \
        random seed=${RANDOM} 
    ~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_variational.csv
    ~/Documents/programming_examples/python/summarize_stan.py ~/EpiProjects/CPUM/output/${ROOT}_variational.csv
    #err, varies by seed
elif [ $ARG1 = 'optimize' ]; then
    echo "Running optimize with Stan"
    ./$ROOT \
        method=optimize iter=$ARG2 \
        data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_data.standata output file=~/EpiProjects/CPUM/output/${ROOT}_optim.csv \
        random seed=${RANDOM}
    echo "Done with optimization"
    ~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_optim.csv
    ~/Documents/programming_examples/python/summarize_stan.py ~/EpiProjects/CPUM/output/${ROOT}_optim.csv
    #crude model
    #full data err = .5949, sas MCMC gives 0.5903, sas NLMIXED gives 0.5877, gnm (R) gives 0.5877
    #err, NA = 0.8513, sas MCMC gives , sas NLMIXED =  gives 0.8471
elif [ $ARG1 = 'bayes_full' ]; then
    echo "Running Bayesian inference with Stan (full data)"
    for i in {1..5}
    do
    ./$ROOT \
        method=sample num_samples=$ARG2 num_warmup=$ARG3 save_warmup=0\
        data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_datafull.standata \
        output file=~/EpiProjects/CPUM/output/${ROOT}_sample_full${i}.csv \
        random seed=${RANDOM} 
    done
    wait
    ~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_sample_full*.csv
   ~/Documents/programming_examples/python/summarize_stan.py '~/EpiProjects/CPUM/output/${ROOT}_sample_full*.csv' 5
    # seems to diverge and take a long time without setting priors on the err
    #full data err = .5949
    #err, NA 1.22
elif [ $ARG1 = 'variational_full' ]; then
    echo "Running variational inference with Stan (full data)"
    #Variational
    ./$ROOT \
        method=variational iter=$ARG2 elbo_samples=$ARG3 grad_samples=3\
        data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_datafull.standata \
        output file=~/EpiProjects/CPUM/output/${ROOT}_variational_full.csv \
        random seed=${RANDOM} 
    ~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_variational_full.csv
    ~/Documents/programming_examples/python/summarize_stan.py ~/EpiProjects/CPUM/output/${ROOT}_variational_full.csv
    #err, varies by seed
elif [ $ARG1 = 'optimize_full' ]; then
    echo "Running optimize with Stan (full data)"
    ./$ROOT \
        method=optimize iter=$ARG2 \
        data file=~/EpiProjects/CPUM/data/cpum_bayesgf_20151029_datafull.standata output file=~/EpiProjects/CPUM/output/${ROOT}_optim_full.csv \
        random seed=${RANDOM} 
    ~/repo/cmdstan/bin/stansummary --sig_figs=4 ~/EpiProjects/CPUM/output/${ROOT}_optim_full.csv
    ~/Documents/programming_examples/python/summarize_stan.py ~/EpiProjects/CPUM/output/${ROOT}_optim_full.csv
    #crude model
    #full data err = .5949, sas MCMC gives 0.5903, sas NLMIXED gives 0.5877, gnm (R) gives 0.5877
    #err, NA = 0.8513, sas MCMC gives , sas NLMIXED =  gives 0.8471
else
 echo "Argument 1 (if given) should be 'bayes', 'variational', or 'optimize'"
fi

