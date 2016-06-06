// standard g-formula model for the natural course for the CPUM data, could use a bit of tweaking, but looks ok
// updated 6/3/16 with stan language tweaks
data {
    // integer values for looping
    int<lower=0> N;
    int<lower=0> J;
    int<lower=0> minT;
    int<lower=0> maxT;
    int<lower=0> obscomplete;
    
    
    // covariates in observed data (for modeling)
    row_vector[obscomplete] id;
    row_vector[obscomplete] wlm;          // annual radon exposure in WLM
    row_vector[obscomplete] cumwlm;       // cumulative radon in WLM after follow-up starts (i.e. does not include pre-follow-up exposure)
    row_vector[obscomplete] cumwlm2lag;   // cum radon after follow-up, 2 year lag
    row_vector[obscomplete] atwork;       // binary indicator of current employment (1=yes, 0=no)
    row_vector[obscomplete] atwork2lag;   // work status, lagged 2 years (only during follow-up)
    row_vector[obscomplete] agein;        // age at beginning of person period
    row_vector[obscomplete] ageout;       // age at end of person period
    row_vector[obscomplete] dateout;      // calendar year at end of person period
    row_vector[obscomplete] cohort_1;     // birth cohort
    row_vector[obscomplete] cumyrsexp;    // cumulative employed time accrued after follow-up started
    
    
    // outcomes, l variables
    int<lower=0, upper=1> leftwork[obscomplete];
    int<lower=0, upper=1> d_lc[obscomplete];
    int<lower=0, upper=1> d_nonlc[obscomplete];
    
    }
transformed data{
    row_vector[obscomplete] ageoutcen;
    row_vector[obscomplete] dateoutcen;
    row_vector[obscomplete] py;

    
    row_vector[obscomplete] BL_cumwlmcen;
    row_vector[obscomplete] BL_cumyrsexpcen;
    row_vector[obscomplete] cumyrsexpcen;
    row_vector[obscomplete] cumwlm2lagcen;
    


    // standardizing vars
    real meanageout;
    real sdageout;
    real meandateout;
    real sddateout;
    real meancumyrsexp;  
    real meancumwlm2lag;
    real sdcumyrsexp;  
    real sdcumwlm2lag;
    real maxX;
    
    // indicator variables (from beir VII)
    // time since exposure (named by upper bound of category, or 100 for highest)
    // row_vector[obscomplete] tse_cont;
    // int<lower=0, upper=1> tse14[obscomplete];
    // int<lower=0, upper=1> tse24[obscomplete];
    // int<lower=0, upper=1> tse100[obscomplete];
    //// age categories  (ageout)
    // int<lower=0, upper=1> age55[obscomplete];
    // int<lower=0, upper=1> age64[obscomplete];
    // int<lower=0, upper=1> age74[obscomplete];
    // int<lower=0, upper=1> age100[obscomplete];
    //// duration of exposure (cumyrsexp)
    // int<lower=0, upper=1> dur5[obscomplete];
    // int<lower=0, upper=1> dur14[obscomplete];
    // int<lower=0, upper=1> dur24[obscomplete];
    // int<lower=0, upper=1> dur34[obscomplete];
    // int<lower=0, upper=1> dur100[obscomplete];
    //// exposure rate
    // row_vector[obscomplete] rate_cont;
    // int<lower=0, upper=1> ratep5[obscomplete];
    // int<lower=0, upper=1> rate1[obscomplete];
    // int<lower=0, upper=1> rate3[obscomplete];
    // int<lower=0, upper=1> rate5[obscomplete];
    // int<lower=0, upper=1> rate15[obscomplete];
    // int<lower=0, upper=1> rate100[obscomplete];
    

    meanageout = mean(ageout);
    meandateout = mean(dateout);
    meancumyrsexp = mean(cumyrsexp);
    meancumwlm2lag = mean(cumwlm2lag);
    sdageout = sd(ageout);
    sddateout = sd(dateout);
    sdcumyrsexp = sd(cumyrsexp);
    sdcumwlm2lag = sd(cumwlm2lag);
    

    
    // centering an standardizing these variables makes a HUGE difference in the optimization speed
    for(n in 1:obscomplete){
        py[n] = ageout[n]-agein[n] + 0.0004491;
        ageoutcen[n] = (ageout[n] - meanageout) /sdageout;
        dateoutcen[n] = (dateout[n] - meandateout) /sddateout;
        cumyrsexpcen[n] = (cumyrsexp[n] - meancumyrsexp) /sdcumyrsexp;
        cumwlm2lagcen[n] = (cumwlm2lag[n] - meancumwlm2lag)/sdcumwlm2lag;
    }
    
    // categorical variables (beir VII)
    // for(n in 1:obscomplete){
    //     tse_cont[n] = 1.0;  // need to fix this
    //     rate_cont[n] = cumwlm[n]/cumyrsexp[n];
    // }
    
    
    maxX = max(cumwlm2lag);
}


parameters{
    real<lower=-1/maxX> err;
    real a0;
    // real a[3];  // loglinear term 1
}
transformed parameters{
}
model{
        //err ~ normal(-1/maxX, 10) T[-1/maxX, ];        
        err ~ cauchy(0, 5) T[-1/maxX, ];        
        //a0 ~ normal(0, 10);
        // a ~ normal(0, 10);
        { // locenv(1): local variables only in here
            row_vector[obscomplete] mu;
            row_vector[obscomplete] ll1;
            for(n in 1:obscomplete){
                // ll1[n] = a0 + a[1]*ageoutcen[n] + a[2]*ageoutcen[n]*ageoutcen[n] + a[3]*ageoutcen[n]*ageoutcen[n]*ageoutcen[n];
                ll1[n] = a0;
                //mu[n] = py[n]/10000 * exp(ll1[n]) * (1 + err*cumwlm2lag[n]/100);   // linear ERR model
                mu[n] = log(py[n]/10000 * exp(ll1[n]) * (1 + err*cumwlm2lag[n]/100));   // linear ERR model modeled as log
            }
        //d_lc ~ poisson(mu);
        // https://stat.ethz.ch/pipermail/r-help/2014-July/420302.html
        d_lc ~ poisson_log(mu);
        }// end locenv(1)
}