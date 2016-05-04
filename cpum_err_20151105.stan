// standard g-formula model for the natural course for the CPUM data, could use a bit of tweaking, but looks ok
data {
    //integer values for looping
    int<lower=0> N;
    int<lower=0> J;
    int<lower=0> minT;
    int<lower=0> maxT;
    int<lower=0> obscomplete;
    
    
    //covariates in observed data (for modeling)
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
    row_vector[obscomplete] BL_cumwlm;    // cumulative radon exposure accrued before follow-up started
    row_vector[obscomplete] BL_cumyrsexp; // cumulative employed time accrued before follow-up started
    row_vector[obscomplete] cumyrsexp;    // cumulative employed time accrued after follow-up started
    
    
    //outcomes, l variables
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
    


    //standardizing vars
    real meanageout;
    real sdageout;
    real meandateout;
    real sddateout;
    real meanBL_cumwlm;
    real meanBL_cumyrsexp;
    real meancumyrsexp;  
    real meancumwlm2lag;
    real sdBL_cumwlm;
    real sdBL_cumyrsexp;
    real sdcumyrsexp;  
    real sdcumwlm2lag;
    real maxX;
    

    meanageout <- mean(ageout);
    meandateout <- mean(dateout);
    meanBL_cumwlm <- mean(BL_cumwlm);
    meanBL_cumyrsexp <- mean(BL_cumyrsexp);
    meancumyrsexp <- mean(cumyrsexp);
    meancumwlm2lag <- mean(cumwlm2lag);
    sdageout <- sd(ageout);
    sddateout <- sd(dateout);
    sdBL_cumwlm <- sd(BL_cumwlm);
    sdBL_cumyrsexp <- sd(BL_cumyrsexp);
    sdcumyrsexp <- sd(cumyrsexp);
    sdcumwlm2lag <- sd(cumwlm2lag);
    

    
    //centering an standardizing these variables makes a HUGE difference in the optimization speed
    for(n in 1:obscomplete){
        py[n] <- ageout[n]-agein[n] + 0.0004491;
        ageoutcen[n] <- (ageout[n] - meanageout) /sdageout;
        dateoutcen[n] <- (dateout[n] - meandateout) /sddateout;
        BL_cumwlmcen[n] <- (BL_cumwlm[n] - meanBL_cumwlm) /sdBL_cumwlm;
        BL_cumyrsexpcen[n] <- (BL_cumyrsexp[n] - meanBL_cumyrsexp) /sdBL_cumyrsexp;
        cumyrsexpcen[n] <- (cumyrsexp[n] - meancumyrsexp) /sdcumyrsexp;
        cumwlm2lagcen[n] <- (cumwlm2lag[n] - meancumwlm2lag)/sdcumwlm2lag;
    }
    
    maxX <- max(cumwlm2lag);
}


parameters{
    real a0;
    real<lower=-1/maxX> err;
//    real lnrr;
//    real a[1];  // loglinear term 1
//    real d[9];  // loglinear term 2
}
transformed parameters{
}
model{
//      a0 ~ normal(0, 5); // uniform prior if commented out
//      d0 ~ normal(0, 100); // uniform prior if commented out
//      lnrr ~ normal(0, 20);
        err ~ normal(-1/maxX, 10) T[-1/maxX, ];
//        err ~ chi_square(2);
        
        a0 ~ normal(0, 10);
        { 
            row_vector[obscomplete] mu;
            for(n in 1:obscomplete){
                mu[n] <-  py[n]/10000 * exp(a0) * (1 + err*cumwlm2lag[n]/100);   // linear ERR model
        }
        d_lc ~ poisson(mu);
}

}