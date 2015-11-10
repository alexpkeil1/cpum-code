// todo: modeling leaving work
//       bring in baseline covariates
//       
//       
//       
//       
//       
data {
    //integer values for looping
    int<lower=0> N;
    int<lower=0> J;
    int<lower=0> minT;
    int<lower=0> maxT;
    int<lower=0> obscomplete;
    int<lower=0> obs;
    
    
    //covariates in observed data (for modeling)
    row_vector[obscomplete] wlm;
    row_vector[obscomplete] cumwlm;
    row_vector[obscomplete] cumwlm2lag;
    row_vector[obscomplete] atwork2lag;
	row_vector[obscomplete] ageout;
    row_vector[obscomplete] cohort_1;
    row_vector[obscomplete] BL_cumwlm;
    row_vector[obscomplete] BL_cumatwork;
    row_vector[obscomplete] cumatwork;
    
    
    //outcomes, l variables
    int<lower=0, upper=1> atwork[obscomplete];
    int<lower=0, upper=1> leftwork[obscomplete];
    int<lower=0, upper=1> d_lc[obscomplete];
    int<lower=0, upper=1> d_nonlc[obscomplete];
    
    
    // variables that are given for data from entry up to age at potential censoring (for inference)
    // see 'generated quantities' section for more details
    row_vector[obs] id_full;
    row_vector[obs] cohort_1_full;
    row_vector[obs] time_full;
    row_vector[obs] smoke3_2_full;
    row_vector[obs] minage_full;
    row_vector[obs] maxage_full;
    row_vector[obs] BL_cumwlm_full;
    row_vector[obs] BL_cumatwork_full;

}
transformed data{
    row_vector[obscomplete] logwlm;
    row_vector[obscomplete] ageoutcen;

    
    row_vector[obscomplete] BL_cumwlmcen;
    row_vector[obscomplete] BL_cumatworkcen;
    row_vector[obscomplete] cumatworkcen;
    row_vector[obscomplete] cumwlm2lagcen;
    
    

    row_vector[obs] time_fullcen;
    row_vector[obs] BL_cumwlm_fullcen;
    row_vector[obs] BL_cumatwork_fullcen;
	
	//standardizing vars
	real meanageout;
	real sdageout;
	real meanBL_cumwlm;
	real meanBL_cumatwork;
	real meancumatwork;  
	real meancumwlm2lag;
	real sdBL_cumwlm;
	real sdBL_cumatwork;
	real sdcumatwork;  
	real sdcumwlm2lag;

    meanageout <- mean(ageout);
    meanBL_cumwlm <- mean(BL_cumwlm);
    meanBL_cumatwork <- mean(BL_cumatwork);
    meancumatwork <- mean(cumatwork);
    meancumwlm2lag <- mean(cumwlm2lag);
    sdageout <- sd(ageout);
    sdBL_cumwlm <- sd(BL_cumwlm);
    sdBL_cumatwork <- sd(BL_cumatwork);
    sdcumatwork <- sd(cumatwork);
    sdcumwlm2lag <- sd(cumwlm2lag);
    

    
    //centering an standardizing these variables makes a HUGE difference in the optimization speed
    for(n in 1:obscomplete){
        if(wlm[n]>0) logwlm[n] <- log(wlm[n]);
        else logwlm[n] <- 0.01;
        ageoutcen[n] <- (ageout[n] - meanageout) /sdageout;
        BL_cumwlmcen[n] <- (BL_cumwlm[n] - meanBL_cumwlm) /sdBL_cumwlm;
        BL_cumatworkcen[n] <- (BL_cumatwork[n] - meanBL_cumatwork) /sdBL_cumatwork;
        cumatworkcen[n] <- (cumatwork[n] - meancumatwork) /sdcumatwork;
        cumwlm2lagcen[n] <- (cumwlm2lag[n] - meancumwlm2lag)/sdcumwlm2lag;
        
    }
    
    for(c in 1:obs){
        time_fullcen[c] <- (time_full[c]-meanageout)/sdageout;
        BL_cumwlm_fullcen[c]    <- (BL_cumwlm_full[c] - meanBL_cumwlm)/sdBL_cumwlm;
        BL_cumatwork_fullcen[c] <- (BL_cumatwork_full[c] - meanBL_cumatwork)/sdBL_cumatwork;
    }
    
}


parameters{
    real a0;
    real b0;
    real c0;
    real g0;
    real<lower=0> sigma2;
    real a[2];
    real b[2];
    real c[2];
    real g[2];
}
transformed parameters{
}
model{
//      a0 ~ normal(0, 100); // uniform prior
//      b0 ~ normal(0, 100); // uniform prior
//      c0 ~ normal(0, 100); // uniform prior
//      g0 ~ normal(0, 100); // uniform prior
    a ~ normal(0, 11);
    b ~ normal(0, 11);
    c ~ normal(0, 11);
    g ~ normal(0, 11);
    sigma2 ~ inv_gamma(0.1, 0.1); //scale parameter is switched in stan vs. sas (stan scale parameter = iscale parameter in sas)
    for (n in 1:obscomplete){
        if(atwork[n]==1 && wlm[n]>0){
        // convergence is much slower when including this piece, helps to transform wlm by multiplication
        log(wlm[n]/1000) ~ normal(
            a0 + a[1]*ageoutcen[n]
            , sigma2);
        } else increment_log_prob(0);
        
        if(atwork[n]==1 || leftwork[n]==1){
        leftwork[n] ~ bernoulli_logit(
            g0 + g[1]*ageoutcen[n]  
        );
        } else increment_log_prob(0);
        }//n
        
        //atwork ~ bernoulli_logit(
        //    a0 + a[1]*ageoutcen  + a[2]*ageoutcen .* ageoutcen
        //);

        d_lc ~ bernoulli_logit(
            b0 + b[1]*ageoutcen  + b[2]*ageoutcen .* ageoutcen
        );
        d_nonlc ~ bernoulli_logit(
            c0 + c[1]*ageoutcen  + c[2]*ageoutcen .* ageoutcen
        );


}
generated quantities{
// This section is for sampling from the posterior predictive distribution - 
// person time works differently in this section from the data.
// Late entry is maintained, but individuals are kept in the data until
// censoring (by end of study or reaching the max age). 
// Death is assigned as a probability, rather than a binary variable 
// so an individual can be kept in the dataset since they represent
// the discrete hazard of dying, rather than representing a specific individual
// Thus, the total person time in this section is expanded beyond the original data
// and some variables are given different names to correspond to each variable
// over the expanded person-time. For example, cohort_1 becomes cohort_1_full
// because they are just vectors of different lengths that correspond to the
// total person-time vector of the cohort_1 variable in the observed/posterior data.
// Not all of these name switches are intuitive (cumwlm becomes cumx), but I will clean
// them up as I have time to work over the code.

    //summary functions
    real surv_nc[J];
    real cilc_nc[J];
    real cinlc_nc[J];
    real meanCumX;
    real meanWkyrs;
    real y_nc[J];
    real ylc[J];
    real ynlc[J];

{ // local scope
    real meanX[J];
    int R[J]; //count of risk set

    int lswitch;
    
    real xhat[obs]; //exposure
    real lhat[obs]; //employment status
    real leftworkf[obs]; //employment status
    real h_lcn[obs];
    real h_nlcn[obs];
    //
    real cumx[obs];
    real cuml[obs];
    real cumx5lag[obs];
    real lhat5lag[obs];
    real lhat2lag[obs];
    
    //initilialize lagged, cumulative variables
    meanCumX <- 0;
    meanWkyrs <- 0;
    for(n in 1:obs){
        cumx[n] <- 0; // cumulative exposure
        cuml[n] <- 0;  // cumulative time at work
        cumx5lag[n] <- 0;
        lhat5lag[n] <- 0;
        lhat2lag[n] <- 0;
    }
    //initialize potential average rates, mean exposure
    for(k in 1:J){
    	R[k] <- 0;
        ylc[k] <- 0;
        ynlc[k] <- 0;
        meanX[k] <- 0;
    }
   //note: this can be done more efficiently in terms of looping, but it will save on drive space
   //using likelihood to generate potential expected covariate, outcomes
    for (n in 1:obs){
        ////////
        //employment status
        ////////
        //work status treated like a binary switch that starts on the 'on' position and can only be
        // turned off once
    	if(n==1 || id_full[n] != id_full[n-1]){
    	  lhat[n] <- 1;
    	  lswitch <- 1;
    	}
    	if(lswitch==1){
        leftworkf[n] <- bernoulli_rng(inv_logit(
            g0 + g[1]*time_fullcen[n] 
        ));
        if(leftworkf[n]==1){
        	lhat[n] <- 0;
        	lswitch <- 0;
        }
        } else {
            leftworkf[n] <- 0;
            lhat[n] <- 0;
        }

        ////////
        //exposure
        ////////
        if(lhat[n]==1){
            xhat[n] <- 1000*exp(normal_rng(
            a0 + a[1]*time_fullcen[n]
            , sigma2
        ));
        }
        else if(lhat[n]==0) xhat[n] <- 0;
    
    //cumulative, lag variables
    if(n>1 && id_full[n]==id_full[n-1]){
        cumx[n] <- cumx[n-1] + xhat[n];
        cuml[n] <- cuml[n-1] + lhat[n];
    }
    if(n>5 && id_full[n]==id_full[n-5]){
        cumx5lag[n] <- cumx[n-5];
        lhat5lag[n] <- lhat[n-5];
    }
    if(n>2 && id_full[n]==id_full[n-2]){
        lhat2lag[n] <- lhat[n-2];
    }
    ////////
    //lung cancer mortality
    ////////

    h_lcn[n] <- inv_logit(
        b0 + b[1]*time_fullcen[n]  + b[2]*time_fullcen[n]*time_fullcen[n]
    );
    ////////
    //all other cause mortality
    ////////
    h_nlcn[n] <- inv_logit(
        c0 + c[1]*time_fullcen[n]  + c[2]*time_fullcen[n]*time_fullcen[n]
    );
    
    
    //natural course
    for (k in 1:J){
    //loop over time of interest for inference (will essentially drop observations if they occur outside of this window)
        if(time_full[n]==(k+minT-1)){
            //risk set size
            R[k] <- R[k] + 1;
            //these will be averaged below
            ylc[k] <- ylc[k] + h_lcn[n];
            ynlc[k] <- ynlc[k] + h_nlcn[n];
            meanX[k] <- meanX[k] + xhat[n];
    	} //time_full
    } // k in 1:J
    if(time_full[n]==maxT || time_full[n]==maxage_full[n]) {
     	meanCumX <- meanCumX + cumx[n]/N;
     }
    } // n in 1:obs
    for (k in 1:J){
        //take average over risk set (average rate)
        ylc[k] <- ylc[k]/R[k];
        ynlc[k] <- ynlc[k]/R[k];
        meanX[k] <- meanX[k]/R[k];
        y_nc[k] <- ylc[k] + ynlc[k];
    }
    // calculate survival, survival difference, risk difference under interventions
    surv_nc[1]    <- 1 - (ylc[1] + ynlc[1]);
    cilc_nc[1]  <- ylc[1];
    cinlc_nc[1] <-  ynlc[1];
    for (k in 2:J){
        surv_nc[k]  <-                   (1-(ylc[k] + ynlc[k]))*(surv_nc[k-1]);
        cilc_nc[k]  <-   cilc_nc[k-1]  + (   ylc[k]           )*(surv_nc[k-1]); 
        cinlc_nc[k] <-   cinlc_nc[k-1] + (            ynlc[k] )*(surv_nc[k-1]); 
    }
}// end local scope

}