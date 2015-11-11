// standard g-formula model for the natural course for the CPUM data, could use a bit of tweaking, but looks ok
data {
    //integer values for looping
    int<lower=0> N;
    int<lower=0> J;
    int<lower=0> minT;
    int<lower=0> maxT;
    int<lower=0> obscomplete;
    int<lower=0> obs;
    
    
    //covariates in observed data (for modeling)
    row_vector[obscomplete] id;
    row_vector[obscomplete] wlm;          // annual radon exposure in WLM
    row_vector[obscomplete] cumwlm;       // cumulative radon in WLM after follow-up starts (i.e. does not include pre-follow-up exposure)
    row_vector[obscomplete] cumwlm2lag;   // cum radon after follow-up, 2 year lag
    row_vector[obscomplete] atwork;       // binary indicator of current employment (1=yes, 0=no)
    row_vector[obscomplete] atwork2lag;   // work status, lagged 2 years (only during follow-up)
    row_vector[obscomplete] ageout;       // age at end of person period
    row_vector[obscomplete] dateout;      // calendar year at end of person period
    row_vector[obscomplete] cohort_1;     // birth cohort
    row_vector[obscomplete] BL_cumwlm;    // cumulative radon exposure accrued before follow-up started
    row_vector[obscomplete] BL_cumyrsexp; // cumulative employed time accrued before follow-up started
    row_vector[obscomplete] cumyrsexp;    // cumulative employed time accrued after follow-up started
    row_vector[obscomplete] cumyrsexp2lag;    // cumulative employed time accrued after follow-up started lagged 2 years
    
    
    //outcomes, l variables
    int<lower=0, upper=1> leftwork[obscomplete];  // indicator that individual worked in previous person period, but not in current
    int<lower=0, upper=1> d_lc[obscomplete];      // lung cancer death in person period
    int<lower=0, upper=1> d_nonlc[obscomplete];   // non-lung cancer death in person period
    
    
    // variables that are given for data from entry up to age at potential censoring (for inference)
    // see 'generated quantities' section for more details
    row_vector[obs] id_full;
    row_vector[obs] cohort_1_full;
    row_vector[obs] age_full;
    row_vector[obs] date_full;
    row_vector[obs] smoke3_2_full;
    row_vector[obs] minage_full;
    row_vector[obs] maxage_full;
    row_vector[obs] BL_cumwlm_full;
    row_vector[obs] BL_cumyrsexp_full;

}
transformed data{
    row_vector[obscomplete] ageoutcen;
    row_vector[obscomplete] dateoutcen;

    
    row_vector[obscomplete] BL_cumwlmcen;
    row_vector[obscomplete] BL_cumyrsexpcen;
    row_vector[obscomplete] cumyrsexpcen;
    row_vector[obscomplete] cumyrsexp2lagcen;
    row_vector[obscomplete] cumwlm2lagcen;
    

    row_vector[obs] age_fullcen;
    row_vector[obs] date_fullcen;
    row_vector[obs] BL_cumwlm_fullcen;
    row_vector[obs] BL_cumyrsexp_fullcen;

    //standardizing vars
    real meanageout;
    real sdageout;
    real meandateout;
    real sddateout;
    real meanBL_cumwlm;
    real meanBL_cumyrsexp;
    real meancumyrsexp;  
    real meancumyrsexp2lag;  
    real meancumwlm2lag;
    real sdBL_cumwlm;
    real sdBL_cumyrsexp;
    real sdcumyrsexp;  
    real sdcumyrsexp2lag;  
    real sdcumwlm2lag;
    
    
    //exposure after baseline

    meanageout <- mean(ageout);
    meandateout <- mean(dateout);
    meanBL_cumwlm <- mean(BL_cumwlm);
    meanBL_cumyrsexp <- mean(BL_cumyrsexp);
    meancumyrsexp <- mean(cumyrsexp);
    meancumyrsexp2lag <- mean(cumyrsexp2lag);
    meancumwlm2lag <- mean(cumwlm2lag);
    sdageout <- sd(ageout);
    sddateout <- sd(dateout);
    sdBL_cumwlm <- sd(BL_cumwlm);
    sdBL_cumyrsexp <- sd(BL_cumyrsexp);
    sdcumyrsexp <- sd(cumyrsexp);
    sdcumyrsexp2lag <- sd(cumyrsexp2lag);
    sdcumwlm2lag <- sd(cumwlm2lag);
    


    
   
    
    //centering an standardizing these variables makes a HUGE difference in the optimization speed
    for(n in 1:obscomplete){
        cumyrsexpcen[n] <- (cumyrsexp[n] - meancumyrsexp) /sdcumyrsexp;
        cumyrsexp2lagcen[n] <- (cumyrsexp2lag[n] - meancumyrsexp2lag) /sdcumyrsexp2lag;
        cumwlm2lagcen[n] <- (cumwlm2lag[n] - meancumwlm2lag)/sdcumwlm2lag;

        ageoutcen[n] <- (ageout[n] - meanageout) /sdageout;
        dateoutcen[n] <- (dateout[n] - meandateout) /sddateout;
        BL_cumwlmcen[n] <- (BL_cumwlm[n] - meanBL_cumwlm) /sdBL_cumwlm;
        BL_cumyrsexpcen[n] <- (BL_cumyrsexp[n] - meanBL_cumyrsexp) /sdBL_cumyrsexp;

    }
    
    for(c in 1:obs){
        age_fullcen[c] <- (age_full[c]-meanageout)/sdageout;
        date_fullcen[c] <- (date_full[c]-meandateout)/sddateout;
        BL_cumwlm_fullcen[c]    <- (BL_cumwlm_full[c] - meanBL_cumwlm)/sdBL_cumwlm;
        BL_cumyrsexp_fullcen[c] <- (BL_cumyrsexp_full[c] - meanBL_cumyrsexp)/sdBL_cumyrsexp;
    }
    
            
}


parameters{
    real a0;
    real b0;
    real c0;
    real g0;
    real<lower=0> sigma2;
    real a[13];  // exposure
    real g[11];  // employment
    real b[14]; // lung cancer
    real c[14]; // all other causes
}
transformed parameters{
}
model{
//      a0 ~ normal(0, 100); // uniform prior if commented out
//      b0 ~ normal(0, 100); // uniform prior if commented out
//      c0 ~ normal(0, 100); // uniform prior if commented out
//      g0 ~ normal(0, 100); // uniform prior if commented out
    a ~ normal(0, 11);
    b ~ normal(0, 11);
    c ~ normal(0, 11);
    g ~ normal(0, 11);
    sigma2 ~ inv_gamma(0.1, 0.1); //scale parameter is switched in stan vs. sas (stan scale parameter = iscale parameter in sas)
    for (n in 1:obscomplete){

        ////////
        //employment status model
        ////////
        
        if((atwork[n]==1 || leftwork[n]==1) && dateout[n] < 1977.5){
        leftwork[n] ~ bernoulli_logit(
g0 +
g[1]*BL_cumwlmcen[n] +
g[2]*BL_cumyrsexpcen[n] +
g[3]*cohort_1[n] +
g[4]*cumyrsexp2lagcen[n] +
g[5]*cumyrsexp2lagcen[n]*cumyrsexp2lagcen[n] +
g[6]*cumwlm2lagcen[n] +
g[7]*cumwlm2lagcen[n]*cumwlm2lagcen[n] +
g[8]*ageoutcen[n] +
g[9]*ageoutcen[n]*ageoutcen[n]  +
g[10]*dateoutcen[n] +
g[11]*dateoutcen[n]*dateoutcen[n]
       );
        } else increment_log_prob(0);

        ////////
        // exposure model
        ////////
        if(atwork[n]==1 && wlm[n]>0){
        log(wlm[n]/1000) ~ normal(
a0 +
a[1]*BL_cumwlmcen[n] +
a[2]*BL_cumyrsexpcen[n] +
a[3]*cohort_1[n] +
a[4]*cumyrsexpcen[n] +
a[5]*cumyrsexpcen[n]*cumyrsexpcen[n] +
a[6]*cumyrsexpcen[n]*cumyrsexpcen[n]*cumyrsexpcen[n] +
a[7]*cumwlm2lagcen[n] +
a[8]*cumwlm2lagcen[n]*cumwlm2lagcen[n] +
a[9]*cumwlm2lagcen[n]*cumwlm2lagcen[n]*cumwlm2lagcen[n] +
a[10]*ageoutcen[n] +
a[11]*ageoutcen[n]*ageoutcen[n] +
a[12]*dateoutcen[n] +
a[13]*dateoutcen[n]*dateoutcen[n]
            , sigma2
            );
        } else increment_log_prob(0);

        }//n

        

        ////////
        //lung cancer mortality model
        ////////
        d_lc ~ bernoulli_logit(
b0 +
b[1]*BL_cumwlmcen +
b[2]*BL_cumyrsexpcen +
b[3]*cohort_1 +
b[4]*cumyrsexpcen +
b[11]* cumyrsexpcen .* cumyrsexpcen +
b[5]*atwork2lag +
b[6]*cumwlm2lagcen +
b[7]*ageoutcen +
b[8]*ageoutcen .* ageoutcen +
b[9]*ageoutcen .* ageoutcen .* ageoutcen +
b[10]*ageoutcen .* ageoutcen .* ageoutcen .* ageoutcen +
b[12]*dateoutcen +
b[13]*dateoutcen .* dateoutcen +
b[14]*dateoutcen .* dateoutcen .* dateoutcen
        );
        ////////
        // all other cause mortality model
        ////////
        d_nonlc ~ bernoulli_logit(
c0 +
c[1]*BL_cumwlmcen +
c[2]*BL_cumyrsexpcen +
c[3]*cohort_1 +
c[4]*cumyrsexpcen +
c[11]* cumyrsexpcen .* cumyrsexpcen +
c[5]*atwork2lag +
c[6]*cumwlm2lagcen +
c[7]*ageoutcen +
c[8]*ageoutcen .* ageoutcen +
c[9]*ageoutcen .* ageoutcen .* ageoutcen  +
c[10]*ageoutcen .* ageoutcen .* ageoutcen .* ageoutcen +
c[12]*dateoutcen +
c[13]*dateoutcen .* dateoutcen +
c[14]*dateoutcen .* dateoutcen .* dateoutcen
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
    real cinlc_nc[J];
    real cilc_nc[J];
    real meanCumX[4];
	
    real meanWkyrs[4];

{ // local scope
    int R[J]; //count of risk set
    real meanX[J]; // mean annual exposure
    real surv_2[J];
    real cilc_2[J];
    real cinlc_2[J];
    real surv_1[J];
    real cilc_1[J];
    real cinlc_1[J];
    real surv_33[J];
    real cilc_33[J];
    real cinlc_33[J];

    real y_ac[J];
    real ylc[J];
    real ynlc[J];
    
    real xhat[obs]; //exposure
    real lhat[obs]; //employment status
    real h_lcn[obs];
    real h_nlcn[obs];
    //
    real cumx[obs];
    real cuml[obs];
    real cumlcen[obs];
    real cumx2lag[obs];
    real cumx2lagcen[obs];
    real cuml2lag[obs];
    real cuml2lagcen[obs];
    real cumx5lag[obs];
    real lhat5lag[obs];
    real lhat2lag[obs];
    
    
    ///// variables from the original data + full data counterparts: ////
    // cumwlmcen = cumxcen
    // cumwlm2lagcen = cumx2lagcen
    // ageout = age_full
    // ageoutcen = age_fullcen
    // cohort_1 = cohort_1_full
    // cumyrsexpcen = cumlcen
    // BL_cumyrsexp = BL_cumyrsexpfull
    // BL_cumwlmcen = BL_cumwlm_full
    // BL_cumyrsexpcen = BL_cumyrsexpfullcen
    // BL_cumwlmcencen = BL_cumwlm_fullcen
    // atwork = lhat
    // wlm = xhat
    /////////////////////////////////////////////////////////////////////
    //initilialize lagged, cumulative variables
    
for(interv in 1:4){    
    //intervention loop:
    // 1 = 2 wlm/m limit (24/yr)
    // 2 = 1 wlm/m limit (12/yr)
    // 3 = 0.33 wlm/m limit (4/yr)
    // 4 = natural course
    meanCumX[interv] <- 0;
    meanWkyrs[interv] <- 0;
    
    //initialize time varying exposure/covariates
    for(n in 1:obs){
        lhat[n] <- 0;
        xhat[n] <- 0;
        cumx[n] <- 0; // cumulative exposure
        cuml[n] <- 0;  // cumulative time at work
    }
    //initialize potential average rates, mean exposure
    for(k in 1:J){
    	R[k] <- 0;
        ylc[k] <- 0;
        ynlc[k] <- 0;
        meanX[k] <- 0;
    }
   //////////////////////// 
   // main loop - looping over person-periods
   //////////////////////// 
   //using likelihood to generate potential expected covariate, outcomes
    for (n in 1:obs){
    
		// lagged variables
		if(n>5 && id_full[n]==id_full[n-5]){ //calculate cumulative values if id has at least 5 prior time points
			cumx5lag[n] <- cumx[n-5];
			lhat5lag[n] <- lhat[n-5];
		}
		else{
			cumx5lag[n] <- 0;
			lhat5lag[n] <- 0;
		}
		if(n>2 && id_full[n]==id_full[n-2]){ //calculate cumulative values if id has at least 2 prior time points
			lhat2lag[n] <- lhat[n-2];
			cumx2lag[n] <- cumx[n-2];
			cumx2lagcen[n] <- (cumx2lag[n]-meancumwlm2lag)/sdcumwlm2lag;
			cuml2lag[n] <- cuml[n-2];
			cuml2lagcen[n] <- (cuml2lag[n]-meancumyrsexp2lag)/sdcumyrsexp2lag;
		}
		else{
			lhat2lag[n] <- 0;
			cumx2lag[n] <- 0;
    	    cumx2lagcen[n] <- (cumx2lag[n]-meancumwlm2lag)/sdcumwlm2lag; // the referent level for a centered variable
			cuml2lag[n] <- 0;
			cuml2lagcen[n] <- (cuml2lag[n]-meancumyrsexp2lag)/sdcumyrsexp2lag;
		}

        ////////
        //employment status
        ////////
        // no returning to work once off work
    	if(n==1 || id_full[n] != id_full[n-1]) lhat[n] <- 1; //assume at work if in first time period
    	else if(n>1 && id_full[n] == id_full[n-1] && lhat[n-1] == 1  && date_full[n] < 1978){// can't actually leave first time point, can't be exposed after 1970
            // work[n] = 1-probability of leaving work
			lhat[n] <- 1-bernoulli_rng(inv_logit(
g0 +
g[1]*BL_cumwlm_fullcen[n] +
g[2]*BL_cumyrsexp_fullcen[n] +
g[3]*cohort_1_full[n] +
g[4]*cuml2lagcen[n] +
g[5]*cuml2lagcen[n]*cuml2lagcen[n] +
g[6]*cumx2lagcen[n] +
g[7]*cumx2lagcen[n]*cumx2lagcen[n] +
g[8]*age_fullcen[n] +
g[9]*age_fullcen[n]*age_fullcen[n] +
g[10]*date_fullcen[n] +
g[11]*date_fullcen[n]*date_fullcen[n]
			));
        }
        else lhat[n] <- 0;// no returning work
    
		//cumulative employment variables
		if(n==1 || id_full[n] != id_full[n-1]){ //first observation for each individual
			cuml[n] <- lhat[n];
			cumlcen[n] <- (cuml[n]-meancumyrsexp)/sdcumyrsexp;
		}
		else if(n>1 && id_full[n]==id_full[n-1]){ //calculate cumulative values if not first observation
			cuml[n] <- cuml[n-1] + lhat[n];
			cumlcen[n] <- (cuml[n]-meancumyrsexp)/sdcumyrsexp;
		}
        ////////
        //exposure
        ////////
        if(lhat[n]==1){
            xhat[n] <- 4500;
            rep <- 0;
            while(xhat[n]>4000){
            rep <- rep+1;
            if(rep>10) print "exposure model wacky";
            	xhat[n] <- 1000*exp(normal_rng(
a0 +
a[1]*BL_cumwlm_fullcen[n] +
a[2]*BL_cumyrsexp_fullcen[n] +
a[3]*cohort_1_full[n] +
a[4]*cumlcen[n] +
a[5]*cumlcen[n]*cumlcen[n] +
a[6]*cumlcen[n]*cumlcen[n]*cumlcen[n] +
a[7]*cumx2lagcen[n] + 
a[8]*cumx2lagcen[n]*cumx2lagcen[n] + 
a[9]*cumx2lagcen[n]*cumx2lagcen[n]*cumx2lagcen[n] + 
a[10]*age_fullcen[n] +
a[11]*age_fullcen[n]*age_fullcen[n] +
a[12]*date_fullcen[n] +
a[13]*date_fullcen[n]*date_fullcen[n]
         		, sigma2
        		)) ;
        	} //while loop for truncating exposures
        }
        else if(lhat[n]==0) xhat[n] <- 0;
        
        // intervention levels for exposures
        if(interv==1 && xhat[n]>24){
            xhat[n] <- 24;
        }
        if(interv==2 && xhat[n]>12){
            xhat[n] <- 12;
        }
        if(interv==3 && xhat[n]>4){
            xhat[n] <- 4;        
        }
    
		//cumulative exposure variables
		if(n==1 || id_full[n]!=id_full[n-1]){ //first observation for each individual
			cumx[n] <- xhat[n];
		}
		if(n>1 && id_full[n]==id_full[n-1]){ //calculate cumulative values if not first observation
			cumx[n] <- cumx[n-1] + xhat[n];
		}
		////////
		//lung cancer mortality
		////////

		h_lcn[n] <- inv_logit(
b0 +
b[1]*BL_cumwlm_fullcen[n] +
b[2]*BL_cumyrsexp_fullcen[n] +
b[3]*cohort_1_full[n] +
b[4]*cumlcen[n] + //ends up with a large contribution
b[11]*cumlcen[n]*cumlcen[n] +
b[5]*lhat2lag[n] +
b[6]*cumx2lagcen[n] +
b[7]*age_fullcen[n] +
b[8]*age_fullcen[n]*age_fullcen[n] +
b[9]*age_fullcen[n]*age_fullcen[n]*age_fullcen[n] +
b[10]*age_fullcen[n]*age_fullcen[n]*age_fullcen[n]*age_fullcen[n] +
b[12]*date_fullcen[n] +
b[13]*date_fullcen[n]*date_fullcen[n] +
b[14]*date_fullcen[n]*date_fullcen[n]*date_fullcen[n]
	   );
		////////
		//all other cause mortality
		////////
		h_nlcn[n] <- inv_logit(
c0 +
c[1]*BL_cumwlm_fullcen[n] +
c[2]*BL_cumyrsexp_fullcen[n] +
c[3]*cohort_1_full[n] +
c[4]*cumlcen[n] +
c[11]*cumlcen[n]*cumlcen[n] +
c[5]*lhat2lag[n] +
c[6]*cumx2lagcen[n] +
c[7]*age_fullcen[n] +
c[8]*age_fullcen[n]*age_fullcen[n] +
c[9]*age_fullcen[n]*age_fullcen[n]*age_fullcen[n] +
c[10]*age_fullcen[n]*age_fullcen[n]*age_fullcen[n]*age_fullcen[n] +
c[12]*date_fullcen[n] +
c[13]*date_fullcen[n]*date_fullcen[n] +
c[14]*date_fullcen[n]*date_fullcen[n]*date_fullcen[n]
		);
	
	
		for (k in 1:J){
		//loop over time of interest for inference (will essentially drop observations if they occur outside of this window)
			if(age_full[n]==(k+minT-1)){
				//risk set size
				R[k] <- R[k] + 1;
				//these will be averaged below
				ylc[k] <- ylc[k] + h_lcn[n];
				ynlc[k] <- ynlc[k] + h_nlcn[n];
				meanX[k] <- meanX[k] + xhat[n];
			} //age_full
		} // k in 1:J
		if(age_full[n]==maxT || age_full[n]==maxage_full[n]) {
			meanCumX[interv] <- meanCumX[interv] + cumx[n]/N;
			meanWkyrs[interv] <- meanWkyrs[interv] + cuml[n]/N;
		 }
    } // n in 1:obs (end main loop)

    for (k in 1:J){
        //take average over risk set (average rate)
        ylc[k] <- ylc[k]/R[k];
        ynlc[k] <- ynlc[k]/R[k];
        meanX[k] <- meanX[k]/R[k];
        y_ac[k] <- ylc[k] + ynlc[k];
    }

    
    //interventions
    
    if(interv==1){    
		// calculate survival, survival difference, risk difference under interventions
		surv_2[1]    <- 1 - (ylc[1] + ynlc[1]);
		cilc_2[1]  <- ylc[1];
		cinlc_2[1] <-  ynlc[1];
		for (k in 2:J){
			surv_2[k]  <-                  (1-(ylc[k] + ynlc[k]))*(surv_2[k-1]);
			cilc_2[k]  <-   cilc_2[k-1]  + (   ylc[k]           )*(surv_2[k-1]); 
			cinlc_2[k] <-   cinlc_2[k-1] + (            ynlc[k] )*(surv_2[k-1]); 
		}
    }// interv 1
    
    if(interv==2){    
		// calculate survival, survival difference, risk difference under interventions
		surv_1[1]    <- 1 - (ylc[1] + ynlc[1]);
		cilc_1[1]  <- ylc[1];
		cinlc_1[1] <-  ynlc[1];
		for (k in 2:J){
			surv_1[k]  <-                  (1-(ylc[k] + ynlc[k]))*(surv_1[k-1]);
			cilc_1[k]  <-   cilc_1[k-1]  + (   ylc[k]           )*(surv_1[k-1]); 
			cinlc_1[k] <-   cinlc_1[k-1] + (            ynlc[k] )*(surv_1[k-1]); 
		}
    }// interv 2
    if(interv==3){    
		// calculate survival, survival difference, risk difference under interventions
		surv_33[1]    <- 1 - (ylc[1] + ynlc[1]);
		cilc_33[1]  <- ylc[1];
		cinlc_33[1] <-  ynlc[1];
		for (k in 2:J){
			surv_33[k]  <-                  (1-(ylc[k] + ynlc[k]))*(surv_33[k-1]);
			cilc_33[k]  <-   cilc_1[k-1]  + (   ylc[k]           )*(surv_33[k-1]); 
			cinlc_33[k] <-   cinlc_1[k-1] + (            ynlc[k] )*(surv_33[k-1]); 
		}
    }// interv 3
        if(interv==4){    
		// calculate survival, survival difference, risk difference under interventions
		surv_nc[1]    <- 1 - (ylc[1] + ynlc[1]);
		cilc_nc[1]  <- ylc[1];
		cinlc_nc[1] <-  ynlc[1];
		for (k in 2:J){
			surv_nc[k]  <-                   (1-(ylc[k] + ynlc[k]))*(surv_nc[k-1]);
			cilc_nc[k]  <-   cilc_nc[k-1]  + (   ylc[k]           )*(surv_nc[k-1]); 
			cinlc_nc[k] <-   cinlc_nc[k-1] + (            ynlc[k] )*(surv_nc[k-1]); 
		}
    }// natural course

    
    }//end intervention
}// end local scope

}