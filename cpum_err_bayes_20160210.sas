/**********************************************************************************************************************
* Author: Alex Keil
* Program: cpum_err_bayes_20160210.sas
* Date: Wednesday, February 10, 2016 at 2:20:15 PM
* Project:
* Tasks:
* Data in: 
* Data out:
* Description: 
* Released under the GNU General Public License: http://www.gnu.org/copyleft/gpl.html
**********************************************************************************************************************/
*clear the log window and the output window;
DM LOG 'clear;' CONTINUE; DM OUT 'clear;' CONTINUE; 
OPTIONS MERGENOBY = warn NODATE NONUMBER LINESIZE = 120  PAGESIZE=80 SKIP = 2 FORMDLIM = '-' MPRINT NOCENTER;
OPTIONS FORMCHAR = '|----|+|---+=|-/\<>*';

*** macro parameters for location ***********************;
%LET PATH = Z:\\EpiProjects\CPUM;
*** end macro parameters for location *******************;
%LET output = &PATH./output;
%LET data = &PATH./data;

LIBNAME cpum "&data";

DATA an;
* SET cpum.an0001(WHERE=(smoke3_2>.z AND race=2));
 SET cpum.an0001(WHERE=(smoke3_2>.z));

PROC NLMIXED DATA=an ;
 TITLE "ERR model - PROC NLMIXED";
 lambda = exp(a0)*(1 + err*cumwlm2lag/100);
 MODEL d_lc ~ POISSON(py/10000*lambda);
 ODS OUTPUT parameterestimates = nlmixed;
RUN;

PROC MCMC DATA = an/* SEED = 12123*/ NMC = 10000 OUTPOST=dpost 
MONITOR=(a0 err);
 TITLE "ERR model, lung cancer";
 ODS SELECT PostSumInt ESS TADPanel;
*priors;
 PARMS a0 {0} err {0};
 PRIOR a0 ~ GENERAL(0);
 PRIOR err ~ GENERAL(0);
 *PRIOR a0 ~ NORMAL(0, var=100); 
 *PRIOR err~ NORMAL(1, var=4, lower=-1);

*err model;
  mu = py/10000*exp(a0)*(1 + err*cumwlm2lag/100);
  MODEL d_lc ~ BINARY(mu);
RUN;

PROC MEANS DATA = dpost MEDIAN P5 P95;
 VAR a0 err;
RUN;


RUN;QUIT;RUN;
/*DM ODSRESULTS 'clear;' CONTINUE; *clear ODS generated datasets;*/
