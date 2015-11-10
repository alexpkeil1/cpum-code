/**********************************************************************************************************************
* Author: Alex Keil
* Program: cpum_repl_20151107.sas
* Date: Saturday, November 7, 2015 at 10:17:31 PM
* Project: Replicating some beir VI results using CPUM data
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
 SET cpum.an0001;
LIBNAME oldcpum "Z:\\School\Dissertation\data";
OPTIONS NOFMTERR;
DATA oldan; MERGE oldcpum.cpum_an02 oldcpum.cpum_bl01; BY id;
RUN;


PROC PHREG DATA = oldan;
 TITLE 'Cox PH model';
 WHERE pyar>0;
 MODEL (agein ageout)*d_LC(0) = cum100wlm5lag;
  cum100wlm5lag = MAX(0, (cumwlm5lag-bl_cumwlm))/100; *1.053 with old data;
RUN;
PROC PHREG DATA = an;
 TITLE 'Cox PH model';
 MODEL (agein ageout)*d_LC(0) = cum100wlm5lag;
  cum100wlm5lag = (cumwlm5lag)/100; *1.053;
RUN;


PROC NLMIXED DATA=an ;
 TITLE "ERR model - PROC NLMIXED";
 *WHERE pyar>0;
 eta = beta0;
 lambda = exp(eta)*(1 + beta2*cumwlm2lag/100);
 MODEL d_lc ~ POISSON(py*lambda); *err of 0.4790 (.77 with 5 year lag, .593 with 2 year lag) 
                                   per 100wlm (old data gives .3329 or 0.55?), beir gives 0.42;
 ODS OUTPUT parameterestimates = nlmixed;
RUN;






*proc optmodel to fit linear err Cox model;

PROC OPTMODEL;* FD=CENTRAL PRESOLVER=3;
 SET OBS, Nbeta={1..2};
 NUMBER x{OBS, NBETA}, in{OBS}, out{OBS}, y{OBS}, death{OBS}, cohort{OBS};
 READ DATA a() INTO OBS=[_N_] y;
 READ DATA a(KEEP=x z RENAME=(x=x1 z=x2)) INTO OBS=[_N_] {j IN nbeta}  <x[_N_,j]= COL("x"||j)>; 
 READ DATA a() INTO OBS=[_N_] cohort in out;
 *by group processing;
 *model components;
 VAR betaX{Nbeta}  >= -400 <= 400;
 IMPVAR theta{i IN OBS_BY} = SUM{p IN Nbeta} x[i,p]*betaX[p];
 *cox's likelihood (Breslow's method for ties);
 IMPVAR lpli{i IN OBS_BY}  = IF y[i]=1 
  THEN (LOG(1+(theta[i])) - LOG( SUM {j IN OBS_BY}  (in[j]<out[i]<=out[j])*(1+ SUM{p IN Nbeta} x[j,p]*betaX[p]) ))
  ELSE log(1);
 *efron's likelihood (todo); 
  MAX lpl = SUM{i IN OBS_BY} lpli[i];
  IMPVAR deviance = -2*lpL;
 *storage matrices;
 NUM betax_by{Nbeta};
 NUM deviance_by{BYSET};
 *loop over by values;
  SOLVE WITH nlp / ; 
    PRINT deviance 12.8 betax;
 END;
 CREATE DATA err_deviance FROM [i] deviance_by;
 CREATE DATA err_betaX FROM [cohort parm] betax_by;*linear components;
QUIT;


RUN;QUIT;RUN;
/*DM ODSRESULTS 'clear;' CONTINUE; *clear ODS generated datasets;*/
