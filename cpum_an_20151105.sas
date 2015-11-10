/**********************************************************************************************************************
* Author: Alex Keil
* Program: cpum_an_20151101.sas
* Date: Friday, November 6, 2015 at 8:55:38 AM
* Project: colorado plateau uranium miners
* Tasks: create analysis file with exposure only accrued after follow-up starts counted as cumulative exposure
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


DATA vs; SET cpum.vs0001;
DATA demo; SET cpum.demo0001;
DATA expwide (RENAME=(cumwlm=oldcumwlm cumyrsexp=oldcumyrsexp)); SET cpum.exp0111;RUN;

%LET censage=99;
%MACRO lag(lag);
  IF i>&LAG THEN DO;
    yrsexp&lag.lag = MAX(0,pyexp[i-&LAG]*(ageout-agein));
    yrsatwork&lag.lag = MAX(0, yrsatworkarr[i-&LAG]);    
    atwork&lag.lag = (yrsatwork&lag.lag>0);
    wlm&lag.lag = MAX(0,annwlm[i-&LAG]*(ageout-agein));
    cumwlm&lag.lag = SUM(cumwlm&lag.lag, wlm&lag.lag);*add in partial-year exposure for those with early exits and a long enough exposure history;
  END;
  IF i>&LAG THEN cumyrsexp&lag.lag = SUM(cumyrsexp&lag.lag, yrsexp&lag.lag);
  IF cumyrsexp&lag.lag>0 THEN avgwlm&lag.lag=cumwlm&lag.lag/cumyrsexp&lag.lag; ELSE avgwlm&lag.lag=0;
%MEND;
%MACRO INITLAG(LAG);
cumwlm&lag.lag=0;wlm&lag.lag=0; cumyrsexp&lag.lag=0; cumyrsatwork&lag.lag=0;yrsexp&lag.lag=0; avgwlm&lag.lag=0;atwork&lag.lag=0;
%MEND;


DATA an0001 (KEEP=id ageout agein yob datein dateout cumwlm: eligage dod wlm yrsexp: cumyrsexp: atwork:
                       avgwlm: d_: icd icdver eligdate  dlo agelo ageatadmincens dob bl_: py sex race cohort: smoke: date_fe termdate priorwlm ) 
          ERR (KEEP= id  ageout dlo dob eligage wlm age_fe cumwlm); 
 LENGTH  id 8 agein ageout datein dateout d_lc d_any d_copd d_nonlc dob eligdate dlo dod  wlm cumwlm yrsexp cumyrsexp 8;
 MERGE vs(IN=invs) demo expwide ;
 BY id;
 IF dob <=.z OR dlo<=.z OR eligdate<=.z THEN OUTPUT err;
 ELSE DO;

 *AGES 8-99;
 ARRAY annwlm[*] annwlm008-annwlm0&censage;
 ARRAY pyexp[*] pyexp008-pyexp0&censage;
 ARRAY yrsatworkarr[*] pyexp008-pyexp0&censage;
 cumwlm=0; cumyrsexp=0; cumyrsatwork=0;;yrsexp=0; d_lc=0; d_any=0; avgwlm=0;
  *%initLAG(0);*test;
  %initLAG(2);
  %initLAG(5);
  %initLAG(10);
  %initLAG(15);
  %initLAG(20);
  %initLAG(25);
  IF dlo NE dod AND dod NE . THEN ERR=1;
  *change DLO to match Schubauer-Berigan dates;
  IF dlo > '31dec2005'd THEN dlo = '31dec2005'd;
  IF dod > '31dec2005'd THEN DO;
      dlo = '31dec2005'd;
	  dod = .;
	  y_any=0; y_lc=0; y_copd=0; d_nonlc=0;
	  *loss of 50 deaths: to 3075 (2960 after 1960, compare to 2964 in schubauer berigan );
	  *one death from earlier censoring: died at age 107, but censored in this study;
  END;
  
  yob = 1960 + YRDIF(0, dob, 'ACT/ACT');
  agelo = YRDIF(dob, dlo, 'ACT/ACT');
  ageatadmincens = YRDIF(dob, '31dec2005'd, 'ACT/ACT');
 afe=.;
 IF y_any=. THEN y_any=0;
 IF y_lc=. THEN y_lc=0;
 IF y_copd=. THEN y_copd=0;
 d_nonlc=0;
 *fix some recrods where the 'pybegin' variable from the raw data comes before the actual exposure date;
 neweligage = eligage;
 IF age_fe>eligage THEN DO; neweligage=age_fe; err=2; END;
 DO i = 1 TO DIM(annwlm);
  agein = i+7;
  ageout=agein+1;
  datein = yob+agein;
  dateout = yob+ageout;

  IF FLOOR(neweligage) = agein THEN agein = neweligage; *realign to start of first eligibility date;
  py = ageout-agein;
  yrsexp=MAX(0,pyexp[i]);
  yrsatwork = MAX(0, yrsatworkarr[i]);
  atwork = (yrsatwork>0);;
  wlm = MAX(0,annwlm[i]);
  cumwlm = SUM(cumwlm, wlm);
    d_any = 0; d_lc=0; d_copd=0; d_nonlc=0;
  IF dod>.z THEN DO;
   IF agein < (YRDIF(dob,dod, 'ACT/ACT')) <= ageout  THEN DO;
    d_any = y_any; d_lc=y_lc; d_copd=y_copd; d_nonlc=d_any-d_lc;
    ageout = YRDIF(dob,dod, 'ACT/ACT');
	dateout = yob+ageout;
   END;
  END;
   ELSE IF dlo>.z THEN DO;
   IF agein < (YRDIF(dob,dlo, 'ACT/ACT')) <= ageout  THEN DO;
    d_any = y_any; d_lc=y_lc; d_copd=y_copd; d_nonlc=d_any-d_lc;
    ageout = YRDIF(dob,dlo, 'ACT/ACT');
	dateout = yob+ageout;
   END;
  END;
cumyrsexp = SUM(cumyrsexp, yrsexp);
cumyrsatwork = SUM(cumyrsatwork, yrsatwork);
IF cumyrsexp>0 THEN avgwlm=cumwlm/cumyrsexp; ELSE avgwlm=0;
  *%LAG(0);*test;
  %LAG(2);
  %LAG(5);
  %LAG(10);
  %LAG(15);
  %LAG(20);
  %LAG(25);

  IF dlo>.z AND eligdate>.z AND agein>.z AND 
   neweligage<=agein<YRDIF(dob, dlo, 'ACT/ACT') THEN OUTPUT an0001;
   IF err>.z THEN output err;
 END;
  ;
  END;
RUN;



*create leave work variable;
/*PROC SORT DATA = an0001; BY id DESCENDING agein;
DATA an0001 (DROP=lastwork); 
  SET an0001;
 BY id DESCENDING agein;
 RETAIN lastwork;
 IF first.id THEN lastwork=1; *don't say they left work if it's just censoring at last work period;
 IF atwork=1 and lastwork=0 THEN leftwork=1;
 ELSE leftwork=0;
OUTPUT;
lastwork=atwork;
*/
DATA an0001 (DROP=lastwork); 
  SET an0001;
 BY id agein;
 RETAIN lastwork;
 IF first.id THEN lastwork=1; *don't say they left work if it's just censoring at last work period;
 IF atwork=0 and lastwork=1 THEN leftwork=1;
 ELSE leftwork=0;
OUTPUT;
lastwork=atwork;

PROC SORT DATA = an0001; BY id agein; RUN;

DATA cpum.an0001; SET an0001;run;


*closer inspection of some deaths (checking why one might be missing from analysis file);
DATA lastids;
 SET an0001(keep=id ageout dod dlo d_:);
 BY id ageout;
 IF last.id;

DATA dt;
 MERGE lastids(IN=ina KEEP=id dlo dod d_:)  vs(KEEP=id icd: y_any);
 BY id;
 IF ina and d_any = 1 THEN incl=1; ELSE incl=0;
RUN;
PROC SORT DATA=DT; BY incl DESCENDING dlo;run;

*checking whether first ids are at work;
DATA firstwork (KEEP=ID);
 SET an0001;
 BY id;
 IF first.id AND atwork=0 THEN OUTPUT;

DATA firstworkers;
 MERGE an0001 (KEEP=id agein ageout atwork wlm py bl_:) firstwork (IN=inb);
 BY id;
 IF inb;
RUN;



*checking some distributions, comparing to schubauer berigan;
PROC MEANS DATA = an0001 MEAN SUM MAX NMISS;;
 TITLE 'Distribution in analysis data';
RUN;
PROC MEANS DATA = an0001 MEAN SUM MAX NMISS;;
 TITLE 'matching Schubauer Berigan';
 WHERE dateout>1960;
RUN;

DATA an0001b; SET an0001;
 IF dateout>1960 THEN surv60=1; ELSE surv60=0;
RUN;
PROC MEANS DATA = an0001b NOPRINT;
 CLASS id surv60;
 VAR py;
 OUTPUT OUT = sumpy SUM=py;

PROC MEANS DATA = sumpy SUM MEAN;
 CLASS surv60 _TYPE_ / MISSING;
 VAR PY;
RUN;
  


*compare with older version;
LIBNAME oldcpum "Z:\\School\Dissertation\data";
OPTIONS NOFMTERR;
DATA oldan; SET oldcpum.cpum_an02;
RUN;

PROC MEANS DATA = oldan NOPRINT;
 CLASS id;
 WHERE pyar>0;
 VAR wlm d_lc atwork;
 OUTPUT OUT=oldstuff SUM=cumwlm_OLD d_lc_OLD cumatwork_old;
PROC MEANS DATA = oldan NOPRINT;
 CLASS id;
 VAR wlm d_lc atwork;
 OUTPUT OUT=oldstuff2 SUM=cumwlm_OLD d_lc_OLD cumatwork_old;
PROC MEANS DATA = an0001 NOPRINT;
 CLASS id;
 VAR wlm d_lc atwork;
 OUTPUT OUT=newstuff SUM=cumwlm d_lc cumatwork;
RUN;

DATA compare;
 LENGTH id cumwlm_old cumwlm d_lc_old d_lc cumatwork_old cumatwork 8; 
 MERGE oldstuff newstuff;
 BY id;
 FORMAT _NUMERIC_ best9.;
 DIFF = (d_lc ^= d_lc_old)*5 + (cumatwork_old ^= cumatwork)*2 + (ROUND(cumwlm_old) ^= ROUND(cumwlm));
RUN;



PROC SORT DATA = COMPARE; BY descending diff DESCENDING d_lc_old;
RUN;

RUN;QUIT;RUN;
/*DM ODSRESULTS 'clear;' CONTINUE; *clear ODS generated datasets;*/
