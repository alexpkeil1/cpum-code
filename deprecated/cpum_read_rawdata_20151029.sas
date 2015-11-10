*clear the log window and the output window;
/**********************************************************************************************************************
* Author: Alex Keil
* Program: cpum_read_rawdata_20151029.sas
* Date: Friday, November 16, 2012 4:50:55 PM
* Project: Dissertation (colorado miners)
* Tasks: 
* Data in: "Z:\\EpiProjects\CPUM\data\raw\ltas_UR3_demo_FOIA_201112.txt";
*		   "Z:\\EpiProjects\CPUM\data\raw\ltas_UR3_outcome_FOIA_201112.txt";
*		   "Z:\\EpiProjects\CPUM\data\raw\ltas_UR3_hist_FOIA_201112.txt";
* Data out: all in Z:\\EpiProjects\CPUM\data
		SAS data files:
			CPUM_an02.sas7bdat - Analysis file, single record per 1 year of age
			CPUM_bl01.sas7bdat - Baseline variables (baseline time = time at 1st interview) including baseline exposure, employment
			CPUM_dg01.sas7bdat - Demographics file, single record per individual
			CPUM_vs02.sas7bdat - vital status file, based on LTAS analysis data
			

* Description: Read in raw data from colorado plateau u miners study from NIOSH, compare with historical data
* Keywords: CPUM
*
* CHANGES: 2/11/13 - added 5, 10 year lagged employment variables
*		   2/18/13 - separated time periods for pre-post enrollment
*		   3/15/13 - fixed person time calculation mistake
*		   3/18/13 - Added baseline dataset to stata data sets
*		   4/4/13 - Changed lag code to allow a list of comma separated lags
*          10/29/15 - restricted number of datasets to output
**********************************************************************************************************************/
/*
Data Dictionary for ltas_UR3_demo_FOIA_201112.txt (recfmt=V, lrecl=256)
Columns	Variable	Description
001-006	unique id	Unique ID for each study subject (identical across all three files)
008	gender			Gender (1=male; 2=female)
010	race			Race (1=white; 2=all other races)
012	VS				Vital status (1=alive; 2=deceased)
014-021	DOB			Date of birth as mmddyyyy (note: dd assigned a value of "00")
023-030	PY begin	Person-year begin date as mmddyyyy
032-039	DLO			Date last observed as mmddyyyy (note: dd assigned a value of "00")
041	Smoking			Last smoking category (1=Never; 2=Former; 3=Current, <1 pack/day; 4=Current, 1 pack/day; 5=Current, >1 pack/day; 9=Unknown)


Data Dictionary for ltas_UR3_outcome_FOIA_201112.txt (recfmt=V, lrecl=256)
Columns	Variable	Description
001-006	unique id	Unique ID for each study subject (identical across all three files)
008-015	Date		Outcome date as mmddyyyy (note: dd assigned a value of "00")
017	Terminal		Status indicator for whether outcome is terminal (fatal) (F=false; T=true)
019	Underlying		Status indicator for whether outcome is underlying (F=false; T=true)
021-025	ICDcode		ICD code associated with outcome (coded to revision in effect at time of death)


Data Dictionary for ltas_UR3_hist_FOIA_201112.txt (recfmt=V, lrecl=256)
Numeric exposure data are stored in an exponential format
Columns	Variable	Description
001-006	unique id	Unique ID for each study subject (identical across all three files)
008-015	Begin date	Begin date for exposure history record (as mmddyyyy)
017-024	End date	End date for exposure history record (as mmddyyyy)
026-40	WLM			Working level month
042	Employed		Status indicator for employment . (0=no; 1=yes). Used to accumulate employment duration.
*/
OPTIONS MERGENOBY = warn NODATE NONUMBER LINESIZE = 120 SKIP = 2 MPRINT NOCENTER;
OPTIONS FORMCHAR = "|----|+|---+=|-/\<>*";
%LET PROGNAME =	cpum_read_rawdata_20121116.sas;
TITLE;
FOOTNOTE "&progname run at &systime on &sysdate";

*** macro parameters for location ***********************;
%LET PATH = Z:\\EpiProjects\CPUM;
*** end macro parameters for location *******************;



%LET output = &PATH./output;
%LET data = &PATH./data;


LIBNAME cpum "&data";

*set minimum and maximum lags to calculate;
*%LET laglist=%STR(1,2,3,4,5,6,7,8,9,10,11,14,15,16,19,20,21,24,25,26); 
%LET laglist=%STR(2, 5, 10, 20); 

*count the lags;
%LET lagcount=%EVAL(%SYSFUNC(COUNTW(&laglist)));


FILENAME demo "&data./raw/ltas_UR3_demo_FOIA_201112.txt";
FILENAME out "&data./raw/ltas_UR3_outcome_FOIA_201112.txt";
FILENAME hist "&data./raw/ltas_UR3_hist_FOIA_201112.txt";

PROC FORMAT CNTLOUT="&data./raw_data_formats";
	VALUE smform 1="Never" 2="Former" 3="Current, <1 pack/day" 4="Current, 1 pack/day" 5="Current, >1 pack/day" 9="Unknown";
	VALUE $ tf  "T"="True" "F"="False";
	VALUE vsf 1="Alive" 2 = "Deceased"; 
	VALUE racef 1 = "White" 2 = "Other";
	VALUE sexf 1 = "Male" 2= "Female";
	VALUE empf 1= "Yes" 0 = "No";
	VALUE da 1="A" 2= "D";
	VALUE cohortf -3 = "1870-1879" -2 = "1880-1889" -1 = "1890-1899" 0 = "1900-1909" 1 = "1910-1919" 2 = "1920-1929" 3 = "1930-1939" 4 = "1940-1949";
	VALUE cohort2f 0 = "<1910" 1 = "1910-1919" 2 = "1920-1929" 3 = ">1929";
RUN;
DATA demo (DROP=tempfla: dobm: doby: dlom: dloy: inm: iny:);
	INFILE demo;
	INPUT id 1-6 sex 8 race 10 vs 12 tempflag1 16-17 tempflag2 25-26 tempflag3 34-35 smokelast 41 @@;
		IF tempflag1 NE 0 THEN DO; 
			INPUT @14 dob MMDDYY8. @23 indate  MMDDYY8. @32 dlo MMDDYY8. @@; 
		END;
		ELSE IF tempflag1 = 0 THEN DO;
			INPUT dobmonth 14-15 dobyear 18-21 @@;
		END;
		IF tempflag2 NE 0 THEN DO; 
			INPUT @23 indate  MMDDYY8. @@; 
		END;
		ELSE IF tempflag2 = 0 THEN DO;
			INPUT inmonth 23-24 inyear 27-30 @@;
		END;
		IF tempflag3 NE 0 THEN DO; 
			INPUT @32 dlo MMDDYY8.; 
		END;
		ELSE IF tempflag3 = 0 THEN DO;
			INPUT dlomonth 32-33 dloyear 36-39;
		END;
	IF tempflag1 = 0 THEN dob = MDY(dobmonth+1*(dobmonth<12)-11*(dobmonth=12), 1, dobyear+1*(dobmonth=12))-1;*take last day in month;
	IF tempflag2 = 0 THEN indate = MDY(inmonth+1*(inmonth<12)-11*(inmonth=12), 1, inyear+1*(inmonth=12))-1;*take last day in month;
	IF tempflag3 = 0 THEN dlo = MDY(dlomonth+1*(dlomonth<12)-11*(dlomonth=12), 1, dloyear+1*(dlomonth=12))-1;*take last day in month;
	IF indate > . THEN agestart=YRDIF(dob, indate+1,'ACT/ACT');
	cohort = FLOOR(YEAR(dob)/10)-190;
	cohort2 = cohort;
	IF cohort <0 THEN cohort2 = 0;
	ELSE IF cohort =4 THEN cohort2 = 3;
	IF smokelast IN (1) THEN smoke_0=1; ELSE smoke_0=0; *never;
	IF smokelast IN (2) THEN smoke_1=1; ELSE smoke_1=0; *former;
	IF smokelast IN (3) THEN smoke_2=1; ELSE smoke_2=0; *<1 pack per day;
	IF smokelast IN (4) THEN smoke_3=1; ELSE smoke_3=0; *1 pack per day;
	IF smokelast IN (5) THEN smoke_4=1; ELSE smoke_4=0; *>1 pack per day;
	IF smokelast IN (2) THEN smoke3_1=1; ELSE smoke3_1=0; *former;
	IF smokelast IN (3,4,5) THEN smoke3_2 = 1; ELSE smoke3_2 = 0;*current;
	IF smokelast IN (9) THEN DO; *ukn;
		smoke_0=.; smoke_1=.; smoke_2=.; smoke_3=.;smoke_4=.; smoke3_1=.;smoke3_2=.;*2/6/13 update, added smoke3_1 to this line;
	END;


	LABEL sex = "Sex" 
		  race = "Race"
		  vs = "Vital status as of 12/31/2005"
		  smokelast = "Smoking status as of 12/31/2005"
		  dob = "Date of birth"
		  indate = "Start date in study"
		  dlo = "Date last observed"
		  agestart= "Age at first interview"
		  cohort = "Birth cohort"
		  cohort2 = "Birth cohort (collapsed)"
		  smoke_0 = "Never smoker"
		  smoke_1 = "Former smoker"
		  smoke_2 = "Current, <1 pack/day"
		  smoke_3 = "Current, 1 pack/day"
		  smoke_4 = "Current, >1 pack/day"
		  smoke3_1 = "Former smoker"
		  smoke3_2 = "Current smoker (any amount)";
	FORMAT dob indate dlo MMDDYY10. smokelast SMFORM. vs VSF. race RACEF. sex SEXF. cohort cohortf. cohort2 cohort2f.;
RUN;

/*
PROC FREQ DATA = demo;
	TITLE "Birth cohorts, smoking";
	TABLES cohort cohort2;
	TABLES smokelast;
RUN;
*/
DATA out (DROP=tempflag month year icd3);
	INFILE out;
	INPUT id 1-6 tempflag 10-11 terminal $ 17 underlying $ 19 icd $ 21-25 @@;
		IF tempflag NE 0 THEN DO; 
			INPUT @8 outdate MMDDYY8.; 
		END;
		ELSE IF tempflag = 0 THEN DO;
			INPUT month 8-9 year 12-15;
		END;
	IF tempflag = 0 THEN outdate = MDY(month+1*(month<12)-11*(month=12), 1, year+1*(month=12))-1;

	ICD3 = SUBSTR(icd, 1, 3);
	*ICD version;
	IF  	1949 <= YEAR(outdate) <= 1957 THEN icdver=6;
	ELSE IF 1958 <= YEAR(outdate) <= 1967 THEN icdver=7;
	ELSE IF 1968 <= YEAR(outdate) <= 1978 THEN icdver=8;
	ELSE IF 1979 <= YEAR(outdate) <= 1998 THEN icdver=9;
	ELSE IF 1999 <= YEAR(outdate) <= 2006 THEN icdver=10; 
	
*SPECIFIC CAUSES OF DEATH;
	IF ICD NE "" THEN DO;
	*Malignant neoplasm of the trachea,bronchus, and lung;
	*hand checked against LTAS definitions - OK;
		IF icdver = 6 AND ICD3 IN ("162", "163") THEN lc = 1;
		ELSE IF icdver = 7 AND ICD IN ("162.0","162.1","162.8","163") THEN lc = 1;
		ELSE IF icdver = 8 AND ICD3 IN ("162") THEN lc = 1;
		ELSE IF icdver = 9 AND ICD3 IN ("162") THEN lc = 1;
		ELSE IF icdver = 10 AND ICD3 IN ("C33", "C34") THEN lc = 1;
		ELSE lc = 0;
		LABEL lc = "Lung cancer as terminal cause of death";	
	
	*COPD;
	*Hand checked against LTAS definitions - possible err0r in LTAS coding for ICD code 519.3;
	*update - 519.3 not in WHO handbook, but added in US as per NCHS, CDC;
		IF icdver = 6 AND ICD3 IN ("501", "502") OR icd = "527.1" THEN copd = 1;
		ELSE IF icdver = 7 AND (ICD3 IN ("501", "502") OR icd = "527.1") THEN copd = 1;
		ELSE IF icdver = 8 AND (ICD3 IN ("490", "491", "492") OR icd =  "519.3") THEN copd = 1;
		ELSE IF icdver = 9 AND ICD3 IN ("490", "491", "492", "496") THEN copd = 1;
		ELSE IF icdver = 10 AND ICD3 IN ("J40", "J41", "J42", "J43", "J44") THEN copd = 1;
		ELSE copd = 0;
		LABEL copd = "COPD as terminal cause of death";	
	
	END;

	LABEL terminal = "Indicator: cause of death is terminal"
		  underlying = "Indicator: cause of death is underlying"
		  icd = "ICD code active at time of death"
		  icdver = "ICD version active at time of death";
	FORMAT outdate MMDDYY10. terminal underlying $TF.;
RUN;



DATA hist;
	INFILE hist;
	INPUT id 1-6 @8 begdate MMDDYY8. @17 enddate MMDDYY8. wlmperday 26-40 emp 42; 
	
DATA hist(DROP=priorwlm ) baseline(KEEP=id priorwlm) hdate(KEEP=id hiredate) tdate(KEEP=id termdate);
 SET hist;
 BY id begdate;
	FORMAT begdate enddate MMDDYY10.;
	wlm = (wlmperday)*(enddate-begdate+1)*emp;
	*wlm = (wlmperday)*(enddate-begdate)*emp; *incorrect;
	wlm_all = wlm + (enddate=begdate)*wlmperday;
	priorwlm = (enddate=begdate)*wlmperday;
	LABEL wlm_all = "Rad exp at end of period, incl. hard rock (WLM)"
		  wlm = "Rad exp at end of period, on U mining only (WLM)"
		  priorwlm = "Hard rock mining radon exposure (WLM)"
		  hiredate = "First recorded exposure history record"
		  begdate="Begin date for exposure history record"
		  enddate="End date for exposure history record"
		  wlmperday = "Exposure rate (WLM/per day)"
		  emp = "Employed during period";
	FORMAT emp empf. begdate enddate hiredate MMDDYY10.;
	IF first.id THEN DO;
	 IF emp=1 THEN hiredate=begdate;
	 ELSE hiredate=begdate+1;
	 OUTPUT hdate;
	END;
	IF last.id THEN DO;
	 IF emp=1 THEN termdate=enddate;
	 ELSE termdate=begdate+1;
	 OUTPUT tdate;
	END;
	IF priorwlm>0 THEN OUTPUT baseline;
	OUTPUT hist;
RUN;

PROC IMPORT OUT =ltac DATAFILE="&data/raw/ltas_causes2.csv" DBMS=CSV REPLACE;
RUN;

DATA vs02;
 MERGE out ltac(RENAME=(personid=id cause=ltas_cause) KEEP=personid cause);
 BY id;
 IF 2<ltas_cause<=38 AND ltas_cause NE 92 THEN allcancer=1; ELSE allcancer=0;
 IF allcancer=1 THEN noncancer=0; ELSE IF ltas_cause NE 92 THEN noncancer=1;
 LABEL allcancer = "Cancer as terminal cause of death (LTAS)";	
 LABEL noncancer = "Non-cancer as terminal cause of death (LTAS)";	
RUN;


/*
PROC FREQ DATA = hist;
	TITLE "Checking employment status variable";
	WHERE begdate=enddate;
	TABLES emp;
RUN;

PROC MEANS DATA = hist MIN MAX;
	TITLE "Start and end date of exposure data";
	VAR begdate enddate;
RUN;

DATA hist_gaps;
	SET hist;
	BY id begdate;
	RETAIN lagdate;
	IF first.id THEN gapdate=0;
	ELSE IF begdate-lagdate>1 THEN gapdate=1;
	ELSE IF begdate-lagdate=1 THEN gapdate=0;
	ELSE IF lagdate>begdate THEN gapdate=9;
	lagdate=enddate; 
	FORMAT lagdate MMDDYY10.;
RUN;

PROC FREQ DATA = hist_gaps;
	TITLE "Check for gaps in exposure history"; *ok;
	TABLES gapdate;
RUN;
*/


*create person month record;
DATA monthly_t(KEEP = id wlmt int outt weekin emp );
	SET hist(KEEP = id begdate enddate wlmperday emp  WHERE=(emp=1));
	BY id begdate;
	RETAIN wlmt;
	DO day = begdate TO enddate BY 1;
		outt=day;
		*first day of the month, or first day of record;
		IF day = begdate OR MONTH(day) NE MONTH(day-1) THEN DO;
			wlmt=0;
			int=day;
			weekin = YEAR(int)*1000+MONTH(int);
		END;
		*not last day of the month;
		IF MONTH(day) = MONTH(day+1) THEN DO;
			wlmt = wlmt+wlmperday;
			IF day = enddate THEN OUTPUT;
		END;
		*last day of the month;
		ELSE IF MONTH(day) NE MONTH(day+1) THEN DO;;
			wlmt = wlmt+wlmperday;
			outt=day;
			OUTPUT;
		END;
	END;
	FORMAT int outt MMDDYY10.;
RUN;

PROC SORT DATA = monthly_t;
	BY id weekin;
DATA monthly_tb;
	MERGE monthly_t;
	BY id weekin;
	RETAIN wlm in out;
	IF first.weekin THEN DO;
		wlm = wlmt;
		in = int;
		out=outt;
	END;
	ELSE DO;
		wlm = wlm + wlmt;
		out=outt;
	END;
	FORMAT in out MMDDYY10.;
	IF last.weekin THEN OUTPUT;
RUN;			

DATA monthlynolag cum_checknolag(KEEP = id cum: );	
	MERGE monthly_tb(KEEP=id in out wlm weekin emp );
	BY id weekin;
	RETAIN cumwlm ;
	IF wlm=. THEN wlm=0;
	IF first.id THEN DO;
		cumwlm = wlm;
	END;
	ELSE DO;
		cumwlm = cumwlm + wlm;
	END;
	IF last.id THEN OUTPUT cum_checknolag;
	OUTPUT monthlynolag;
RUN;


OPTIONS SYMBOLGEN;
%MACRO domorelags(lags=%STR(&laglist), nlags=&lagcount);
*create person month record - lagged &i years;
%DO j = 1 %TO &nlags;
	%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

	DATA monthly_t&I.LAG(KEEP = id wlmt int outt weekin  int&I.lag outt&I.lag emp);
		SET monthly_t(KEEP = id wlmt int outt weekin emp RENAME=(weekin=weekin2));
		weekin=weekin2+&I.*1000;
		int&I.lag = INTNX('YEAR', int, &I., 'SAME');
		outt&I.lag = INTNX('YEAR', outt, &I., 'SAME');
		FORMAT int&I.lag outt&I.lag MMDDYY10.;
	RUN;
PROC SORT DATA = monthly_t&I.LAG;
	BY id weekin;
DATA monthly_tb&I.lag;
	SET monthly_t&I.LAG(RENAME=(wlmt=wlmt&I.lag emp=emp&I.lag));
	BY id weekin;
	RETAIN wlm&I.lag in out;
	IF first.weekin THEN DO;
		wlm&I.lag = wlmt&I.lag;
		in = int&I.lag;
		out=outt&I.lag;
	END;
	ELSE DO;
		wlm&I.lag = wlm&I.lag + wlmt&I.lag;
		out=outt&I.lag;
	END;
	FORMAT in out MMDDYY10.;
	IF last.weekin THEN OUTPUT;
RUN;
DATA monthly&I.lag(DROP = last:) cum_check&I.lag(KEEP = id cum: );	
	SET monthly_tb&I.lag(KEEP=id in out wlm&I.lag emp&I.lag weekin RENAME=(in=in&I.lag out=out&I.lag));
	BY id weekin;
	RETAIN cumwlm&I.lag lastin&I.lag lastout&I.lag;
	FORMAT lastin&I.lag lastout&I.lag MMDDYY10.;
	IF first.id THEN DO;
		cumwlm&I.lag = wlm&I.lag;
		in&I.lag = MDY(MONTH(in&I.lag), 1, YEAR(in&I.lag));
	END;
	ELSE DO;
		cumwlm&I.lag = cumwlm&I.lag + wlm&I.lag;
		IF in&I.lag-lastout&I.lag>1 THEN in&I.lag = lastout&I.lag+1;
	END;
	lastin&I.lag=in&I.lag; lastout&I.lag=out&I.lag;
	IF last.id THEN OUTPUT cum_check&I.lag;
	OUTPUT monthly&I.lag;
RUN;

%END;

DATA monthly (DROP=weekin last:);
	MERGE monthlynolag 
	%IF addin=1 %THEN %DO; monthly %END; 
	%DO j = 1 %TO &nlags;
	%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));
		monthly&I.lag  
	%END;
	;
	BY id weekin;
	IF wlm=. THEN wlm=0;
	%DO j = 1 %TO &nlags;
	%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));		
	IF wlm&i.lag=. THEN wlm&i.lag=0;
	%END;
	RETAIN lastcumwlm  lastin lastout	%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); lastcumwlm&I.lag %END;;
	IF first.id THEN DO;
		lastcumwlm=.; 
		%DO j = 1 %TO &nlags;
		%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

			lastcumwlm&I.lag=.;
		%END;
	END;
	IF cumwlm=. THEN cumwlm=lastcumwlm;
		%DO j = 1 %TO &nlags;
		%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

		IF in&i.lag = . THEN DO;
			IF lastcumwlm&I.lag = . THEN  cumwlm&I.lag=0;
			ELSE cumwlm&I.lag=lastcumwlm&I.lag;
		END; 
		lastcumwlm&I.lag = cumwlm&I.lag;
		%END;
	lastcumwlm = cumwlm;
	IF in=. THEN DO;
		%DO j = 1 %TO &nlags;
	%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

		IF in&i.lag > . THEN DO;
			in=in&i.lag;out=out&i.lag;
		END;
		%END;
		END;
	IF in-lastout>1 THEN in = lastout+1;
	lastin=in; lastout=out;
RUN;
%MEND;
%DOMORELAGS();
OPTIONS NOSYMBOLGEN;

DATA cum_check;
	MERGE cum_check10lag cum_check5lag cum_checknolag;
	BY id;
RUN;

PROC MEANS DATA = cum_check N MEAN MIN MAX STD FW=6 MAXDEC=2;
	TITLE "Cum rad exposure, monthly data";
	VAR cumw: ;
RUN;
/*
DATA hist(DROP=cumprior cntr) ct (DROP = wlm: emp begdate enddate prior:) checkme(KEEP=id begdate enddate wlm) ;
	SET hist;
	BY id;
	RETAIN cumwlm cumwlm_all cumprior;
	IF first.id THEN DO;
		cumwlm = wlm;
		cumwlm_all = wlm_all;
		cumprior = priorwlm;
		cntr=1;
		firstid=1;
	END;
	ELSE DO;
		firstid=0;
		cumwlm = cumwlm + wlm;
		cumwlm_all = cumwlm_all + wlm_all;
		cumprior = priorwlm + cumprior;
		cntr+1;
		IF begdate=enddate THEN OUTPUT checkme;
	END;
	IF last.id THEN OUTPUT ct;
	OUTPUT hist ;
	LABEL cntr = "Record counter" 
		  cumwlm_all = "Cum. rad exp at end of period, incl pre-study (WLM)"
		  cumwlm = "Cum. rad exp at end of period, on study (WLM)";
RUN;

PROC FREQ DATA= hist;
	TITLE "employment variable - always first observation?";
	TABLES firstid*emp;
RUN;

PROC PRINT DATA=hist ; 
	TITLE "Sample Exposure data";
	WHERE id = 2641;
RUN;


DATA cums;
	MERGE ct demo(KEEP=id race sex);
	BY id;

PROC MEANS DATA = cums N MIN MAX P50 MEAN STD MAXDEC=2 FW=6;
	TITLE "cum exposure data - new data";
	CLASS race sex;
	VAR cum: cntr ;
RUN;
PROC MEANS DATA = cums N MIN MAX P50 MEAN STD MAXDEC=2 FW=6;
	TITLE "cum exposure data - new data";
	VAR cum: cntr ;
RUN;

PROC FREQ DATA = cums;
	TABLES cntr;
RUN;
*/
DATA pmonths;
	MERGE demo(KEEP=id indate dlo dob)  monthly(KEEP=id wl: in out cumwl: em:  );
	BY id;
	IF out <=dlo AND out >= dob;
	IF emp = . THEN emp=0;
	IF in<dob THEN in=dob;
RUN;


%MACRO MANAGEMONTHS(lags=%STR(&laglist), nlags=&lagcount);
*SHOULD BE SAME PARAMETER VALUES AS DOMORELAGS MACRO!!!!;
*analytic data set - using only underlying cause of death;

DATA pmonths(DROP=lastout lastin indate dlo);
	SET pmonths;
	BY id in;
	RETAIN lastout lastin;
		
	IF indate <= in THEN atrisk = 1; ELSE atrisk=0;
	IF first.id THEN DO;
		lastin=in;lastout=out;
		OUTPUT;
		IF indate < in THEN DO;
			out = in-1; in=indate; wlm=0; cumwlm=0; OUTPUT;
		END;
	END;
	ELSE IF last.id THEN DO;
		LASTID=1;
		lastin=in;lastout=out;
		IF in <= dlo <= out THEN DO;
			*get exposure rate for last period;
			exprate = wlm/(out-in+1);
		%DO j = 1 %TO &nlags;
		%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

			exprate&I. = wlm&I.lag/(out-in+1);
		%END;
			*subtract last exposure;
			cumwlm = cumwlm-wlm;
		%DO j = 1 %TO &nlags;
			%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

			cumwlm&I.lag = cumwlm&I.lag-wlm&I.lag;
		%END;
			out = dlo;
			*make corrected exposure for dates;
			wlm = exprate*(out-in+1);
			*add back in corrected exposure;
			cumwlm = cumwlm+wlm;
		%DO j = 1 %TO &nlags;
		%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

			wlm&I.lag = exprate&I.*(out-in+1);
			cumwlm&I.lag = cumwlm&I.lag+wlm&I.lag;
		%END;
			OUTPUT;
		END;
		ELSE IF dlo > out THEN DO;
			OUTPUT;
			in=lastout+1; out=dlo; wlm=0; 
			OUTPUT;
		END;
	END;	
	ELSE OUTPUT;
RUN;


DATA pmonths2(DROP=day weekin last: save:);
	SET pmonths;
	BY id;
	RETAIN lastcumwlm lastwlm
			%DO j = 1 %TO &nlags;
			%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

			lastcumwlm&I.lag   lastwlm&I.lag		
			%END;
			;
	IF first.id THEN DO;
		lastcumwlm = 0; 
		lastwlm = 0; 
			%DO j = 1 %TO &nlags;
			%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

			lastcumwlm&I.lag=0;  lastwlm&I.lag = 0;		
			%END;

	END;
	*period longer than one month;
	IF (out-in+1)>31 THEN DO; 
				savewlm=wlm; 
				savecumwlm=cumwlm;
			%DO j = 1 %TO &nlags;
			%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

				savewlm&I.lag=wlm&I.lag;		
				savecumwlm&I.lag=cumwlm&I.lag;		
			%END;

		*move forward one day at a time;		
		DO day = in TO out BY 1;
			outt=day;
			*first day of the month, or first day of record;
			IF day = in OR MONTH(day) NE MONTH(day-1) THEN DO;
				int=day;
				weekin = YEAR(int)*100+MONTH(int);
				wlm=0;  *initialize exposure to 0;
				cumwlm = lastcumwlm; *initialize cumulative exposure to cumulative exposure at end of last record;
				%DO j = 1 %TO &nlags;
				%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

					wlm&I.lag = 0;		
					cumwlm&I.lag = lastcumwlm&I.lag;		
				%END;

			END;
			*not last day of the month;
			IF MONTH(day) = MONTH(day+1) THEN DO;
				wlm=0; 
				cumwlm = lastcumwlm; 
				%DO j = 1 %TO &nlags;
				%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

					wlm&I.lag = 0;		
					cumwlm&I.lag = lastcumwlm&I.lag;		
				%END;
				IF day = out THEN OUTPUT;
			END;
			*last day of the month;
			ELSE IF MONTH(day) NE MONTH(day+1) THEN DO;;
				outt=day;
				IF day=out THEN DO;
					wlm = savewlm; *set WLM to 
					cumwlm = savecumwlm; 
				%DO j = 1 %TO &nlags;
				%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

					wlm&I.lag = savewlm&I.lag;		
					cumwlm&I.lag = savecumwlm&I.lag;		
				%END;
				END;
				OUTPUT;
			END;
		END;
	END;
	ELSE OUTPUT;
		lastcumwlm = cumwlm;  
		lastwlm = wlm; 
		%DO j = 1 %TO &nlags;
		%LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,)));

			lastcumwlm&I.lag = cumwlm&I.lag;		
			lastwlm&I.lag = wlm&I.lag;		
		%END;

RUN;
%MEND;
%MANAGEMONTHS(lags=%STR(&laglist), nlags=&lagcount);

PROC SORT DATA = pmonths2;
	BY id in;
RUN;

DATA pmonths2(DROP=int outt DOB);
	SET pmonths2;
	IF outt > . THEN DO;
		in=int; out=outt;
	END;
	agein=YRDIF(dob, in+1,'ACT/ACT');
	ageout=YRDIF(dob, out+1,'ACT/ACT');
	LABEL atrisk = "At risk of death";
RUN;



*function to go from monthly data to chunkier time;

%MACRO reducepmonths2(intsize=1, inttype="month", outdat=exp_monthly,lags=%STR(&laglist), nlags=&lagcount);


* bring in monthly data with appropriate lagged exposure already in place;
DATA pmonths3;
	MERGE demo(KEEP = id indate)
	pmonths2;
	BY id;

* aggregate monthly exposures over longer time period - time periods can be defined by
* calendar time: inttype="year" or inttype="month" or age: inttype="age"
* creates cumulative and period specific exposure metrics: __cumwlm, __wlm
* also creates at indicator of work during period with lags: __emp;
DATA __TEMPTIME;
	SET pmonths3;
	BY id in;
	ARRAY w [*] emp %DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); emp&i.lag %END;;
	ARRAY exp [*] wlm  %DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); wlm&i.lag %END;;
	ARRAY cum [*] cumwlm %DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); cumwlm&i.lag %END;;
	ARRAY wR [*] __emp %DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); __emp&i.lag %END;;
	ARRAY expR [*] __wlm %DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); __wlm&i.lag %END;;
	ARRAY cumR [*] __cumwlm %DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); __cumwlm&i.lag %END;;
	RETAIN __wlm %DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); __wlm&i.lag %END; __cumwlm 
	%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); __cumwlm&i.lag %END; __in __agein __LASTMONTHIN __LASTYEARIN __LASTAGEIN __emp 
	%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); __emp&i.lag %END;;;
	*create seperate person periods for at risk vs. not at risk;
	*one may enter a period early (i.e. 5 year age category entered at 32, if first interview then, but period still ends at 35);
	__MONTHIN = FLOOR((YEAR(in)*1000+MONTH(in))/&intsize)*&intsize  + 0.01*(in=>indate);
	__YEARIN = FLOOR(YEAR(in)/&intsize)*&intsize  + 0.01*(in=>indate);
	__AGEIN2 = FLOOR(agein/&intsize)*&intsize  + 0.01*(in=>indate);
	IF first.id THEN DO;
		__LASTMONTHIN = .;
		__LASTYEARIN = .;
		__LASTAGEIN = .;
	END;
	*first period to include in coarsened period;
	IF (&inttype="month" AND __MONTHIN NE __LASTMONTHIN) OR
		(&inttype="age" AND __AGEIN2 NE __LASTAGEIN) OR
	   (&inttype="year" AND __YEARIN NE __LASTYEARIN) 
		THEN DO i = 1 TO DIM(expR);
		expR[i] = 0;
		*cumR[i] = cum[i];
		wR[i] = w[i];
		__in=in;
		inOLD=in;
		__agein = agein;
	END;
	*sum exposure over periods, save the _in variable;
	ELSE IF (&inttype="month" AND __MONTHIN = __LASTMONTHIN) OR
		(&inttype="age" AND __AGEIN2 = __LASTAGEIN) OR
		(&inttype="year" AND __YEARIN = __LASTYEARIN) 
		THEN DO i = 1 TO DIM(expR);
		exp[i] = expR[i] + exp[i];
		*cum[i] = cumR[i] + exp[i];
		wR[i] = MAX(w[i], wR[i]);
		inOLD=in;
		in=__in;
		agein=__agein;
	END;

	DO j = 1 TO DIM(expR);
		expR[j] = exp[j];
		*cumR[j] = cum[j];
		wR[j] = w[j];
	END;
	__LASTMONTHIN = __MONTHIN;
	__LASTYEARIN = __YEARIN;
	__LASTAGEIN = __AGEIN2;



* merge time varying variables with baseline variables and outcome variables
* 
;
DATA __jk (KEEP=id d d: dob in out cumwl: wl: atrisk em:  race smoke: agestart ag: cohor: afe ale tsfe tsle __: indate priorwlm  ) 
	 lastobs(KEEP=id out tafe tale cum: d d_: race  );	
	LENGTH id in out wlm cumwlm wlm100 cumwlm100  
		%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); 
			wlm&I.lag cumwlm&I.lag wlm&I.lag100 cumwlm&I.lag100 
		%END;
		d d_lc d_copd d_cancer d_noncancer atrisk   tafe tale afe ale tsfe tsle cohort cohort2 priorwlm 8.;
	MERGE __TEMPTIME() vs02(KEEP=id outdate lc copd allcancer noncancer IN=inb) 
		  demo(KEEP = id indate sex race dob dlo race agestart smoke: cohor: )
		  baseline(IN=inbase);
	BY id;

	RETAIN TAFE TALE LASTWLM AFE ALE;
	IF first.id THEN DO;
		LASTWLM=0; AFE=.; ALE=.; TAFE=.; TALE=.;
	END;
	IF NOT inbase THEN priorwlm=0;
	wlm100 = wlm/100;
	cumwlm100 = cumwlm/100;
	%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); 
		wlm&I.lag100 = wlm&I.lag/100; 
		cumwlm&I.lag100 = cumwlm&I.lag/100; 
	%END;;
	*time since first/last exposure; *age at first, last exposure;
	IF (wlm > 0 AND lastwlm = 0) OR (first.id AND wlm>0) THEN tafe = in;
	IF (wlm = 0 AND lastwlm > 0) OR (last.id AND wlm>0) THEN tale = out;
	IF tsfe = . THEN tsfe = 0;
	IF tsle = . THEN tsle = 0;
	FORMAT tafe tale MMDDYY10.;

	IF tafe = inOLD THEN afe = agein;
	IF tale = out THEN ale = ageout;
	IF tale > . THEN tsle = YRDIF(tale, out+1,'ACT/ACT');
	IF tafe > . THEN tsfe = YRDIF(tafe, out+1,'ACT/ACT');

	IF in<=outdate<=out THEN DO; *4 individuals with in=out;
		d = 1; d_lc=0; d_copd=0; d_cancer=0; d_noncancer=0;
		IF lc = 1 THEN d_lc=1;
		ELSE IF copd = 1 THEN d_copd = 1;
		IF allcancer = 1 THEN d_cancer=1;
		IF noncancer = 1 THEN d_noncancer=1;
	END;
	ELSE DO;
		d = 0; d_lc=0; d_copd=0;d_cancer=0; d_noncancer=0;
	END;
	lastwlm = wlm;
	LABEL tafe = "Time at first exposure"
		 tale = "Time at last exposure"
		 afe = "Age at first exposure"
		 ale = "Age at last exposure"
		 tsfe = "Time since first exposure"
		 tsle = "Time since last exposure"
		 agein = "Age at start of period"
		 ageout = "Age at end of period"
		 in = "Time at start of period"
		 out = "Time at end of period"
		 wlm = "Unlagged radon exposure in period (WLM)"
		 cumwlm = "Unlagged cum radon exposure at end of period (WLM)"
		%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); 
			wlm&I.lag = "&I.-year lagged radon exposure in period (WLM)"
			cumwlm&I.lag = "&I.-year lagged cum radon exposure at end of period (WLM)"
		 	wlm&I.lag100 = "&I.-year lagged radon exposure in period (WLM100)"
		 	cumwlm&I.lag100 = "&I.-year lagged cum radon exposure at end of period (WLM100)"
		%END;
		 wlm100 = "Unlagged radon exposure in period (100 WLM)"
		 cumwlm100 = "Unlagged cum radon exposure at end of period (100 WLM)"
		 d = "Death in person period"
	     d_lc = "Death from respiratory cancer in period"
		 d_copd = "Death from COPD in period"
		 d_cancer = "Death from any cancer in period"
		 d_noncancer = "Death from any noncancer in period";
	FORMAT wlm cumwlm agein ageout afe ale tsfe tsle agestart wlm100 cumwlm100 
		   wlm: cumwlm: 6.2
		   d d_lc d_copd atrisk smoke:;
	OUTPUT __jk;
	IF last.id THEN OUTPUT lastobs;
RUN;


DATA &outdat(DROP = __: emp %DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); emp&i.lag %END;	
 	RENAME=(indate=datestart));
	SET __jk;
	RETAIN atwork %DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); atwork&i.lag %END;;
	%IF &inttype="month" %THEN %DO;
		BY id __MONTHIN;
		IF first.__MONTHIN THEN DO;
			atwork=MAX(__emp,0); 
			%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); 
				atwork&i.lag=MAX(__emp&i.lag,0);
			%END;;	
		END;
		IF __emp=1 AND atwork=0 THEN atwork=1; 
			%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); 
				IF __emp&i.lag=1 AND atwork&i.lag=0 THEN atwork&i.lag=1;
			%END;;	
		IF last.__MONTHIN;
	%END;
	%ELSE %IF &inttype="year" %THEN %DO;
		BY id __YEARIN;
		IF first.__YEARIN THEN DO;
			atwork=MAX(__emp,0); 
			%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); 
				atwork&i.lag=MAX(__emp&i.lag,0);
			%END;;	
		END;
		IF __emp=1 AND atwork=0 THEN atwork=1; 
			%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); 
				IF __emp&i.lag=1 AND atwork&i.lag=0 THEN atwork&i.lag=1;
			%END;;	
		IF last.__YEARIN;
	%END;
	%ELSE %IF &inttype="age" %THEN %DO;
		BY id __AGEIN2;
		IF first.__AGEIN2 THEN DO; 
			atwork=MAX(__emp,0); 
			%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); 
				atwork&i.lag=MAX(__emp&i.lag,0);
			%END;;	
		END;
		IF __emp=1 AND atwork=0 THEN atwork=1; 
			%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); 
				IF __emp&i.lag=1 AND atwork&i.lag=0 THEN atwork&i.lag=1;
			%END;;	
		IF last.__AGEIN2;
	%END;
	*person years in time perio;
	py=YRDIF(in, out+1,'ACT/ACT');
	*person years at risk in time period (person years, only if on study at that point);
	*should be equal at death for all period widths (i.e. monthly or annual data);
	IF indate < out THEN pyar=MIN(YRDIF(indate, out+1,'ACT/ACT'), YRDIF(in, out+1,'ACT/ACT'));
	ELSE pyar = 0;
	LABEL py = "Person-years in period"
		 pyar = "Person-years at risk in period"
		 atwork = "Active U mining work during period (y/n)"
			%DO j = 1 %TO &nlags; %LET i = %EVAL(%QSCAN(%STR(&lags),&j,%STR(,))); 
				atwork&i.lag = "Active U mining work during period, &i year lag (y/n)"
			%END;
		 indate = "Date at start of follow up";
	FORMAT  py pyar 6.2 atwork atwork: 1.0;
RUN;

%MEND;

*%REDUCEPMONTHS2(intsize=5, inttype="year", outdat=exp_5year);
*%REDUCEPMONTHS2(intsize=1, inttype="month", outdat=exp_1month);
*%REDUCEPMONTHS2(intsize=3, inttype="month", outdat=exp_3month);
*%REDUCEPMONTHS2(intsize=6, inttype="month", outdat=exp_6month);
%REDUCEPMONTHS2(intsize=1, inttype="age", outdat=exp_1yrage);
*%REDUCEPMONTHS2(intsize=2, inttype="age", outdat=exp_2yrage);




PROC MEANS DATA = lastobs FW=6 MAXDEC=2 MEAN STD P50 SUM;
	TITLE "cum variables";
	VAR cum:;
RUN;
PROC MEANS DATA = lastobs FW=6 MAXDEC=2 MEAN STD P50 SUM;
	TITLE "cum variables ";
	TITLE2 "alive on 1/1/1960";
	WHERE out >= '1jan1960'd;
	VAR cum:;
RUN;
PROC MEANS DATA = lastobs FW=6 MAXDEC=2 MEAN STD P50 SUM;
	CLASS race;
	WHERE out >= '1jan1960'd;
	VAR cum:;
RUN;

PROC FREQ DATA = lastobs;
	TITLE "Cohort mortality characteristics";
	TABLES race*(d d_lc d_copd d_cancer d_noncancer) / NOROW NOCOL;
RUN;
PROC FREQ DATA = lastobs;
	TITLE2 "alive on 1/1/1960";
	WHERE out >= '1jan1960'd;
	TABLES race*(d d_lc d_copd d_cancer d_noncancer) / NOROW NOCOL;
RUN;

DATA cumdiffs;
	SET lastobs;
	IF cumwlm NE cumwlm5lag;
RUN;

PROC PRINT DATA=cumdiffs;
	TITLE "Check these observations! Difference between cum exposure vars that shouldn't be there.";
	WHERE out>tale+365*5;*check for those with differences - ok!;
	VAR id;
RUN;

DATA checkdeadERR;
	MERGE lastobs(KEEP=id d d: OUT) out(KEEP=id outdate ic:);
	BY id;
	IF (d = 1 AND outdate=.) OR (d = 0 AND outdate>.z) THEN OUTPUT;
RUN;

*todo: more logic checks;
	
/*
*compare with historical data;
PROC IMPORT OUT =olddat(RENAME=(record=id))
	DATAFILE = "\\VBOXSVR/akeil/School/Dissertation/Cohorts/CPUM/Langholz/data/csv_data_CPUM Folder/uminers.csv"
	DBMS=CSV
	REPLACE;
RUN;

PROC SORT DATA = olddat;BY id;

PROC MEANS DATA = olddat N MIN MAX P50 MEAN STD MAXDEC=2 FW=6;
	TITLE "cum exposure data - old data";
	VAR totrdn priorex;
RUN;
*/

*data file with exposure, employment data at baseline;
DATA anpre;
	*SET exp_1month; *1 month per record;
	SET exp_1yrage; *1 month per record;
	IF pyar=0;*only person time BEFORE first interview;
DATA anprework (KEEP=id bl_:  );
	MERGE anpre(IN=ina) demo(KEEP=id IN=inb);
	BY id;
	RETAIN cumpy cumatwork cumatwork5lag cumatwork10lag;
	IF first.id THEN DO ;
		cumpy=py;
		cumatwork = py*atwork;
		cumatwork5lag = py*atwork5lag;
		cumatwork10lag = py*atwork10lag;
	END;
	ELSE DO;
		cumpy = cumpy+py;
		cumatwork = cumatwork + py*atwork;
		cumatwork5lag = cumatwork5lag + py*atwork5lag;
		cumatwork10lag = cumatwork10lag + py*atwork10lag;
	END;;
	IF inb AND NOT ina THEN DO;
		cumpy=0; cumatwork=0;cumatwork5lag=0;cumatwork10lag=0; 
		cumwlm=0;cumwlm5lag=0; cumwlm5lag100=0; cumwlm100=0;
	END;
	LABEL cumatwork="Cumulative person years at work, pre-enrollment"
		  cumatwork5lag="Cumulative person years at work, pre-enrollment, 5 year lag"
		  cumatwork10lag="Cumulative person years at work, pre-enrollment, 10 year lag"
		  cumwlm = "Cumulative exposure at work, pre-enrollment (WLM)"
		  cumwlm5lag = "Cumulative exposure at work, pre-enrollment, 5 year lag (WLM)"
		  cumwlm5lag100 = "Cumulative exposure at work, pre-enrollment, 5 year lag (100 WLM)"
		  cumwlm100 = "Cumulative exposure at work, pre-enrollment (100 WLM)";
	RENAME cumwlm=BL_cumwlm cumwlm5lag=BL_cumwlm5lag cumwlm5lag100=BL_cumwlm5lag100 cumwlm100=BL_cumwlm100 
		cumatwork=BL_cumatwork cumatwork5lag=BL_cumatwork5lag cumatwork10lag=BL_cumatwork10lag;
	IF last.id THEN OUTPUT;
RUN;


*write out sas data files;
DATA cpum.CPUM_dg01(LABEL="CPUM demographics data - 1 record per individual ");
	SET demo(WHERE=(ID NOT IN(3326, 1052, 989)));*excluded due to 10k Plus cum exposures;
	BY id;
/*
DATA cpum.CPUM_ex01 (LABEL="CPUM exposure data - 1 record per exposure period");
	SET hist(WHERE=(ID NOT IN(3326, 1052, 989)));
	BY id;
DATA cpum.CPUM_ex02(LABEL="CPUM exposure data - 1 record per month");
	SET monthly(WHERE=(ID NOT IN(3326, 1052, 989)));
	BY id;
	
DATA cpum.CPUM_vs01(LABEL="CPUM vital status data - 1 record per individual");
	SET out(WHERE=(ID NOT IN(3326, 1052, 989)));
	BY id;
RUN;
*/
DATA cpum.CPUM_vs02(LABEL="CPUM vital status data, miscellaneous causes - 1 record per individual");
	SET out(WHERE=(ID NOT IN(3326, 1052, 989)));
	BY id;
RUN;
/*
DATA cpum.CPUM_an01(LABEL="CPUM analysis data set - 1 record per person-month");
	MERGE exp_1month(WHERE=(ID NOT IN(3326, 1052, 989)));
	BY id;
RUN;

PROC CONTENTS DATA =cpum.CPUM_an01;
RUN;
*/
DATA cpum.CPUM_an02(LABEL="CPUM analysis data set - 1 record per year of age");
	MERGE exp_1yrage(WHERE=(ID NOT IN(3326, 1052, 989)));
	BY id;
RUN;
/*
DATA cpum.CPUM_an03(LABEL="CPUM analysis data set - 1 record per 5 years of age");
	MERGE exp_5year(WHERE=(ID NOT IN(3326, 1052, 989))) ;
	BY id;
RUN;

DATA cpum.CPUM_an03(LABEL="CPUM analysis data set - 1 record per 3 months of age");
	MERGE exp_3month(WHERE=(ID NOT IN(3326, 1052, 989))) ;
	BY id;
RUN;

DATA cpum.CPUM_an06(LABEL="CPUM analysis data set - 1 record per 6 months of age");
	MERGE exp_6month(WHERE=(ID NOT IN(3326, 1052, 989))) ;
	BY id;
RUN;

DATA cpum.CPUM_an07(LABEL="CPUM analysis data set - 1 record per 2 years of age");
	MERGE exp_2yrage(WHERE=(ID NOT IN(3326, 1052, 989)));
	BY id;
RUN;


DATA cpum.CPUM_an04(LABEL="CPUM analysis data set - 1 record per individual");
	SET exp_1month(DROP=in wl:  py pyar atrisk agein dlo atwork atwork5lag atwork10lag WHERE=(ID NOT IN(3326, 1052, 989)));
	BY id;
	IF last.id;
	LABEL afe = "Age at first exposure"
		 ale = "Age at last exposure"
		 tsfe = "Time since first exposure"
		 tsle = "Time since last exposure"
		 ageout = "Age at death/loss to follow up"
		 out = "Time at end of period"
		 cumwlm = "Unlagged cum radon exposure at death/loss to follow up (WLM)"
		 cumwlm5lag = "5-year lagged cum radon exposure at death/loss to follow up (WLM)"
		 cumwlm10lag = "10-year lagged cum radon exposure at death/loss to follow up (WLM)"
		 cumwlm100 = "Unlagged cum radon exposure at death/loss to follow up (100 WLM)"
		 cumwlm5lag100 = "5-year lagged cum radon exposure at death/loss to follow up (WLM100)"
		 cumwlm10lag100 = "10-year lagged cum radon exposure at death/loss to follow up (WLM100)"
		 d = "Death from any cause"
	     d_lc = "Death from respiratory cancer"
	     d_cancer = "Death from any cancer (LTAS)"
	     d_noncancer = "Death from any non-cancer (LTAS)"
		 d_copd = "Death from COPD";
RUN;
*/
DATA cpum.CPUM_bl01(LABEL="CPUM baseline employment data set - 1 record per individual");
	MERGE anprework(WHERE=(ID NOT IN(3326, 1052, 989))) hdate tdate;
	BY id;
RUN;

*do some maintenence;
PROC DATASETS LIBRARY=WORK;
	DELETE __:; 
QUIT;
/*
********************************************************************************;
*write out LTAS datafiles for use in SMR analysis;
FILENAME per2 "Z:\\EpiProjects\CPUM\data\cpum_ltas.per";
FILENAME out2 "Z:\\EpiProjects\CPUM\data\cpum_ltas.out";
FILENAME exp2 "Z:\\EpiProjects\CPUM\data\cpum_ltas.exp";
FILENAME expcum "Z:\\EpiProjects\CPUM\data\cpum_ltas_cum.exp";

DATA t1;
	SET demo;
	FORMAT _ALL_;
	FORMAT vs DA.;
	FILE per2;
	PUT @1 id 			Z9. 
		@12 sex			Z1. 
		@14 race		Z1. 
		@16 vs			$
		@18 dob			MMDDYY10.
		@29 indate 		MMDDYY10.
		@40 dlo			MMDDYY10.
		@51 smokelast	Z1.;
RUN;

DATA t2;
	SET hist;
	IF begdate < enddate;
	FORMAT _ALL_;
	FILE exp2;
	PUT @1 id 				Z9. 
		@12 begdate			MMDDYY10. 
		@23 enddate			MMDDYY10. 
		@34 wlmperday		BEST16.
		@52 cumwlm			BEST16.;
RUN;

DATA t4;
	SET monthly;
	FORMAT _ALL_;
	FILE expcum;
	PUT @1 id 				Z9. 
		@12 in				MMDDYY10. 
		@23 out				MMDDYY10. 
		@34 cumwlm			BEST16.;
RUN;

DATA t3;
	SET out;
	FORMAT _ALL_;
	FILE out2;
	PUT @1 id 					Z9. 
		@12 terminal		$	 
		@14 underlying		$	 
		@16 icd				$	 
		@28 outdate				MMDDYY10. ;
RUN;
*/

TITLE;FOOTNOTE;
RUN;QUIT;RUN;
/*DM ODSRESULTS 'clear;' CONTINUE; *clear ODS generated datasets;*/

