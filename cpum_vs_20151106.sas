/**********************************************************************************************************************
* Author: Alex Keil
* Program: cpum_vs_20151106.sas
* Date: Friday, November 6, 2015 at 9:02:59 AM
* Project: colorado plateau uranium miners
* Tasks: make a vital status file
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
FILENAME out "&data./raw/ltas_UR3_outcome_FOIA_201112.txt";

DATA out (DROP=tempflag month year icd3 terminal underlying WHERE=(ID NOT IN(3326, 1052, 989)));
	INFILE out;
	INPUT id 1-6 tempflag 10-11 terminal $ 17 underlying $ 19 icd $ 21-25 @@;
		IF tempflag NE 0 THEN DO; 
			INPUT @8 dod MMDDYY8.; 
		END;
		ELSE IF tempflag = 0 THEN DO;
			INPUT month 8-9 year 12-15;
		END;
	IF tempflag = 0 THEN dod = MDY(month+1*(month<12)-11*(month=12), 1, year+1*(month=12))-1;

	ICD3 = SUBSTR(icd, 1, 3);
	*ICD version;
	IF  	1949 <= YEAR(dod) <= 1957 THEN icdver=6;
	ELSE IF 1958 <= YEAR(dod) <= 1967 THEN icdver=7;
	ELSE IF 1968 <= YEAR(dod) <= 1978 THEN icdver=8;
	ELSE IF 1979 <= YEAR(dod) <= 1998 THEN icdver=9;
	ELSE IF 1999 <= YEAR(dod) <= 2006 THEN icdver=10; 
	
*SPECIFIC CAUSES OF DEATH;
	IF ICD NE "" THEN DO;
	*Malignant neoplasm of the trachea,bronchus, and lung;
	*hand checked against LTAS definitions - OK;
		IF icdver = 6 AND ICD3 IN ("162", "163") THEN y_lc = 1;
		ELSE IF icdver = 7 AND ICD IN ("162.0","162.1","162.8","163") THEN y_lc = 1;
		ELSE IF icdver = 8 AND ICD3 IN ("162") THEN y_lc = 1;
		ELSE IF icdver = 9 AND ICD3 IN ("162") THEN y_lc = 1;
		ELSE IF icdver = 10 AND ICD3 IN ("C33", "C34") THEN y_lc = 1;
		ELSE y_lc = 0;
		LABEL y_lc = "Lung cancer as terminal cause of death";	
	
	*COPD;
	*Hand checked against LTAS definitions - possible err0r in LTAS coding for ICD code 519.3;
	*update - 519.3 not in WHO handbook, but added in US as per NCHS, CDC;
		IF icdver = 6 AND ICD3 IN ("501", "502") OR icd = "527.1" THEN y_copd = 1;
		ELSE IF icdver = 7 AND (ICD3 IN ("501", "502") OR icd = "527.1") THEN y_copd = 1;
		ELSE IF icdver = 8 AND (ICD3 IN ("490", "491", "492") OR icd =  "519.3") THEN y_copd = 1;
		ELSE IF icdver = 9 AND ICD3 IN ("490", "491", "492", "496") THEN y_copd = 1;
		ELSE IF icdver = 10 AND ICD3 IN ("J40", "J41", "J42", "J43", "J44") THEN y_copd = 1;
		ELSE y_copd = 0;
		LABEL y_copd = "COPD as terminal cause of death";	
	END;

	LABEL terminal = "Indicator: cause of death is terminal"
		  underlying = "Indicator: cause of death is underlying"
		  icd = "ICD code active at time of death"
		  icdver = "ICD version active at time of death";
	FORMAT dod MMDDYY10.;
	y_any=1;
	LABEL y_any = "Death from any cause";
	IF icd3 = "" THEN DO;*assume alive unless noted;
     y_lc=0; y_copd=0;
	END;
RUN;

DATA cpum.vs0001; SET out;


RUN;QUIT;RUN;
/*DM ODSRESULTS 'clear;' CONTINUE; *clear ODS generated datasets;*/
