*clear the log window and the output window;
/**********************************************************************************************************************
* Author: Alex Keil
* Program: cpum_make_gfdata_20151031.sas
* Date: Friday, November 16, 2012 4:50:55 PM
* Project: Dissertation (colorado miners)
* Tasks: 
* Data in: CPUM_bl01.sas7bdat, CPUM_an02.sas7bdat
* Data out: all in Z:\\EpiProjects\CPUM\data
		SAS data files:
			CPUM_gf01.sas7bdat - Analysis file, for g-formula, 1 year of age per record
			

* Description: Read in raw data from colorado plateau u miners study from NIOSH, compare with historical data
* Keywords: CPUM
**********************************************************************************************************************/
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
PROC FORMAT CNTLIN="&data./raw_data_formats";

DATA cpum_gf01;
 MERGE cpum.cpum_an02 cpum.cpum_bl01;
 BY id;

DATA cpum.cpum_gf01;
SET cpum_gf01;
BY id;
 WHERE pyar>0;
 RETAIN cumatwork lastwork;
 IF first.id THEN DO; cumatwork=0; lastwork=1; END;
 cumatwork = cumatwork + atwork*py;
 IF NOT first.id AND lastwork=1 AND atwork=0 THEN leftwork=1; ELSE leftwork=0;
 OUTPUT;
 lastwork=atwork;
RUN;
