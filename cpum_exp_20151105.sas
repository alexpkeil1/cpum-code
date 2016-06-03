/**********************************************************************************************************************
* Author: Alex Keil
* Program: cpum_exp_20151105.sas
* Date: Thursday, November 5, 2015 at 11:11:34 PM
* Project: Colorado plateau uranium miners data
* Tasks:
* Data in: 
* Data out:
* Description: generating exposure files + demographics files
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


FILENAME demo "&data./raw/ltas_UR3_demo_FOIA_201112.txt";
FILENAME hist "&data./raw/ltas_UR3_hist_FOIA_201112.txt";
DATA demo (DROP=tempfla: dobm: doby: dlom: dloy: inm: iny: WHERE=(ID NOT IN(3326, 1052, 989)));
	INFILE demo;
	INPUT id 1-6 sex 8 race 10 vs 12 tempflag1 16-17 tempflag2 25-26 tempflag3 34-35 smokelast 41 @@;
		IF tempflag1 NE 0 THEN DO; 
			INPUT @14 dob MMDDYY8. @23 eligdate  MMDDYY8. @32 dlo MMDDYY8. @@; 
		END;
		ELSE IF tempflag1 = 0 THEN DO;
			INPUT dobmonth 14-15 dobyear 18-21 @@;
		END;
		IF tempflag2 NE 0 THEN DO; 
			INPUT @23 eligdate  MMDDYY8. @@; 
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
	IF tempflag2 = 0 THEN eligdate = MDY(inmonth+1*(inmonth<12)-11*(inmonth=12), 1, inyear+1*(inmonth=12))-1;*take last day in month;
	IF tempflag3 = 0 THEN dlo = MDY(dlomonth+1*(dlomonth<12)-11*(dlomonth=12), 1, dloyear+1*(dlomonth=12))-1;*take last day in month;
	IF eligdate > . THEN eligage=YRDIF(dob, eligdate+1,'ACT/ACT');
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
		  eligdate = "Start date in study"
		  dlo = "Date last observed"
		  eligage= "Age at first interview"
		  cohort = "Birth cohort"
		  cohort2 = "Birth cohort (collapsed)"
		  smoke_0 = "Never smoker"
		  smoke_1 = "Former smoker"
		  smoke_2 = "Current, <1 pack/day"
		  smoke_3 = "Current, 1 pack/day"
		  smoke_4 = "Current, >1 pack/day"
		  smoke3_1 = "Former smoker"
		  smoke3_2 = "Current smoker (any amount)";
	FORMAT dob eligdate dlo MMDDYY10. ;
RUN;
DATA hist(WHERE=(ID NOT IN(3326, 1052, 989)));
	INFILE hist;
	INPUT id 1-6 @8 datein MMDDYY8. @17 dateout MMDDYY8. wlmrt 26-40 emp 42; 
	FORMAT datein dateout MMDDYY10.;
RUN;
DATA exp_001( ) baseline(KEEP=id priorwlm) hdate(KEEP=id date_fe ) tdate(KEEP=id termdate );
 SET hist;
 BY id datein;
    RETAIN date_fe;
	FORMAT datein dateout MMDDYY10.;
	FORMAT datein dateout date_fe termdate MMDDYY10.;
	IF first.id THEN DO;
	 IF emp=1 THEN date_fe=datein;
	 ELSE date_fe=datein+1;
	 OUTPUT hdate;
	END;
	IF last.id THEN DO;
	 IF emp=1 THEN termdate=dateout;
	 ELSE termdate=datein+1;
	 OUTPUT tdate;
	END;
	priorwlm = (dateout=datein)*wlmrt;
	IF priorwlm>0 THEN OUTPUT baseline;
	IF dateout > datein THEN OUTPUT exp_001;
RUN;

DATA exp_001;
 MERGE exp_001 demo (KEEP=id dob eligdate dlo);
 BY id;
 eligage = YRDIF(dob, eligdate, 'ACT/ACT');

RUN;





*split at eligdate;

DATA exp_002 (DROP=old: PRIOR:);
 SET exp_001;
 BY id datein;
 RETAIN olddatein olddateout ;
 IF first.datein THEN DO;olddatein=datein; olddateout=dateout; END;
 IF datein>=eligdate OR dateout<eligdate THEN OUTPUT;
 ELSE DO; 
  IF datein < eligdate AND dateout>=eligdate THEN DO;
   dateout = eligdate - 1;
   OUTPUT;
   datein = eligdate; dateout=olddateout;
   OUTPUT;
  END;
 END;
RUN;
DATA exp_002(DROP=date_fe termdate termage) ;
 SET exp_002;
 BY id;
 RETAIN  cumwlm cum_yrs_exposed;
 	IF first.id THEN DO; cumwlm=0; cum_yrs_exposed=0; END;
    agein = YRDIF(dob, datein, 'ACT/ACT');
    ageout = YRDIF(dob, dateout, 'ACT/ACT');
	IF datein >= eligdate THEN atrisk = 1;
	ELSE if datein < eligdate THEN atrisk=0;
	wlm = (wlmrt)*(dateout-datein+1)*emp;
	cumwlm = cumwlm + wlm;
	yrs_exposed = YRDIF(datein, dateout,'ACT/ACT');;
	cum_yrs_exposed = SUM(cum_yrs_exposed,yrs_exposed);
	LABEL wlm = "Rad wlm in period, U mining only (WLM)"
		  date_fe = "First recorded exposure history record"
		  datein="Begin date for exposure history record"
		  dateout="End date for exposure history record"
		  wlmrt = "Exposure rate (WLM/per day)"
		  emp = "Employed during period"
          termdate = "Last date of employment";
	OUTPUT exp_002;
RUN;

DATA demo; 
 MERGE demo tdate hdate;
 BY id;
 IF date_fe>.z THEN age_fe = YRDIF(dob, date_fe, 'ACT/ACT');
 termage = YRDIF(dob, termdate, 'ACT/ACT');
   LABEL termage = "Age of last employment"
         age_fe = "Age of first recorded exposure";
RUN;


DATA temp;
 SET exp_002;
 ageinf = FLOOR(AGEIN);
 ageoutc = CEIL(ageout);
PROC MEANS DATA = temp NOPRINT;
 VAR datein dateout ageinF ageoutC;
 OUTPUT OUT=minmax MIN=inmin drop1 inmin2 drop3 MAX=drop2 outmax drop4 outmax2;
RUN;
DATA _NULL_;
 SET minmax;
 CALL SYMPUT('agebegin', PUT((inmin2), z3.0));
 CALL SYMPUT('ageend', PUT((outmax2), z3.0));
DATA _NULL_; in=&agebegin; out=&ageend; PUT in= out=;RUN; 

DATA exp_anywide(DROP=flag extratime old:) extratime(KEEP=id age_fe eligage extratime);; 
 MERGE exp_002 DEMO(keep = id termage age_fe date_fe) ;
*fixes an apparent error in the pystart variable, where it starts (sometimes) long before exposure; 
 BY id; 
 IF first.id AND eligage<age_fe THEN DO;
  extratime = age_fe-eligage;
  OUTPUT extratime;
  eligage=age_fe;
  eligdate = date_fe;
 END;
 IF agein < age_fe AND atrisk = 1 THEN DO;
  IF FLOOR(agein) = FLOOR(age_fe) THEN DO;
   atrisk=0;
   olddateout=dateout;
   wlm = (wlmrt)*(dateout-datein+1)*emp;
   dateout = date_fe-1;
   ageout = YRDIF(dob, dateout,'ACT/ACT');
   oldwlm = wlm;
   wlm = (wlmrt)*(dateout-datein+1);
   cumwlm = cumwlm-(oldwlm-wlm);
   yrs_exposed = (ageout-agein)*wlm>0;
   OUTPUT EXP_ANYWIDE;
   agein=age_fe;
   datein=date_fe;
   dateout=olddateout;
   ageout = YRDIF(dob, dateout,'ACT/ACT');
   wlm = (wlmrt)*(dateout-datein+1);
   atrisk=1;
   cumwlm = cumwlm + wlm;
   yrs_exposed = (ageout-agein)*wlm>0;
   OUTPUT EXP_ANYWIDE;
  END;
 END;
 ELSE OUTPUT EXP_ANYWIDE;
RUN;

PROC MEANS DATA = extratime SUM N MAX;
 VAR extratime; *566 person years among 420 people;
RUN;

DATA exp_eligwide (DROP=datein dateout wlmrt emp wlm dob eligdate dlo agein ageout age_fe eligage
                       err year inc incbl yrs_exposed cum_yrs_exposed termage atrisk date_fe) err(KEEP=err id agein ageout datein dateout wlmrt )
      /*test*/;
 LENGTH id cumwlm cumwlm cumyrsexp cum_yrs_exposed BL_cumwlm BL_cumyrsexp 8;
 LENGTH err 8; *simple error handling;

 SET exp_anywide;
 ARRAY annwlm[&agebegin:&ageend] annwlm&agebegin-annwlm&ageend;
 ARRAY anyexp[&agebegin:&ageend] anyexp&agebegin-anyexp&ageend;
 ARRAY pyexp[&agebegin:&ageend] pyexp&agebegin-pyexp&ageend;

 BY id datein;
 RETAIN BL_cumwlm BL_cumyrsexp cumwlm cumyrsexp cumyrsatwork anyexp&agebegin-anyexp&ageend pyexp&agebegin-pyexp&ageend annwlm&agebegin-annwlm&ageend ;
 IF first.id THEN DO;
 cumwlm=0;
 cumyrsexp=0;
 cumyrsatwork=0;
 BL_cumwlm=0;
 BL_cumyrsexp=0;
  DO year = &agebegin TO &ageend;
   pyexp[year]=0;
   annwlm[year]=0;
   anyexp[year]=0;
  END;
 END;*first.id;
 DO year = &agebegin TO &ageend;
  IF FLOOR(agein) <= year AND FLOOR(ageout) => year 
   THEN DO; *in exposure period;
   *any exposure;
   anyexp[year] = 1;
   *days of exposure in the year;
   IF atrisk THEN DO; *whole period eligible (due to previously splitting records by eligdate);
    IF agein <= year AND ageout >= year+.999999 THEN inc =  1; *whole year;
    ELSE IF agein > year AND ageout >= year+.999999 THEN inc = (year+1 - agein) /*+ 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1)*/; *last part of year only;
    ELSE IF agein <= year AND ageout < year+.999999 THEN inc = (ageout - year) +  1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *first part of year only;
    ELSE IF agein > year AND ageout < year+.999999 THEN inc =  (ageout - agein) + 1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *mid-year year;
    ELSE IF agein = year AND ageout = agein THEN inc = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    ELSE IF agein = year + 1-1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1) AND ageout = agein THEN inc = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    incbl=0;
   END;
   IF atrisk=0 THEN DO; *non of the period eligible (should be no partials);
    IF agein <= year AND ageout >= year+.999999 THEN incbl =  1; *whole year;
    ELSE IF agein > year AND ageout >= year+.999999 THEN incbl = (year+1 - agein) /*+ 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1)*/; *last part of year only;
    ELSE IF agein <= year AND ageout < year+.999999 THEN incbl = (ageout - year) +  1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *first part of year only;
    ELSE IF agein > year AND ageout < year+.999999 THEN incbl =  (ageout - agein) + 1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *mid-year year;
    ELSE IF agein = year AND ageout = agein THEN incbl = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    ELSE IF agein = year + 1-1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1) AND ageout = agein THEN incbl = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    inc=0;
   END;
   IF agein < eligage < ageout THEN ERR=4;
    BL_cumyrsexp =  BL_cumyrsexp + incbl;
    BL_cumwlm =  BL_cumwlm + wlmrt * incbl*(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1);
    pyexp[year] = pyexp[year] + inc ;
    annwlm[year] = annwlm[year] + wlmrt * inc*(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *close enough;
  END; *end exposure in year;
 END;
 cumyrsexp = SUM(OF pyexp[*]);
 cumwlm = SUM(OF annwlm[*]);
 IF ageout=agein THEN err=0;
 IF ageout<agein THEN err=1;
 IF agein<&agebegin THEN err=2;
 IF agein>&ageend OR ageout>&ageend THEN err=3;
 IF last.id THEN OUTPUT exp_eligwide;
 IF err>.z THEN OUTPUT err;
 *OUTPUT TEST;
RUN;

DATA exp_allwide (DROP=datein dateout wlmrt emp wlm  dob eligdate dlo agein ageout  age_fe eligage
                       err year inc yrs_exposed cum_yrs_exposed cumwlm) err(KEEP=err id agein ageout datein dateout wlmrt );
  LENGTH id cumwlm cumwlm cumyrsexp cum_yrs_exposed BL_cumwlm BL_cumyrsexp 8;
 LENGTH err 8; *simple error handling;

 SET exp_anywide;
 ARRAY annwlm[&agebegin:&ageend] annwlm&agebegin-annwlm&ageend;
 ARRAY anyexp[&agebegin:&ageend] anyexp&agebegin-anyexp&ageend;
 ARRAY pyexp[&agebegin:&ageend] pyexp&agebegin-pyexp&ageend;

 BY id datein;
 RETAIN BL_cumwlm BL_cumyrsexp cumwlm cumyrsexp cumyrsatwork anyexp&agebegin-anyexp&ageend pyexp&agebegin-pyexp&ageend annwlm&agebegin-annwlm&ageend ;
 IF first.id THEN DO;
 cumwlm=0;
 cumyrsexp=0;
 cumyrsatwork=0;
 BL_cumwlm=0;
 BL_cumyrsexp=0;
  DO year = &agebegin TO &ageend;
   pyexp[year]=0;
   annwlm[year]=0;
   anyexp[year]=0;
  END;
 END;*first.id;
 DO year = &agebegin TO &ageend;
  IF FLOOR(agein) <= year AND FLOOR(ageout) => year 
   THEN DO; *in exposure period;
   *any exposure;
   anyexp[year] = 1;
   *days of exposure in the year;
   IF atrisk THEN DO; *whole period eligible (due to previously splitting records by eligdate);
    IF agein <= year AND ageout >= year+.999999 THEN inc =  1; *whole year;
    ELSE IF agein > year AND ageout >= year+.999999 THEN inc = (year+1 - agein) /*+ 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1)*/; *last part of year only;
    ELSE IF agein <= year AND ageout < year+.999999 THEN inc = (ageout - year) +  1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *first part of year only;
    ELSE IF agein > year AND ageout < year+.999999 THEN inc =  (ageout - agein) + 1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *mid-year year;
    ELSE IF agein = year AND ageout = agein THEN inc = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    ELSE IF agein = year + 1-1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1) AND ageout = agein THEN inc = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    incbl=0;
   END;
   IF atrisk=0 THEN DO; *non of the period eligible (should be no partials);
    IF agein <= year AND ageout >= year+.999999 THEN incbl =  1; *whole year;
    ELSE IF agein > year AND ageout >= year+.999999 THEN incbl = (year+1 - agein) /*+ 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1)*/; *last part of year only;
    ELSE IF agein <= year AND ageout < year+.999999 THEN incbl = (ageout - year) +  1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *first part of year only;
    ELSE IF agein > year AND ageout < year+.999999 THEN incbl =  (ageout - agein) + 1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *mid-year year;
    ELSE IF agein = year AND ageout = agein THEN incbl = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    ELSE IF agein = year + 1-1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1) AND ageout = agein THEN incbl = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    inc=incbl;
   END;
   IF agein < eligage < ageout THEN ERR=4;
    BL_cumyrsexp =  BL_cumyrsexp + incbl;
    BL_cumwlm =  BL_cumwlm + wlmrt * incbl*(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1);
    pyexp[year] = pyexp[year] + inc ;
    annwlm[year] = annwlm[year] + wlmrt * inc*(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *close enough;
  END; *end exposure in year;
 END;
 cumyrsexp = SUM(OF pyexp[*]);
 cumwlm = SUM(OF annwlm[*]);
 IF ageout=agein THEN err=0;
 IF ageout<agein THEN err=1;
 IF agein<&agebegin THEN err=2;
 IF agein>&ageend OR ageout>&ageend THEN err=3;
 IF last.id THEN OUTPUT exp_allwide;
 IF err>.z THEN OUTPUT err;
 *OUTPUT TEST;
RUN;

DATA exp_blwide (DROP=datein dateout wlmrt emp wlm  dob eligdate dlo agein ageout  age_fe eligage
                       err year inc yrs_exposed cum_yrs_exposed cumwlm) err(KEEP=err id agein ageout datein dateout wlmrt );
  LENGTH id cumwlm cumwlm cumyrsexp cum_yrs_exposed BL_cumwlm BL_cumyrsexp 8;
 LENGTH err 8; *simple error handling;

 SET exp_anywide;
 ARRAY annwlm[&agebegin:&ageend] annblwlm&agebegin-annblwlm&ageend;
 ARRAY anyexp[&agebegin:&ageend] anyblexp&agebegin-anyblexp&ageend;
 ARRAY pyexp[&agebegin:&ageend] pyblexp&agebegin-pyblexp&ageend;

 BY id datein;
 RETAIN BL_cumwlm BL_cumyrsexp cumwlm cumyrsexp cumyrsatwork anyblexp&agebegin-anyblexp&ageend pyblexp&agebegin-pyblexp&ageend annblwlm&agebegin-annblwlm&ageend ;
 IF first.id THEN DO;
 cumwlm=0;
 cumyrsexp=0;
 cumyrsatwork=0;
 BL_cumwlm=0;
 BL_cumyrsexp=0;
  DO year = &agebegin TO &ageend;
   pyexp[year]=0;
   annwlm[year]=0;
   anyexp[year]=0;
  END;
 END;*first.id;
 DO year = &agebegin TO &ageend;
  IF FLOOR(agein) <= year AND FLOOR(ageout) => year 
   THEN DO; *in exposure period;
   *any exposure;
   anyexp[year] = 1;
   *days of exposure in the year;
   IF atrisk THEN DO; *whole period eligible (due to previously splitting records by eligdate);
    IF agein <= year AND ageout >= year+.999999 THEN inc =  1; *whole year;
    ELSE IF agein > year AND ageout >= year+.999999 THEN inc = (year+1 - agein) /*+ 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1)*/; *last part of year only;
    ELSE IF agein <= year AND ageout < year+.999999 THEN inc = (ageout - year) +  1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *first part of year only;
    ELSE IF agein > year AND ageout < year+.999999 THEN inc =  (ageout - agein) + 1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *mid-year year;
    ELSE IF agein = year AND ageout = agein THEN inc = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    ELSE IF agein = year + 1-1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1) AND ageout = agein THEN inc = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    incbl=0;
   END;
   IF atrisk=0 THEN DO; *non of the period eligible (should be no partials);
    IF agein <= year AND ageout >= year+.999999 THEN incbl =  1; *whole year;
    ELSE IF agein > year AND ageout >= year+.999999 THEN incbl = (year+1 - agein) /*+ 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1)*/; *last part of year only;
    ELSE IF agein <= year AND ageout < year+.999999 THEN incbl = (ageout - year) +  1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *first part of year only;
    ELSE IF agein > year AND ageout < year+.999999 THEN incbl =  (ageout - agein) + 1/(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *mid-year year;
    ELSE IF agein = year AND ageout = agein THEN incbl = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    ELSE IF agein = year + 1-1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1) AND ageout = agein THEN incbl = 1/(MDY(12,31, year(datein))-MDY(1,1, year(datein)) + 1);*single day;
    inc=0;
   END;
   inc=incbl;
   IF agein < eligage < ageout THEN ERR=4;
    *BL_cumyrsexp =  BL_cumyrsexp + incbl;
    *BL_cumwlm =  BL_cumwlm + wlmrt * incbl*(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1);
    pyexp[year] = pyexp[year] + inc ;
    annwlm[year] = annwlm[year] + wlmrt * inc*(MDY(12,31, year(dateout))-MDY(1,1, year(dateout)) + 1); *close enough;
  END; *end exposure in year;
 END;
 cumyrsexp = SUM(OF pyexp[*]);
 cumwlm = SUM(OF annwlm[*]);
 IF ageout=agein THEN err=0;
 IF ageout<agein THEN err=1;
 IF agein<&agebegin THEN err=2;
 IF agein>&ageend OR ageout>&ageend THEN err=3;
 IF last.id THEN OUTPUT exp_blwide;
 IF err>.z THEN OUTPUT err;
 *OUTPUT TEST;
RUN;


DATA exp_003;
 SET exp_002;
 OUTPUT exp_003;

DATA cpum.demo0001; MERGE demo hdate tdate baseline; BY id; IF priorwlm=. THEN priorwlm=0; RUN;
DATA cpum.hist0001; SET hist;
DATA cpum.exp0001; SET exp_003;
DATA cpum.exp0101; SET exp_allwide;
DATA cpum.exp0102; SET exp_blwide;
DATA cpum.exp0111; SET exp_eligwide; RUN;

/*PROC MEANS DATA = exp_anywide;
 VAR cum:;RUN;*/


RUN;QUIT;RUN;
/*DM ODSRESULTS 'clear;' CONTINUE; *clear ODS generated datasets;*/
