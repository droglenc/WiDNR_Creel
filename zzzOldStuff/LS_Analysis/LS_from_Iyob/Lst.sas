OPTIONS PAGESIZE=118 LINESIZE=132;
OPTIONS YEARCUTOFF=1950;
*LASER PRINTER SETTINGS:  FONT=108  LINES=120/PAGE;

**********************************************************************;
*                                                                     ;
*   PROGRAM TO ANALYZE "BUS ROUTE" TYPE LAKE SUPERIOR CREEL           ;
*     SINGLE ROUTE  -   INTEGRATED EFFORT AT LANDING COUNT            ;
*  NOTE:  THE COUNTS FOR LAKE SUPERIOR ARE AVERAGE NUMBER OF          ;
*         PARTIES PRESENT DURING THE WAIT TIME - NOT TOTAL EFFORT     ;
*         SEEN DURING THE WAIT TIME.                                  ;
*                                                                     ;
*         VERSION 91.1         JULY, 1991                             ;
*                                                                     ;
*  NOTE:  ONLY OFFICIAL HOLIDAYS ARE NEW YEARS, MEMORIAL DAY,         ;
*  4TH OF JULY, AND LABOR DAY (THANKSGIVING AND X-MAS NOT INCLUDED)   ;
*                                                                     ;
*  TO RUN:  FILL IN APPROPRIATE INFORMATION AND FILENAMES             ;
*                                                                     ;
**********************************************************************;

LIBNAME SAVE 'C:\Iyob-DNR\Lake Superior Creel';     *DIRECTORY WHERE SAS DATA FILES STORED;
%LET COUNTS=supCNTS;                         *NAME OF COUNTS SAS FILE;
%LET INTS=supINTS;                         *NAME OF INTERVIEW SAS FILE;
%LET NAME=LAKE SUPERIOR -sup;  *SURVEY NAME;
%LET SDATE=05/21/14;                *START DATE OF ANALYSIS;
%LET FDATE=09/30/14;                             *END DATE OF ANALYSIS;
%LET JAN=00;                          *SET CORRECT FISHING DAY LENGTHS;
%LET FEB=00;
%LET MAR=00;
%LET APR=00;
%LET MAY=16;
%LET JUN=16;
%LET JUL=16;
%LET AUG=16;
%LET SEP=16;
%LET OCT=00;
%LET NOV=00;
%LET DEC=00;

**********  NO CHANGES IN PROGRAM NEEDED AFTER THIS LINE  *************;
TITLE CREEL SURVEY ANALYSIS: &NAME    &SDATE - &FDATE;

PROC FORMAT;
VALUE MON   1='JANUARY' 2='FEBRUARY' 3='MARCH' 4='APRIL' 5='MAY'
            6='JUNE' 7='JULY' 8='AUGUST' 9='SEPTEMBER'
            10='OCTOBER' 11='NOVEMBER' 12='DECEMBER' 99='TOTAL';
VALUE  DAYTYPE 1='WEEKDAY' 2='WEEKEND' 9='TOTAL';
VALUE  FISHERY 1='COLDWATER-OPEN'
               2='WARMWATER-OPEN'
               3='ICE-STREAM MOUTH'
               4='ICE-WARMWATER'
               5='ICE-BOBBING'
               6='BAD RIVER'
               7='NON-FISHING'
			   8='SHORE'
			   9='TRIBAL'
               99='COMBINED';
VALUE  STATE  1='WI' 2='MN' 3='MI' 4='WI/MN' 5='WI/MI';
VALUE STATUS 1='COMPLETE' 2='INCOMPLETE';
VALUE  RES 1='RESIDENT' 2='NONRESIDENT' 3='RES/NONRES';
VALUE $SPECIES   'B01','062'='STURGEON'
                 'I04','033'='LAKE HERRING'
                 'I05','070'='LAKE WHITEFISH'
                 'I12','051'='PINK SALMON'
                 'I14','049'='COHO SALMON'
                 'I16','048'='CHINOOK'
                 'I18','071'='ROUND WHITEFISH'
                 'I19','046'='RAINBOW TROUT'
                 'I20','052'='ATLANTIC SALMON'
                 'I21','045'='BROWN TROUT'
                 'I22','006'='BROOK TROUT'
                 'I23','035'='LAKE TROUT'
                 'I24','034'='SISCOWET'
                 'I28','036'='SPLAKE'
                 'J01','060'='SMELT'
                 'L02','043'='NORTHERN PIKE'
                 'M12','013'='CARP'
                 'R01','011'='BURBOT'
                 'W00','003'='SUNFISH SPP.'
                 'W02','004'='CRAPPIE SPP.'
                 'W04','047'='ROCK BASS'
                 'W06','007'='PUMPKINSEED'
                 'W09','002'='BLUEGILL'
                 'W11','067'='SMALLMOUTH BASS'
                 'W12','066'='LARGEMOUTH BASS'
                 'W14','005'='BLACK CRAPPIE'
                 'X15','076'='YELLOW PERCH'
                 'X22','078'='WALLEYE'
					   '098'=catfish;
VALUE FINCLIPA   0='00: NONE'
                 1='01: AD'
                 2='02: AD+LV'
                 3='03: AD+RV'
                 4='04: AD+LP'
                 5='05: AD+RP'
                 6='06: LV'
                 7='07: RV'
                 8='08: LP'
                 9='09: RP'
                10='10: LV+RV'
                11='11: RP+LV'
                12='12: AD+LV+RV'
                13='13: D'
                14='14: HATCHERY'
                15='15: LP+RV'
                16='16: D+RV'
                17='17: D+RP'
                18='18: AD+RM'
                19='19: LP+RM'
                20='20: LP+LV'
                21='21: D+AD'
                22='22: D+LV+RV'
                23='23: D+LP'
                24='24: D+LV'
                40='40: NOT EXAMINED';
VALUE FINCLIPB  0='NO FINCLIP'
                1-24='FINCLIP';

* CREATES A SAS DATA FILE = CALENDAR, THAT CONTAINS THE NUMBER OF
  WEEKEND/HOLIDAYS (CODE=2) AND WEEKDAYS (CODE=1) IN EACH MONTH.;

DATA CALENDAR;
LENGTH DATE MONTH DAYTYPE 4;
DATE1=INPUT(SYMGET('SDATE'),MMDDYY8.);
DATE2=INPUT(SYMGET('FDATE'),MMDDYY8.);
DO DATE=DATE1 TO DATE2;                    * GENERATES SURVEY DATES;
  MONTH=MONTH(DATE);
  DAY=DAY(DATE);
  WDAY=WEEKDAY(DATE);
  IF WDAY=1 OR WDAY=7 THEN DAYTYPE=2; ELSE DAYTYPE=1;
  IF MONTH=1 AND DAY=1 THEN DAYTYPE=2;               * NEW YEARS DAY;
  IF MONTH=5 AND 25<=DAY<=31 AND WDAY=2 THEN DAYTYPE=2;     *MEM DAY;
  IF MONTH=7 AND DAY=4 THEN DAYTYPE=2;                    * JULY 4TH;
  IF MONTH=9 AND 1<=DAY<=7 AND WDAY=2 THEN DAYTYPE=2;     *LABOR DAY;
  KEEP DATE MONTH DAYTYPE;
  OUTPUT;
END;
FORMAT MONTH MON. DAYTYPE DAYTYPE.;

PROC SUMMARY NWAY;
CLASSES MONTH DAYTYPE;
VAR DATE;
OUTPUT OUT=CALENDAR N=DAYS;

DATA CALENDAR;
SET CALENDAR;
LENGTH DAYLEN 4;
IF MONTH=1 THEN DAYLEN=INPUT(SYMGET('JAN'),4.0);
IF MONTH=2 THEN DAYLEN=INPUT(SYMGET('FEB'),4.0);
IF MONTH=3 THEN DAYLEN=INPUT(SYMGET('MAR'),4.0);
IF MONTH=4 THEN DAYLEN=INPUT(SYMGET('APR'),4.0);
IF MONTH=5 THEN DAYLEN=INPUT(SYMGET('MAY'),4.0);
IF MONTH=6 THEN DAYLEN=INPUT(SYMGET('JUN'),4.0);
IF MONTH=7 THEN DAYLEN=INPUT(SYMGET('JUL'),4.0);
IF MONTH=8 THEN DAYLEN=INPUT(SYMGET('AUG'),4.0);
IF MONTH=9 THEN DAYLEN=INPUT(SYMGET('SEP'),4.0);
IF MONTH=10 THEN DAYLEN=INPUT(SYMGET('OCT'),4.0);
IF MONTH=11 THEN DAYLEN=INPUT(SYMGET('NOV'),4.0);
IF MONTH=12 THEN DAYLEN=INPUT(SYMGET('DEC'),4.0);
LABEL DAYLEN='ASSUMED FISHING DAY LENGTH';

PROC TABULATE FORMAT=8.0;
TITLE2 'NUMBER OF DAYS OF EACH DAYTYPE FOR SURVEY PERIOD AND ASSUMED FISHING DAY LENGTH';
TITLE3 'THESE NUMBERS WILL BE USED AS EXPANSION FACTORS';
CLASSES MONTH DAYTYPE;
VAR DAYS DAYLEN;
TABLE MONTH ALL, (DAYTYPE ALL)*DAYS ALL*DAYLEN*MEAN/RTS=20;
KEYLABEL SUM=' ' MEAN=' ';
run;

*READ IN INTERVIEW DATA FROM EXISTING SAS DATA SET;

DATA INTS;
SET SAVE.&INTS;                    *CREATE SAS DATE AND TIME VARIABLES;
DATE=MDY(MONTH,DAY,YEAR);
START=HMS(STARTHH,STARTMM,0);
STOP=HMS(STOPHH,STOPMM,0);
RETAIN DATEA DATEB;
IF _N_=1 THEN DO;
  DATEA=INPUT(SYMGET('SDATE'),MMDDYY8.);
  DATEB=INPUT(SYMGET('FDATE'),MMDDYY8.);
END;
IF DATEA <= DATE <= DATEB;                *KEEP INTS FROM SURVEY PERIOD;
IF START=. OR STOP=. THEN DELETE;
IF START>STOP THEN DELETE;   *#added by IYOB;
HOURS=(STOP-START)/3600;                  *TOTAL TIME AWAY FROM LANDING;
FORMAT MONTH MON. DAYTYPE DAYTYPE. FISHERY FISHERY. STATE STATE.;
KEEP DATE MONTH DAYTYPE FISHERY PERSONS HOURS STATE SPEC1-SPEC28 CLIP1-CLIP28 LEN1-LEN28 STATUS;
run;

PROC TABULATE FORMAT=4.0;
TITLE2 'NUMBER OF INTERVIEWS AND ACTUAL AMOUNT OF INTERVIEWED EFFORT BY VARIOUS STRATA';
CLASSES MONTH DAYTYPE FISHERY STATE;
VAR HOURS;
TABLE STATE ALL,(DAYTYPE ALL)*(FISHERY ALL),(MONTH ALL)*HOURS*(N SUM*F=COMMA6.0)/
RTS=30 MISSTEXT='0' CONDENSE BOX=_PAGE_;
KEYLABEL ALL='TOTAL';
run;

PROC SUMMARY NWAY;               *SUM TOTAL INTERVIEWED PARTY HOURS;
CLASSES MONTH DAYTYPE;
VAR HOURS;
OUTPUT OUT=NINTS SUM=THOURS;
run;

DATA NINTS;
SET NINTS;
KEEP MONTH DAYTYPE THOURS;
run;

DATA EFFORT;
SET INTS;
KEEP MONTH DAYTYPE FISHERY WATERS HOURS INDHRS CHOURS;
INDHRS=PERSONS*HOURS;
IF STATUS=1 THEN CHOURS=HOURS; ELSE CHOURS=.;
LENGTH WATERS $ 13;
IF STATE=1 THEN DO;
  WATERS='WISCONSIN';
  OUTPUT;
END;
IF STATE=2 OR STATE=3 THEN DO;
  WATERS='NON-WISCONSIN';
  OUTPUT;
END;
IF STATE=4 OR STATE=5 THEN DO;
  HOURS=HOURS/2;
  CHOURS=CHOURS/2;
  INDHRS=INDHRS/2;
  WATERS='WISCONSIN';
  OUTPUT;
  WATERS='NON-WISCONSIN';
  OUTPUT;
END;

PROC SUMMARY NWAY;
CLASSES MONTH DAYTYPE WATERS FISHERY;
VAR HOURS INDHRS CHOURS;
OUTPUT OUT=EFFORT SUM= MEAN(CHOURS)=MTRIP N(HOURS)=N USS(HOURS)=VHOURS;
run;

DATA EFFORT;
MERGE NINTS EFFORT;
BY MONTH DAYTYPE;
PROP=HOURS/THOURS;    *PERCENT OF INTERVIEWED EFFORT IN THIS STRATA;
PARTY=INDHRS/HOURS;   *PERSONS PER PARTY IN THIS STRATA;
KEEP MONTH DAYTYPE WATERS FISHERY PARTY MTRIP PROP HOURS N VHOURS;
run;


* READ COUNT FILE, ASSUMES THAT ALL MISSING VALUES ARE REALLY ZEROS;

DATA COUNTS;
SET SAVE.&COUNTS;
DATE=MDY(MONTH,DAY,YEAR);
START=HMS(STARTHH,STARTMM,0);
STOP=HMS(STOPHH,STOPMM,0);
RETAIN DATEA DATEB;
IF _N_=1 THEN DO;
  DATEA=INPUT(SYMGET('SDATE'),MMDDYY8.);
  DATEB=INPUT(SYMGET('FDATE'),MMDDYY8.);
END;
IF DATEA <= DATE <= DATEB;            *KEEP COUNTS FROM SURVEY PERIOD;
IF COUNT=. THEN COUNT=0;
IF START<STOP THEN WAIT=(STOP-START)/3600;   *CALCULATE WAIT TIME;
ELSE WAIT=(STOP+24*3600-START)/3600;
* THE AVERAGE COUNTS ARE TRANSFORMED TO TOTAL EFFORT DURING SHIFT;
* SO THAT MULTIPLE SHIFTS ON EACH DAY CAN BE COMBINED;
COUNT=COUNT*WAIT;
FORMAT MONTH MON. DAYTYPE DAYTYPE.;
run;

PROC SUMMARY NWAY;         *COMBINES MULTIPLE COUNTS FOR EACH DAY;
CLASSES DATE SITE;
ID MONTH DAYTYPE;
VAR COUNT WAIT;
OUTPUT OUT=COUNTS SUM=;
run;

DATA COUNTS;                      *EXPAND COUNTS TO ENTIRE STRATA;
SET COUNTS;
IF MONTH=1 THEN DAYLEN=INPUT(SYMGET('JAN'),4.0);
IF MONTH=2 THEN DAYLEN=INPUT(SYMGET('FEB'),4.0);
IF MONTH=3 THEN DAYLEN=INPUT(SYMGET('MAR'),4.0);
IF MONTH=4 THEN DAYLEN=INPUT(SYMGET('APR'),4.0);
IF MONTH=5 THEN DAYLEN=INPUT(SYMGET('MAY'),4.0);
IF MONTH=6 THEN DAYLEN=INPUT(SYMGET('JUN'),4.0);
IF MONTH=7 THEN DAYLEN=INPUT(SYMGET('JUL'),4.0);
IF MONTH=8 THEN DAYLEN=INPUT(SYMGET('AUG'),4.0);
IF MONTH=9 THEN DAYLEN=INPUT(SYMGET('SEP'),4.0);
IF MONTH=10 THEN DAYLEN=INPUT(SYMGET('OCT'),4.0);
IF MONTH=11 THEN DAYLEN=INPUT(SYMGET('NOV'),4.0);
IF MONTH=12 THEN DAYLEN=INPUT(SYMGET('DEC'),4.0);
COUNT=COUNT*DAYLEN/WAIT;
run;

PROC SUMMARY NWAY;      *SUM EFFORT OVER LOCATIONS FOR EACH SHIFT/DATE;
CLASSES DATE;           *GIVES ENTIRE LAKE DAILY EFFORT ESTIMATES;
ID MONTH DAYTYPE;
VAR COUNT;
OUTPUT OUT=COUNTS SUM= ;
run;

PROC SUMMARY NWAY;             *AVERAGE DAILY ESTIMATES;
CLASSES MONTH DAYTYPE;         *SEPARATELY BY MONTH AND DAYTYPE;
VAR COUNT;
OUTPUT OUT=COUNTS MEAN= VAR=VCOUNT N=NCOUNT;

DATA COUNTS;             *EXPAND DAILY AVERAGE BY TOTAL NUMBER OF DAYS;
MERGE COUNTS(IN=A) CALENDAR(IN=B DROP=DAYLEN); *ADD CALENDAR DAYS TO FILE;
BY MONTH DAYTYPE;
IF A AND B;

DATA COUNTS;  *EXTRA DATA STEP IS NECESSARY BECAUSE MATCH-MERGES CAN ;
SET COUNTS;   *CAUSE ERRORS IN CALCULATION IN SAME DATA STEP         ;
COUNT=COUNT*DAYS;            *TOTAL EFFORT INCLUDE NON-ANGLING EFFORT;
VCOUNT=VCOUNT/NCOUNT*DAYS*DAYS;

PROC SUMMARY;
CLASSES MONTH DAYTYPE;
VAR COUNT VCOUNT NCOUNT DAYS;
OUTPUT OUT=COUNTS SUM=;

DATA COUNTS;
SET COUNTS;
IF MONTH=. THEN MONTH=99;
IF DAYTYPE=. THEN DAYTYPE=9;
SDCOUNT=SQRT(VCOUNT);
LABEL COUNT='TOTAL PARTY HOURS'
      SDCOUNT='ST. DEV. PARTY HOURS'
      NCOUNT='DAYS SAMPLED'
      DAYS='TOTAL DAYS';

PROC TABULATE FORMAT=COMMA16.2;
TITLE2 'MONTHLY EFFORT SUMMARY (INCLUDES NON-FISHING EFFORT)';
CLASSES MONTH DAYTYPE;
VAR NCOUNT COUNT SDCOUNT;
TABLE MONTH,DAYTYPE,NCOUNT*F=12.0
  COUNT*F=COMMA16.0 SDCOUNT/RTS=30 CONDENSE BOX=_PAGE_ MISSTEXT='***';
KEYLABEL SUM=' ';
run;

DATA COUNTS;
SET COUNTS;
IF MONTH=99 OR DAYTYPE=9 THEN DELETE;

DATA EFFORT;           *MERGE TOTAL PARTY EFFORT AND PROPORTION FILES;
MERGE EFFORT(IN=A) COUNTS(IN=B);
BY MONTH DAYTYPE;
IF A AND B;             *INCLUDES ONLY STRATA WITH BOTH INTS AND COUNTS;

DATA EFFORT;
SET EFFORT;
PHOURS=COUNT*PROP;              *PARTY HOURS FOR THIS STRATA;
VPHOURS=VCOUNT*PROP**2;
TRIPS=PHOURS/MTRIP;             *TOTAL TRIPS FOR THIS STRATA;
VTRIPS=VPHOURS/MTRIP**2;
INDHRS=PHOURS*PARTY;            *TOTAL INDIVIDUAL HOURS FOR THIS STRATA;
VINDHRS=VPHOURS*PARTY**2;
* PARTY=PERSONS/ PARTY
  MTRIP=MEAN TRIP LENGTH
  TRIPS=NUMBER OF TRIPS
  VTRIPS=VARIANCE TRIPS
  PHOURS=TOTAL PARTY HOURS
  VPHOURS=VARIANCE PARTY HOURS
  INDHRS=TOTAL INDIVIDUAL HOURS
  VINDHRS=VARIANCE INDIVIDUAL HOURS;

PROC SUMMARY;                      *COLLECT MARGINAL TOTALS;
CLASSES MONTH DAYTYPE WATERS FISHERY;
ID N HOURS VHOURS;
VAR TRIPS VTRIPS INDHRS VINDHRS PHOURS VPHOURS;
OUTPUT OUT=EFFORT SUM=;

DATA EFFORT;
SET EFFORT;
PARTY=INDHRS/PHOURS;
MTRIP=PHOURS/TRIPS;
IF MONTH=. THEN MONTH=99;
IF DAYTYPE=. THEN DAYTYPE=9;
IF WATERS=' ' THEN DELETE;
IF FISHERY=. THEN DELETE;
SDINDHRS=SQRT(VINDHRS);
SDPHOURS=SQRT(VPHOURS);
SDTRIPS=SQRT(VTRIPS);
LABEL INDHRS='TOTAL INDIVIDUAL HOURS'
      SDINDHRS='ST. DEV.'
      PHOURS='TOTAL PARTY HOURS'
      SDPHOURS='ST. DEV.'
      TRIPS='NUMBER OF TRIPS'
      SDTRIPS='ST. DEV.'
      PARTY='PERSONS/ PARTY'
      MTRIP='MEAN TRIP LENGTH';

PROC SORT;
BY WATERS FISHERY MONTH DAYTYPE;

PROC PRINT SPLIT=' ';
TITLE2 'MONTHLY ESTIMATES OF FISHING EFFORT BY FISHERY TYPE';
FORMAT PHOURS INDHRS TRIPS COMMA10.0 SDPHOURS SDINDHRS SDTRIPS COMMA10.2 PARTY MTRIP 10.2;
BY WATERS FISHERY;
ID MONTH DAYTYPE;
VAR PHOURS SDPHOURS PARTY INDHRS SDINDHRS MTRIP TRIPS SDTRIPS;
run;

DATA EFFORT;
SET EFFORT;
IF MONTH=99 OR DAYTYPE=9 OR FISHERY=7 THEN DELETE;
KEEP WATERS FISHERY MONTH DAYTYPE N HOURS VHOURS PHOURS VPHOURS INDHRS;
run;

DATA INTS;            *CREATE SEPARATE OBSERVATION FOR EACH FISH CAUGHT;
SET INTS;        *USE VARIABLE OBS TO KEEP TRACK OF SEPARATE INTERVIEWS;
IF FISHERY=7 THEN DELETE;                  *DROP NON-FISHING INTERVIEWS;
ARRAY SPEC (28) SPEC1-SPEC28;
ARRAY CLIP (28) CLIP1-CLIP28;
ARRAY LEN (28) LEN1-LEN28;
LENGTH SPECIES $ 3 OBS 4;
FORMAT SPECIES $SPECIES.;
KEEP DATE MONTH DAYTYPE OBS HOURS SPECIES STATE FISHERY;
OBS=_N_;    *FINCLIP=99 MEANS LENGTH FIELD HAS NUMBER OF FISH HARVESTED;
DO I=1 TO 28;
  IF '001' <= SPEC(I) <= '099' THEN DO;
    SPECIES=SPEC(I);
    IF CLIP(I)=99 THEN DO;
      DO J=1 TO LEN(I);
        OUTPUT;
      END;
    END;
    ELSE DO;
      OUTPUT;
    END;
  END;
END;
run;

PROC SORT;
BY OBS SPECIES;

PROC SUMMARY NWAY;
BY OBS;
CLASSES SPECIES;
ID MONTH DAYTYPE HOURS STATE FISHERY;
VAR DATE;
OUTPUT OUT=INTS N=HARVEST;
run;

DATA INTS;
SET INTS;
LENGTH WATERS $ 13;
IF STATE=1 THEN DO;
  WATERS='WISCONSIN';
  COVAR=HARVEST*HOURS;
  OUTPUT;
END;
IF STATE=2 OR STATE=3 THEN DO;
  WATERS='NON-WISCONSIN';
  COVAR=HARVEST*HOURS;
  OUTPUT;
END;
IF STATE=4 OR STATE=5 THEN DO;
  HARVEST=HARVEST/2;
  HOURS=HOURS/2;
  COVAR=HARVEST*HOURS;
  WATERS='WISCONSIN';
  OUTPUT;
  WATERS='NON-WISCONSIN';
  OUTPUT;
END;

PROC SORT;
BY WATERS;

PROC SUMMARY NWAY;
BY WATERS;
CLASSES FISHERY MONTH DAYTYPE SPECIES;
VAR HARVEST COVAR;
OUTPUT OUT=INTS SUM= USS(HARVEST)=VHARVEST;

DATA INTS;
MERGE INTS EFFORT;
BY WATERS FISHERY MONTH DAYTYPE;

DATA INTS;
SET INTS;
IF N=. THEN N=0;
IF N GE 1 THEN DO;
  IF HOURS=. THEN HOURS=0;
  IF HARVEST=. THEN HARVEST=0;
  IF COVAR=. THEN COVAR=0;
  IF VHARVEST=. THEN VHARVEST=0;
  IF VHOURS=. THEN VHOURS=0;
END;
IF N GT 1 THEN DO;
  COVAR=(COVAR-HARVEST*HOURS/N)/(N-1);
  VHOURS=(VHOURS-HOURS*HOURS/N)/(N-1);
  VHARVEST=(VHARVEST-HARVEST*HARVEST/N)/(N-1);
END;
ELSE DO;
  COVAR=.;
  VHOURS=.;
  VHARVEST=.;
END;
HRATE=HARVEST/HOURS;   *PARTY HARVEST RATE - INCLUDES NON-FISH EFFORT;
MHOURS=HOURS/N;
MHARV=HARVEST/N;
IF MHARV=0 AND N>1 THEN VHRATE=0;
ELSE VHRATE=(VHARVEST/MHARV**2)+(VHOURS/MHOURS**2)-(2*COVAR/MHARV/MHOURS);
VHRATE=(HRATE**2)*VHRATE/N;    *VARIANCE OF HARVEST RATE RATIO ESTIMATE;
HARVEST=PHOURS*HRATE;                         *HARVEST AND VARIANCE;
VHARV=PHOURS**2*VHRATE + HRATE**2*VPHOURS + VHRATE*VPHOURS;
KEEP MONTH DAYTYPE WATERS FISHERY SPECIES N HRATE VHRATE HARVEST VHARV INDHRS;

PROC SORT;
BY WATERS SPECIES;

PROC SUMMARY;                              *COLLECT MARGINAL TOTALS;
BY WATERS SPECIES;
CLASSES FISHERY MONTH DAYTYPE;
VAR HARVEST VHARV INDHRS;
OUTPUT OUT=INTS SUM=;

DATA INTS;              *CALCULATE STANDARD DEVIATIONS;
SET INTS;
IF MONTH=. THEN MONTH=99;
IF FISHERY=. THEN FISHERY=99;
IF DAYTYPE=. THEN DAYTYPE=9;
SDHARV=SQRT(VHARV);
HRATE=HARVEST/INDHRS;
LABEL HARVEST='TOTAL HARVEST'
      SDHARV='ST. DEV.'
      HRATE='HARVEST/ ANGLER HOUR';

PROC SORT;
BY WATERS FISHERY SPECIES MONTH DAYTYPE;

PROC TABULATE FORMAT=COMMA8.0;
TITLE2 'TOTAL HARVEST AND HARVEST RATES BASED ON FISHERY-SPECIFIC ANGLING HOURS - DETAIL REPORT';
BY WATERS;
CLASSES SPECIES FISHERY MONTH DAYTYPE;
VAR HARVEST SDHARV HRATE;
TABLE FISHERY,SPECIES*MONTH,DAYTYPE*(HARVEST SDHARV*F=COMMA8.2 HRATE*F=8.5)/
RTS=40 ROW=FLOAT CONDENSE MISSTEXT='***' BOX=_PAGE_;
KEYLABEL SUM=' ';
run;


DATA LENGTHS;           *CALL IN INTERVIEWS FROM EXISTING SAS DATA SET;
SET SAVE.&INTS;
DATE=MDY(MONTH,DAY,YEAR);
FORMAT DATE MMDDYY8.;
ARRAY SPEC (28) SPEC1-SPEC28;
ARRAY LEN (28) LEN1-LEN28;
ARRAY CLIP (28) CLIP1-CLIP28;
LENGTH SPECIES $ 3 LENGTH 8 FINCLIP 3;
FORMAT SPECIES $SPECIES.;
RETAIN DATEA DATEB;
IF _N_=1 THEN DO;
  DATEA=INPUT(SYMGET('SDATE'),MMDDYY8.);
  DATEB=INPUT(SYMGET('FDATE'),MMDDYY8.);
END;
IF DATEA <= DATE <= DATEB;                *KEEP INTS FROM SURVEY PERIOD;
FORMAT MONTH MON. FISHERY FISHERY. STATE STATE.;
LABEL LENGTH='LENGTH (INCHES)'
      FINCLIP='MARK';
KEEP MONTH SPECIES LENGTH FINCLIP DATE SITE FISHERY STATE;
DO I=1 TO 28;
  IF ('001' <= SPEC(I) <= '099') AND CLIP(I) NE 99 THEN DO;
    SPECIES=SPEC(I);
    IF LEN(I) GT 0 THEN LENGTH=LEN(I); ELSE LENGTH=.;
    IF CLIP(I) NE 40 THEN FINCLIP=CLIP(I); ELSE FINCLIP=.;
    OUTPUT;
  END;
END;

PROC TABULATE FORMAT=8.2;
TITLE2 'MEAN LENGTH OF FISH MEASURED BY SPECIES, MONTH AND MARK';
FORMAT FINCLIP FINCLIPB.;
CLASSES SPECIES MONTH FINCLIP;
VAR LENGTH;
TABLE SPECIES,(MONTH ALL)*(FINCLIP ALL), LENGTH*(MEAN STDERR N*F=8.0 VAR MAX MIN)/
MISSTEXT='***' CONDENSE BOX=_PAGE_;
KEYLABEL ALL='TOTAL' N='NUMBER MEASURED';

PROC TABULATE FORMAT=8.2;
TITLE2 'MEAN LENGTH OF FISH MEASURED BY SPECIES, MONTH AND MARK';
FORMAT FINCLIP FINCLIPA.;
CLASSES SPECIES MONTH FINCLIP;
VAR LENGTH;
TABLE SPECIES,(MONTH ALL)*(FINCLIP ALL), LENGTH*(MEAN STDERR N*F=8.0 VAR MAX MIN)/
MISSTEXT='***' CONDENSE BOX=_PAGE_;
KEYLABEL ALL='TOTAL' N='NUMBER MEASURED';

PROC TABULATE FORMAT=9.0;
TITLE2 'DISTRIBUTION OF FIN CLIPS AMONG FISH EXAMINED FOR MARKS';
CLASSES SPECIES MONTH FINCLIP;
FORMAT FINCLIP FINCLIPA.;
TABLE SPECIES*(FINCLIP ALL),MONTH ALL/RTS=30 MISSTEXT='***';
KEYLABEL N=' ';

DATA LENGTHS;   *SELECT ONLY FISH WITH FINCLIPS;
SET LENGTHS;
IF FINCLIP=40 THEN DELETE;
FORMAT FINCLIP FINCLIPA.;

PROC SORT;
BY SPECIES DATE FINCLIP SITE FISHERY STATE;

PROC PRINT LABEL;
TITLE2 'DETAIL LISTING OF ALL FIN-CLIPPED FISH MEASURED';
BY SPECIES;
ID FINCLIP;
VAR DATE SITE FISHERY STATE LENGTH;
RUN;