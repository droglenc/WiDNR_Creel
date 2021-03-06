filename macros 'C:\Documents and Settings\eggolb\My Documents\SAS programs\creel\schedules\Macro_Door_County_routes.sas';
filename macro2 'C:\Documents and Settings\eggolb\My Documents\SAS programs\creel\schedules\Macro_open_water_creel.sas';

%INCLUDE MACROS;
OPTIONS NOSOURCE2 YEARCUTOFF=1950;

%LET SDATE='15MAR17'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='14MAY17'D;    /*LAST DAY OF CALENDAR*/
%LET NUMDAYS=5;           /*NUMBER OF WORKDAYS PER WEEK*/
%LET ROUTES=2;            /*NUMBER OF ROUTES TO BE COVERED*/
%LET SITES1=8;           /*NUMBER OF SITES ON FIRST ROUTE*/
%LET SITES2=7;            /*NUMBER OF SITES ON SECOND ROUTE*/
%LET SHIFTS=2;            /*NUMBER OF SHIFTS*/
%LET SEED=215485825;      /*RANDOM NUMBER*/
PROC FORMAT;
VALUE ROUTE 1='                 ND1' 2='                 ND2';
VALUE SHIFT 1='AM' 2='PM';
VALUE DIR 1='+' 2='-';
OPTIONS NODATE NONUMBER;

%MOORED2
CARDS;
05/17/17 MOORED BOAT COUNT   1
;
TITLE 'DOOR AND KEWAUNEE COUNTY CREEL SURVEY SCHEDULES FOR 2017';
TITLE2 '2017 DOOR COUNTY CREEL SURVEY RANDOMIZATION - SPRING NORTHERN';
%CALENDAR

%LET SDATE='01SEP17'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='31OCT17'D;    /*LAST DAY OF CALENDAR*/
%LET NUMDAYS=5;           /*NUMBER OF WORKDAYS PER WEEK*/
%LET ROUTES=2;            /*NUMBER OF ROUTES TO BE COVERED*/
%LET SITES1=8;           /*NUMBER OF SITES ON FIRST ROUTE*/
%LET SITES2=7;            /*NUMBER OF SITES ON SECOND ROUTE*/
%LET SHIFTS=2;            /*NUMBER OF SHIFTS*/
%LET SEED=525415258;      /*RANDOM NUMBER*/
PROC FORMAT;
VALUE ROUTE 1='                 ND1' 2='                 ND2';
VALUE SHIFT 1='AM' 2='PM';
VALUE DIR 1='+' 2='-';
OPTIONS NODATE NONUMBER;

%MOORED2
CARDS;
09/13/17 MOORED BOAT COUNT   1
;
TITLE2 '2017 DOOR COUNTY CREEL SURVEY RANDOMIZATION - FALL NORTHERN';
%CALENDAR

run;

%INCLUDE MACRO2;
OPTIONS NOSOURCE2 YEARCUTOFF=1950;

%LET SDATE='15MAR17'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='14MAY17'D;    /*LAST DAY OF CALENDAR*/
%LET UNITS=1;             /*NUMBER OF WORK GROUPS TO BE COVERED*/
%LET SITES1=2;            /*NUMBER OF SITES ON FIRST GROUP*/
%LET COUNTS=2;            /*NUMBER OF INSTANTANEOUS COUNTS PER SHIFT*/
%LET SEED=524158258;      /*RANDOM NUMBER*/
PROC FORMAT;
VALUE $SITE '11'='ALGOMA'  '12'='KEWAUNEE';
OPTIONS NODATE NONUMBER;
%SHIFTS
CARDS;      /*INSERT DAYLENGTH TIMES - MUST ACCOUNT FOR DST*/
03/01/17 03/11/17 06:00 18:30 6.25 5
03/12/17 03/31/17 06:00 20:00 7.0  5
04/01/17 04/30/17 06:00 20:00 7.0  5
05/01/17 05/31/17 05:30 20:30 7.5  5
;

%MOORED2
CARDS;
05/17/17 MOORED BOAT COUNT   1
;

TITLE2 '2017 ALGOMA-KEWAUNEE SPRING CREEL SURVEY RANDOMIZATION';
%INTCOUNT

%LET SDATE='01SEP17'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='31OCT17'D;    /*LAST DAY OF CALENDAR*/
%LET UNITS=1;             /*NUMBER OF WORK GROUPS TO BE COVERED*/
%LET SITES1=2;            /*NUMBER OF SITES ON FIRST GROUP*/
%LET COUNTS=2;            /*NUMBER OF INSTANTANEOUS COUNTS PER SHIFT*/
%LET SEED=524125258;      /*RANDOM NUMBER*/
PROC FORMAT;
VALUE $SITE '11'='ALGOMA'  '12'='KEWAUNEE';
OPTIONS NODATE NONUMBER;
%SHIFTS
CARDS;      /*INSERT DAYLENGTH TIMES - MUST ACCOUNT FOR DST*/
09/01/17 09/30/17 06:00 19:30 6.75 5
10/01/17 10/31/17 06:30 18:30 6.00 5
;

%MOORED
CARDS;
09/13/17 MOORED BOAT COUNT   1
;

%COMBINE
TITLE2 '2017 ALGOMA-KEWAUNEE FALL CREEL SURVEY RANDOMIZATION';
%INTCOUNT

run;

%LET SDATE='15MAY17'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='31AUG17'D;    /*LAST DAY OF CALENDAR*/
%LET NUMDAYS=3;           /*NUMBER OF WORKDAYS PER WEEK*/
%LET ROUTES=1;            /*NUMBER OF ROUTES TO BE COVERED*/
%LET SITES1=9;            /*NUMBER OF SITES ON FIRST ROUTE deleted 3 for 2002*/
%LET SHIFTS=2;            /*NUMBER OF SHIFTS*/
%LET SEED=214582582;      /*RANDOM NUMBER*/
PROC FORMAT;
VALUE ROUTE 1='       NORTHERN DOOR' ;
VALUE SHIFT 1='AM' 2='PM';
VALUE DIR 1='+' 2='-';
OPTIONS NODATE NONUMBER;

%MOORED2
CARDS;
05/17/17 MOORED BOAT COUNT   1
06/14/17 MOORED BOAT COUNT   1
07/12/17 MOORED BOAT COUNT   1
08/16/17 MOORED BOAT COUNT   1
;
TITLE2 '2017 NORTHERN DOOR COUNTY SUMMER CREEL SURVEY RANDOMIZATION';
%CALENDAR

RUN;


%LET SDATE='15MAY17'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='31AUG17'D;    /*LAST DAY OF CALENDAR*/ *Changed to Oct 31 for 2012 season;
%LET UNITS=3;             /*NUMBER OF WORK GROUPS TO BE COVERED*/
%LET SITES1=2;            /*NUMBER OF SITES ON FIRST GROUP*/
%LET SITES2=2;               
%LET SITES3=2;
%LET COUNTS=2;            /*NUMBER OF INSTANTANEOUS COUNTS PER SHIFT*/
%LET SEED=542585693;      /*RANDOM NUMBER*/
PROC FORMAT;
VALUE $SITE '11'='KEW - ALG'  '12'='ALG - KEW'
			'21'='KEW - ST BAY' '22'='ST BAY - KEW'
			'31'='ALG - ST BAY' '32'='ST BAY - ALG';

OPTIONS NODATE NONUMBER;
%SHIFTS
CARDS;      /*INSERT DAYLENGTH TIMES - MUST ACCOUNT FOR DST*/
05/01/17 05/31/17 05:30 20:30 7.5  5
06/01/17 08/31/17 05:00 21:00 8.0  5
09/01/17 09/30/17 06:00 19:30 6.75 5
10/01/17 10/31/17 06:30 18:30 6.00 5
;

%MOORED
CARDS;
05/17/17 MOORED BOAT COUNT   1
06/14/17 MOORED BOAT COUNT   1
07/12/17 MOORED BOAT COUNT   1
08/16/17 MOORED BOAT COUNT   1
;
%COMBINE
TITLE2 '2017 ALGOMA - KEWAUNEE - STURGEON BAY SUMMER CREEL SURVEY RANDOMIZATION';
%INTCOUNT

run;
