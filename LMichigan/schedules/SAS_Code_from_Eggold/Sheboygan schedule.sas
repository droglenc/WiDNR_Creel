filename macros 'C:\Documents and Settings\eggolb\My Documents\SAS programs\creel\schedules\Macro_open_water_creel2.sas';

%INCLUDE MACROS;
OPTIONS NOSOURCE2 YEARCUTOFF=1950;

*TO SAVE $$, WE REDUCED THE DAYS PER WEEK FROM 5 TO 4 STARTING IN 2003 FOR MARCH - MAY AND
SEPTEMBER, OCTOBER.;

* CHANGES MADE IN 03/2000;
* TOOK OFF FISHER CREEK, CLEVELAND RAMP AND 7-MILE CREEK FROM F ROUTE;
* ADDED PIGEON RIVER TO G ROUTE AND TOOK OFF AMSTERDAM FROM H ROUTE;


%LET SDATE='15MAR18'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='14MAY18'D;    /*LAST DAY OF CALENDAR*/
%LET UNITS=3;             /*NUMBER OF WORK GROUPS TO BE COVERED*/
%LET SITES1=2;            /*NUMBER OF SITES ON FIRST GROUP*/
%LET SITES2=2;            /*NUMBER OF SITES ON 2ND GROUP*/
%LET SITES3=2;            /*NUMBER OF SITES ON 3RD GROUP*/
%LET COUNTS=2;            /*NUMBER OF INSTANTANEOUS COUNTS PER SHIFT*/
%LET SEED=542524852;      /*RANDOM NUMBER*/
PROC FORMAT;
*ALUE $SITE '11'='E-F'  '12'='F-E'
            '21'='G-H'  '22'='H-G';
VALUE $SITE '11'='E-H'  '12'='E-G'
            '21'='H-E'  '22'='H-G'
            '31'='G-E'  '32'='G-H';
OPTIONS NODATE NONUMBER;
%SHIFTS
CARDS;      /*INSERT DAYLENGTH TIMES - MUST ACCOUNT FOR DST*/
03/01/18 03/10/18 06:00 18:30 6.25 4
03/11/18 03/31/18 06:00 20:00 7.0  4
04/01/18 04/30/18 06:00 20:00 7.0  4
05/01/18 05/14/18 05:30 20:30 7.5  4
05/15/18 05/31/18 05:30 20:30 7.5  4
06/01/18 08/31/18 05:00 21:30 8.25 4
09/01/18 09/30/18 05:30 20:00 7.25 4
10/01/18 10/31/18 06:30 19:00 6.25 4;

TITLE '2018 OZAUKEE-SHEBOYGAN SPRING CREEL SURVEY RANDOMIZATION';
%INTCOUNT

%LET SDATE='15MAY18'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='31AUG18'D;    /*LAST DAY OF CALENDAR*/
%LET UNITS=1;             /*NUMBER OF WORK GROUPS TO BE COVERED*/
%LET SITES1=2;            /*NUMBER OF SITES ON FIRST GROUP*/
%LET COUNTS=2;            /*NUMBER OF INSTANTANEOUS COUNTS PER SHIFT*/
%LET SEED=548212325;      /*RANDOM NUMBER*/
PROC FORMAT;
VALUE $SITE '11'='SHEB. - PORT W.'  '12'='PORT W. - SHEB.';
OPTIONS NODATE NONUMBER;
%SHIFTS
CARDS;      /*INSERT DAYLENGTH TIMES - MUST ACCOUNT FOR DST*/
03/01/18 03/10/18 06:00 18:30 6.25 4
03/11/18 03/31/18 06:00 20:00 7.0  4
04/01/18 04/30/18 06:00 20:00 7.0  4
05/01/18 05/14/18 05:30 20:30 7.5  4
05/15/18 05/31/18 05:30 20:30 7.5  4
06/01/18 08/31/18 05:00 21:30 8.25 4
09/01/18 09/30/18 05:30 20:00 7.25 4
10/01/18 10/31/18 06:30 19:00 6.25 4;

TITLE '2018 OZAUKEE-SHEBOYGAN AREA SUMMER CREEL SURVEY RANDOMIZATION';
%INTCOUNT


%LET SDATE='01SEP18'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='31OCT18'D;    /*LAST DAY OF CALENDAR*/
%LET UNITS=3;             /*NUMBER OF WORK GROUPS TO BE COVERED*/
%LET SITES1=2;            /*NUMBER OF SITES ON FIRST GROUP*/
%LET SITES2=2;            /*NUMBER OF SITES ON 2ND GROUP*/
%LET SITES3=2;            /*NUMBER OF SITES ON 2ND GROUP*/
%LET COUNTS=2;            /*NUMBER OF INSTANTANEOUS COUNTS PER SHIFT*/
%LET SEED=548521258;     /*RANDOM NUMBER*/
PROC FORMAT;
*ALUE $SITE '11'='E-F'  '12'='F-E'
            '21'='G-H'  '22'='H-G';
VALUE $SITE '11'='E-H'  '12'='E-G'
            '21'='H-E'  '22'='H-G'
            '31'='G-E'  '32'='G-H';
OPTIONS NODATE NONUMBER;
%SHIFTS
CARDS;      /*INSERT DAYLENGTH TIMES - MUST ACCOUNT FOR DST*/
03/01/18 03/10/18 06:00 18:30 6.25 4
03/11/18 03/31/18 06:00 20:00 7.0  4
04/01/18 04/30/18 06:00 20:00 7.0  4
05/01/18 05/14/18 05:30 20:30 7.5  4
05/15/18 05/31/18 05:30 20:30 7.5  4
06/01/18 08/31/18 05:00 21:30 8.25 4
09/01/18 09/30/18 05:30 20:00 7.25 4
10/01/18 10/31/18 06:30 19:00 6.25 4;

TITLE '2018 OZAUKEE-SHEBOYGAN FALL CREEL SURVEY RANDOMIZATION';
%INTCOUNT

run;