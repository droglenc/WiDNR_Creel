filename macros 'C:\Documents and Settings\eggolb\My Documents\SAS programs\creel\schedules\Macro_open_water_creel.sas';
*To save $$, we cut back to 4 days per week for the entire survey in 2003 for March - May and
September and October schedules.;

%INCLUDE MACROS;
OPTIONS NOSOURCE2 YEARCUTOFF=1950;

%LET SDATE='15MAR17'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='14MAY17'D;    /*LAST DAY OF CALENDAR*/
%LET UNITS=2;             /*NUMBER OF WORK GROUPS TO BE COVERED*/
%LET SITES1=2;            /*NUMBER OF SITES ON FIRST GROUP*/
%LET SITES2=2;            /*NUMBER OF SITES ON 2ND GROUP*/
%LET COUNTS=2;            /*NUMBER OF INSTANTANEOUS COUNTS PER SHIFT*/
%LET SEED=124585252;      /*RANDOM NUMBER*/
PROC FORMAT;
VALUE $SITE '11'='A-B'  '12'='B-A'
            '21'='C-D'  '22'='D-C';
OPTIONS NODATE NONUMBER;
%SHIFTS
CARDS;      /*INSERT DAYLENGTH TIMES - MUST ACCOUNT FOR DST*/
03/01/17 03/11/17 06:00 18:30 6.25 4
03/12/17 03/31/17 06:00 20:00 7.0  4
04/01/17 04/30/17 06:00 20:00 7.0  4
05/01/17 05/14/17 05:30 20:30 7.5  4
05/15/17 05/31/17 05:30 20:30 7.5  4
06/01/17 08/31/17 05:00 21:30 8.25 4
09/01/17 09/30/17 06:00 19:30 6.75 4
10/01/17 10/31/17 06:30 18:30 6.0  4
;


%MOORED2
CARDS;
05/17/17 MOORED BOAT COUNT   1
;

TITLE '2017 MANITOWOC SPRING CREEL SURVEY RANDOMIZATION';
%INTCOUNT


%LET SDATE='15MAY17'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='31AUG17'D;    /*LAST DAY OF CALENDAR*/
%LET UNITS=1;             /*NUMBER OF WORK GROUPS TO BE COVERED*/
%LET SITES1=2;            /*NUMBER OF SITES ON FIRST GROUP*/
%LET COUNTS=2;            /*NUMBER OF INSTANTANEOUS COUNTS PER SHIFT*/
%LET SEED=542512582;      /*RANDOM NUMBER*/
PROC FORMAT;
VALUE $SITE '11'='TWO RIVERS - MAN.' '12'='MAN. - TWO RIVERS';
OPTIONS NODATE NONUMBER;
%SHIFTS
CARDS;      /*INSERT DAYLENGTH TIMES - MUST ACCOUNT FOR DST*/
03/01/17 03/11/17 06:00 18:30 6.25 4
03/12/17 03/31/17 06:00 20:00 7.0  4
04/01/17 04/30/17 06:00 20:00 7.0  4
05/01/17 05/14/17 05:30 20:30 7.5  4
05/15/17 05/31/17 05:30 20:30 7.5  4
06/01/17 08/31/17 05:00 21:30 8.25 4
09/01/17 09/30/17 06:00 19:30 6.75 4
10/01/17 10/31/17 06:30 18:30 6.0  4
;

%MOORED2
CARDS;
05/17/17 MOORED BOAT COUNT   1
06/14/17 MOORED BOAT COUNT   1
07/12/17 MOORED BOAT COUNT   1
08/16/17 MOORED BOAT COUNT   1
;

TITLE '2017 MANITOWOC SUMMER CREEL SURVEY RANDOMIZATION';
%INTCOUNT


%LET SDATE='01SEP17'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='31OCT17'D;    /*LAST DAY OF CALENDAR*/
%LET UNITS=2;             /*NUMBER OF WORK GROUPS TO BE COVERED*/
%LET SITES1=2;            /*NUMBER OF SITES ON FIRST GROUP*/
%LET SITES2=2;            /*NUMBER OF SITES ON 2ND GROUP*/
%LET COUNTS=2;            /*NUMBER OF INSTANTANEOUS COUNTS PER SHIFT*/
%LET SEED=215215852;      /*RANDOM NUMBER*/
PROC FORMAT;
VALUE $SITE '11'='A-B'  '12'='B-A'
            '21'='C-D'  '22'='D-C';
OPTIONS NODATE NONUMBER;
%SHIFTS
CARDS;      /*INSERT DAYLENGTH TIMES - MUST ACCOUNT FOR DST*/
03/01/17 03/11/17 06:00 18:30 6.25 4
03/12/17 03/31/17 06:00 20:00 7.0  4
04/01/17 04/30/17 06:00 20:00 7.0  4
05/01/17 05/14/17 05:30 20:30 7.5  4
05/15/17 05/31/17 05:30 20:30 7.5  4
06/01/17 08/31/17 05:00 21:30 8.25 4
09/01/17 09/30/17 06:00 19:30 6.75 4
10/01/17 10/31/17 06:30 18:30 6.0  4
;


%MOORED2
CARDS;
09/12/17 MOORED BOAT COUNT   1
;

TITLE '2017 MANITOWOC FALL CREEL SURVEY RANDOMIZATION';
%INTCOUNT

run;
