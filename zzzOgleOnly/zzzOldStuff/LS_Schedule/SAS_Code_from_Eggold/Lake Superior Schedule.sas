
filename macros 'C:\Documents and Settings\eggolb\My Documents\SAS programs\creel\schedules\lake superior\Macro_LS_schedule.sas';

%INCLUDE MACROS;
OPTIONS NOSOURCE2 YEARCUTOFF=1950;

%LET SDATE='01MAR12'D;    /*FIRST DAY OF CALENDAR*/
%LET FDATE='30SEP12'D;    /*LAST DAY OF CALENDAR*/
%LET UNITS=2;             /*NUMBER OF WORK GROUPS TO BE COVERED*/
%LET SITES1=1;            /*NUMBER OF SITES ON FIRST GROUP*/
%LET SITES2=1;            /*NUMBER OF SITES ON SECOND GROUP*/
%LET COUNTS=1;            /*NUMBER OF INSTANTANEOUS COUNTS PER SHIFT*/
%LET SHIFTS=2;
%LET SEED=568215852;		   *CHANGE SEED ANNUALLY;
PROC FORMAT;
VALUE $SITE '11'='WASHBURN'
            '21'='B/RC/LSB';

OPTIONS NODATE NONUMBER;
%SHIFTS
CARDS;      /*INSERT DAYLENGTH TIMES - MUST ACCOUNT FOR DST*/
03/01/12 03/10/12 07:00 21:00 7.0  5
03/11/12 03/31/12 07:00 21:00 7.0  5
04/02/12 04/30/12 07:00 21:00 7.0  5
05/01/12 05/14/12 07:00 21:00 7.0  5 
05/15/12 05/31/12 07:00 21:00 7.0  5
06/01/12 08/31/12 07:00 21:00 7.0  5
09/01/12 09/30/12 07:00 21:00 7.0  5
10/01/12 10/31/12 07:00 21:00 7.0  5
;
TITLE '2012 LAKE SUPERIOR CREEL SURVEY RANDOMIZATION';
%INTCOUNT

run;
