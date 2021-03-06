
filename macros 'C:\Documents and Settings\eggolb\My Documents\SAS programs\creel\schedules\Macro_GB_ice_routes.sas';

%INCLUDE MACROS;
OPTIONS NOSOURCE2 NODATE NONUMBER LINESIZE=85;;

* THIS PROGRAM;
*   1)PRINTS OUT FEBRUARY COUNT FORMS FOR ALL ROUTES;

%LET SEED=523525821;
%LET START='06:30'T;
%LET STOP='12:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
65 601: VOYAGEUR PARK
23 TRAVEL
39 603: DUCK CREEK
10 TRAVEL
52 604: LONG TAIL POINT
10 TRAVEL
65 651: SUNSET BEACH LN
22 TRAVEL
39 636: GEANO BEACH
35 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'VOYAGEUR ROUTE    SHIFT=AM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=414240477;
%LET START='12:30'T;
%LET STOP='18:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
65 601: VOYAGEUR PARK
23 TRAVEL
39 603: DUCK CREEK
10 TRAVEL
52 604: LONG TAIL POINT
10 TRAVEL
65 651: SUNSET BEACH LN
22 TRAVEL
39 636: GEANO BEACH
35 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'VOYAGEUR ROUTE    SHIFT=PM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=251242588;
%LET START='06:30'T;
%LET STOP='12:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
64 606: OCONTO HARBOR
08 TRAVEL
51 639: OCONTO PARKS
17 TRAVEL
51 638: PESHTIGO R. MOUTH
30 TRAVEL
51 653: LITTLE RIVER
10 TRAVEL
51 650: MENOMINEE RIVER
27 TRAVEL
;

TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'OCONTO ROUTE    SHIFT=AM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=755258985;
%LET START='12:30'T;
%LET STOP='18:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
64 606: OCONTO HARBOR
08 TRAVEL
51 639: OCONTO PARKS
17 TRAVEL
51 638: PESHTIGO R. MOUTH
30 TRAVEL
51 653: LITTLE RIVER
10 TRAVEL
51 650: MENOMINEE RIVER
27 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'OCONTO ROUTE    SHIFT=PM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=545125854;
%LET START='06:30'T;
%LET STOP='12:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
34 648: VOLKS LANDING
07 TRAVEL
34 649: BAYSHORE PARK
06 TRAVEL
43 608: DYCKESVILLE    
07 TRAVEL
43 609: RED RIVER     
14 TRAVEL
43 610: CHAUDOIRS
10 TRAVEL
43 611: SUGAR CREEK
04 TRAVEL
44 612: RITES COVE     
28 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'BAYSHORE ROUTE    SHIFT=AM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=782252525;
%LET START='12:30'T;
%LET STOP='18:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
34 648: VOLKS LANDING
07 TRAVEL
34 649: BAYSHORE PARK
06 TRAVEL
43 608: DYCKESVILLE    
07 TRAVEL
43 609: RED RIVER     
14 TRAVEL
43 610: CHAUDOIRS
10 TRAVEL
43 611: SUGAR CREEK
04 TRAVEL
44 612: RITES COVE     
28 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'BAYSHORE ROUTE    SHIFT=PM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=452512114;
%LET START='06:30'T;
%LET STOP='12:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
40 613: LIME KILN ROAD
05 TRAVEL
38 647: BIG ROCK PLACE
02 TRAVEL
45 614: CLAFINS
02 TRAVEL
25 615: CARMODY PARK
11 TRAVEL
54 616: WOOD LANE
02 TRAVEL
32 618: RILEY BAY RD
02 TRAVEL
32 645: TOWN PARK RD 
03 TRAVEL
52 620: SAND BAY RESORT
15 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'LITTLE STURGEON BAY    SHIFT=AM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=252547525;
%LET START='12:30'T;
%LET STOP='18:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
40 613: LIME KILN ROAD
05 TRAVEL
38 647: BIG ROCK PLACE
02 TRAVEL
45 614: CLAFINS
02 TRAVEL
25 615: CARMODY PARK
11 TRAVEL
54 616: WOOD LANE
02 TRAVEL
32 618: RILEY BAY RD
02 TRAVEL
32 645: TOWN PARK RD 
03 TRAVEL
52 620: SAND BAY RESORT
15 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'LITTLE STURGEON BAY    SHIFT=PM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=542579936;
%LET START='06:30'T;
%LET STOP='12:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
30 623: CABOTS PT
03 TRAVEL
46 654: OAK AVE
04 TRAVEL
31 652: HIGH CLIFF
14 TRAVEL
91 626: POT PARK
12 TRAVEL
30 627: BULLHEAD POINT
03 TRAVEL
30 646: BAYVIEW PARK
04 TRAVEL
46 628: STURGEON BAY YACHT CLUB
16 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'SHERWOOD ROUTE    SHIFT=AM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=552545288;
%LET START='12:30'T;
%LET STOP='18:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
30 623: CABOTS PT
03 TRAVEL
46 654: OAK AVE
04 TRAVEL
31 652: HIGH CLIFF
14 TRAVEL
91 626: POT PARK
12 TRAVEL
30 627: BULLHEAD POINT
03 TRAVEL
30 646: BAYVIEW PARK
04 TRAVEL
46 628: STURGEON BAY YACHT CLUB
16 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'SHERWOOD ROUTE    SHIFT=PM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=125448752;
%LET START='06:30'T;
%LET STOP='12:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
31 630: MEMORIAL DRIVE
05 TRAVEL
31 631: SUNSET PARK
08 TRAVEL
78 632: BIRMINGHAMS
02 TRAVEL
93 633: STONE QUARRY
07 TRAVEL
37 655: PEBBLE BEACH
08 TRAVEL
41 656: MURPHY PARK
19 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'BIG CREEK ROUTE    SHIFT=AM    MONTH=FEBRUARY';
%SKEDULE

%LET SEED=245822585;
%LET START='12:30'T;
%LET STOP='18:30'T;
%LET RANNUM=5;
%LOCS
CARDS;
31 630: MEMORIAL DRIVE
05 TRAVEL
31 631: SUNSET PARK
08 TRAVEL
78 632: BIRMINGHAMS
02 TRAVEL
93 633: STONE QUARRY
07 TRAVEL
37 655: PEBBLE BEACH
08 TRAVEL
41 656: MURPHY PARK
19 TRAVEL
;
TITLE 'GREEN BAY ICE CREEL SURVEY RANDOMIZATION - COUNT FORM';
TITLE2 'BIG CREEK ROUTE    SHIFT=PM    MONTH=FEBRUARY';
%SKEDULE

run;
