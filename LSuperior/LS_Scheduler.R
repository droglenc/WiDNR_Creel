#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
# PROGRAM TO CREATE "BUS ROUTE" SCHEDULES FOR LAKE SUPERIOR CREEL
#     XXXX, 201X (version 1 - Derek O)
#
#  DIRECTIONS:
#  1. Create a "LS_SCHEDULES_OPEN_YEAR" folder (where YEAR is replaced with the
#     year for which schedules will be created; e.g., LS_SCEDULES_OPEN_2019)
#     inside the "LSuperior" folder.
#  2. Put up-to-date LS_Schedules_Open_info.xlsx file in folder from 1.
#  3. Change these items as needed (make sure to run each line).

START_DATE <- "05/16/2019" # Starting date for schedule
END_DATE <- "09/30/2019"   # Ending date for schedule
CLERK <- "A"               # A (Ash-Wash), B (B/RC/LSB & Corny/PW), C (Saxon), D (Superior)
SEED <- NULL               # Enter number for repeatability, otherwise NULL
LAKE <- "SUPERIOR"

#  4. Run below here but don't change anything.

message("Choose the information file in the dialog box",
        " (may be behind other windows).")
INFO <- file.choose()
WDIR <- file.path(here::here(),"LSuperior")
source(file.path(WDIR,"Helpers","LS_Scheduler_Helpers.R"))

#  5. Run below to create a preliminary schedule.

SCHED <- makeSchedule(LAKE,CLERK,START_DATE,END_DATE,SEED,INFO,WDIR,
                      show_summary=TRUE,show_calendars=TRUE)

#  6. Examine the summaries and calendars (can PgUp or PgDn to move through
#     calendars in the separate window). If manual changes are needed then
#     open ( in Excel) the CSV file in the folder from 1, modify the results,
#     and save. If you make manual changes then you can run the code below to
#     examine the modified schedule.

readSchedule(SCHED,WDIR,show_summary=TRUE,show_calendars=TRUE)

#  7. Run below to create printable PDF of calendar and bus routes that will be
#     in the folder from 1.

printForClerk(LAKE,START_DATE,CLERK,SEED,INFO,WDIR,SCHED)

#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
