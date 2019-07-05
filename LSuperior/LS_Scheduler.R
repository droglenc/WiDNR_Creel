#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
# PROGRAM TO CREATE "BUS ROUTE" SCHEDULES FOR LAKE SUPERIOR CREEL
#     XXXX, 201X (version 1 - Derek O)
#
#  DIRECTIONS:
#   * Put up-to-date LS_Scheuler_info.xlsx file in LS_Schedules_YEAR folder.
#   * Follow directions after DIRECTIONS >>> items below.
#   * See resulting files in LS_Schedules_YEAR folder (replace YEAR with #).
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

## DIRECTIONS >>> Change these items as needed (make sure to run each line).
START_DATE <- "05/16/2019" # Starting date for schedule
END_DATE <- "09/30/2019"   # Ending date for schedule
CLERK <- "A"               # A (Ash-Wash), B (B/RC/LSB & Corny/PW), C (Saxon), D (Superior)
SEED <- NULL               # Enter number for repeatability, otherwise NULL
LAKE <- "SUPERIOR"

## DIRECTIONS >>> Run everything below here but don't change anything
INFO <- file.choose()   # Choose LS_Scheduler_info.xlsx file to use
WDIR <- file.path(here::here(),"LSuperior")
source(file.path(WDIR,"Helpers","LS_Scheduler_Helpers.R"))

## DIRECTIONS >>> Run makeSchedule() below to create a preliminary schedule.
## DIRECTIONS >>> Examine the summaries and calendars. If manual changes
## DIRECTIONS >>> are needed then open the CSV file in the LS_Schedules_YEAR
## DIRECTIONS >>> folder in Excel, modify the results, and save. Optionally, run
## DIRECTIONS >>> following to examine schedule (before or after manual changes).
## DIRECTIONS >>>   readSchedule(SCHED,WDIR,show_summary=TRUE,show_calendars=TRUE)
SCHED <- makeSchedule(LAKE,CLERK,START_DATE,END_DATE,SEED,INFO,WDIR,
                      show_summary=TRUE,show_calendars=FALSE)

## DIRECTIONS >>> Run below to create printable PDF of calendar and bus routes
printForClerk(LAKE,START_DATE,CLERK,SEED,INFO,WDIR,SCHED)
