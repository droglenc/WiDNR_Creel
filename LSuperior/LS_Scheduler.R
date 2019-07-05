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
YEAR <- 2019
CLERK <- "B"       # A (Ash-Wash), B (B/RC/LSB & Corny/PW, C (Saxon), D (Superior)
LAKE <- "SUPERIOR"
SEED <- NULL       # Enter number for repeatability, otherwise NULL

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
SCHED <- makeSchedule(LAKE,YEAR,CLERK,SEED,INFO,WDIR,
                      show_summary=TRUE,show_calendars=FALSE)

## DIRECTIONS >>> Run below to create printable PDF of calendar and bus routes
printForClerk(LAKE,YEAR,CLERK,SEED,INFO,WDIR,SCHED)
