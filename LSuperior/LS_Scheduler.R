#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
# PROGRAM TO CREATE "BUS ROUTE" SCHEDULES FOR LAKE SUPERIOR CREEL
#   SEE LS_SCHEDULER_ASSUMPTINOS.Rmd FOR ASSUMPTIONS SCHEDULER IS BUILT UPON
#     XXXX, 201X (version 1 - Derek O)
#
#  DIRECTIONS:
#   * Make sure shedule information is up-to-date in LS_Scheuler_info.xlsx in
#     the Helpers folder.
#   * Follow directions after DIRECTIONS >>> items below
#   * See resulting files in LS_Schedules_YEAR folder (replace YEAR with #).
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

## DIRECTIONS >>> Change these items as needed (make sure to run each line).
YEAR <- 2019
CLERK <- "D"       # A (Ash-Wash), B (B/RC/LSB & Corny/PW, C (Saxon), D (Superior)
LAKE <- "SUPERIOR"
SEED <- NULL       # Enter number for repeatability, otherwise NULL


## DIRECTIONS >>> Run these but don't change anything
WDIR <- paste0(here::here(),"/LSuperior/")
RDIR <- paste0(WDIR,"LS_Schedules_",YEAR,"/")
source(paste0(WDIR,"Helpers/LS_Scheduler_Helpers.R"))


## DIRECTIONS >>> Run makeSchedule() below to create a preliminary schedule.
## DIRECTIONS >>> Examine the summaries and calendars. If manual changes
## DIRECTIONS >>> are needed then open the CSV file in the LS_Schedules_YEAR
## DIRECTIONS >>> folder in Excel, modify the results, and save. Optionally, run
## DIRECTIONS >>> following to examine schedule (before or after manual changes).
## DIRECTIONS >>>   readSchedule(SCHED,WDIR,show_summary=TRUE,show_calendars=TRUE)
SCHED <- makeSchedule(LAKE,YEAR,CLERK,SEED,WDIR,RDIR,
                      show_summary=TRUE,show_calendars=FALSE)


## DIRECTIONS >>> Run below to create printable PDF of calendar and bus routes
printForClerk(LAKE,YEAR,CLERK,SEED,WDIR,RDIR,SCHED)
