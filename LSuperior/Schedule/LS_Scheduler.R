## User-Specified Information ----
## Change these as needed (make sure to run each line).
LAKE <- "SUPERIOR"
YEAR <- 2019
SEED <- NULL     # Enter number for repeatability, otherwise NULL
CLERK <- "A"     # A (Ash-Wash), B (B/RC/LSB & Corny/PW, C (Saxon), D (Superior)

## Get helper files ----
### Run these but don't change anything
setwd(paste0(here::here(),"/LSuperior/Schedule/"))
source("helpers/LS_Scheduler_Helpers.R")

## Make schedule ----
### Run makeSchedule() to create a preliminary schedule. Examine the summaries
### and calendars. If manual changes are needed then open the CSV file in Excel,
### modify the results, and save. Filename is given in SCHED.
SCHED <- makeSchedule(LAKE,YEAR,CLERK,SEED,show_summary=TRUE,show_calendars=FALSE)

### Optionally run following to examine schedule (before or after manual changes)
###   readSchedule(SCHED,show_summary=TRUE,show_calendars=TRUE)

### Run this to create a printable PDF of calendar and bus routes
printForClerk(LAKE,YEAR,CLERK,SEED,SCHED)
