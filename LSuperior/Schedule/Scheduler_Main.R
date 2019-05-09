## User-Specified Information ----
## Change these as needed (make sure to run each line).
##   LAKE ... Can only be "SUPERIOR"
##   YEAR ... Year to make the calendar for
##   CLERK ... A = Ashland-Washburn
##             B = Bayfield/Red Cliff/Little Sand Bay and Corny/Herbster/Port Wing
##             C = Saxon
##             D = Superior
##   SEED ... Random number seed ... enter number for repeatability, otherwise NULL
LAKE <- "SUPERIOR"
YEAR <- 2019
CLERK <- "A"
SEED <- NULL

FLDR <- "LSuperior/Schedule/"
source(paste0(FLDR,"Scheduler_Helpers.R"))

## Make schedule ----
### Run makeSchedule() to create a preliminary schedule. Examine the summaries
### and calendars. If manual changes are needed then open the CSV file in Excel,
### modify the results, and save. Filename is given in fout.
SCHED <- makeSchedule(LAKE,YEAR,readInfo(FLDR,CLERK),SEED,
                      show_summary=FALSE,show_calendars=FALSE)

### Run the following to optionally examine the schedule (before or after
### manual changes):
###   readSchedule(fout,show_summary=TRUE,show_calendars=TRUE)

### Run this to create a printable PDF of calendar and bus routes
printForClerk(LAKE=LAKE,YEAR=YEAR,CLERK=CLERK,SEED=SEED,SCHED=SCHED,FLDR=FLDR)
