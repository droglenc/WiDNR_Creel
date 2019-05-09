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

fldr <- ""
source(paste0(fldr,"Scheduler_Helpers.R"))

## Make schedule ----
### Run makeSchedule() to create a preliminary schedule. Examine the summaries
### and calendars. If manual changes are needed then open the CSV file in Excel,
### modify the results, and save. Filename is given in fout.
SCHED <- makeSchedule(LAKE,YEAR,readInfo(fldr,CLERK),SEED,
                      show_summary=FALSE,show_calendars=FALSE)

### Run the following to examine the schedule (before or after manual changes):
###   readSchedule(fout,show_summary=TRUE,show_calendars=TRUE)


# Run this to create the schedule (set the working directory)
rmarkdown::render(input="Scheduler_Template.Rmd",
                  params=list(LAKE=LAKE,YEAR=YEAR,CLERK=CLERK,SEED=SEED,SCHED=SCHED),
                  output_file=paste0(YEAR,"_Superior_",CLERK,".pdf"),
                  output_format="pdf_document",
                  clean=FALSE,quiet=TRUE)
