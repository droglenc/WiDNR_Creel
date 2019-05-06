LAKE <- "SUPERIOR"
YEAR <- 2019
CLERK <- "A"
SEED <- 345
fldr <- ""

source(paste0(fldr,"Scheduler_Helpers.R"))

info <- readInfo(fldr,CLERK)

## Make the schedule
sched <- makeSchedule(LAKE,YEAR,info,SEED,FALSE)
######## can edit the CSV file here (open in Excel)
sched <- readSchedule(sched)

makeCalendar(sched,MONTH1="May")

mos <- as.character(unique(sched$MONTH))
for (i in mos) makeCalendar(sched,MONTH1=i)
