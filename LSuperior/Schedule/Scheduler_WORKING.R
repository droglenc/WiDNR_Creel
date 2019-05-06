LAKE <- "SUPERIOR"
YEAR <- 2019
CLERK <- "A"
SEED <- NULL
fldr <- ""

source(paste0(fldr,"Scheduler_Helpers.R"))

info <- readInfo(fldr,CLERK)

## Make the schedule
sched <- makeSchedule(LAKE,YEAR,info,SEED)
######## can edit the CSV file here (open in Excel)
sched <- readSchedule(sched)

mos <- as.character(unique(sched$MONTH))
for (i in mos) makeCalendar(sched,MONTH1=i)


makeCalendar(sched,MONTH1="Apr")
makeCalendar(sched,MONTH1="May")
