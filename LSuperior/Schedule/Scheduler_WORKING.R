## User-Specified Information ----
LAKE <- "SUPERIOR"
YEAR <- 2019       # 2010-2099
CLERK <- "B"       # A=Ash-Wash, B=B/R/LSB-Corny/PW, C=Saxon, D=Superior
SEED <- NULL       # NULL is for non-repeatability
fldr <- ""         # 

## Load Helper files and Creel information ----
source(paste0(fldr,"Scheduler_Helpers.R"))

info <- readInfo(fldr,CLERK)

## Make schedule ----
### Run makeSchedule() to create a preliminary schedule. Examine the summaries
### and calendars. If manual changes are needed then open the CSV file in Excel,
### modify the results, and save. Either way run readSchedule() to read the
### schedule into a data.frame for use further below.
prelim <- makeSchedule(LAKE,YEAR,info,SEED,show_summary=TRUE,show_calendars=TRUE)
######## can edit the CSV file here (open in Excel)
sched <- readSchedule(prelim,show_summary=TRUE,show_calendars=TRUE)

mos <- as.character(unique(sched$MONTH))
mos

i <- "Apr"
j <- 1

for (i in mos) {
  tmp <- dplyr::filter(sched,MONTH==i,CREEL=="YES")
  for (j in seq_len(nrow(tmp))) {
    br <- iMakeBusRoute(info,
                        ROUTE=as.character(tmp$ROUTE[j]),
                        SHIFT=as.character(tmp$SHIFT[j]),
                        MONTH1=as.character(tmp$MONTH[j]))
    cat("\\newpage\n")
    print(iPrintBusRouteHeader(tmp[j,],LAKE))
    cat("\\vspace{24pt}\n")
    print(iPrintBusRoute(br))
  }
}
