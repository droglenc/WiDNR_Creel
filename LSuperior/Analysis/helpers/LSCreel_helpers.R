## Load Packages ----
library(FSA)
library(lubridate)
library(tables)
library(huxtable)
library(tidyr)
library(dplyr)


## Helper Functions ----

### Read and prepare the interview data file
###   Convert some codes to words
###   Handle dates (find weekends and weekdays) & times (incl. hours of effort)
readInterviewData <- function(LOC,SDATE,FDATE,
                              dropCLS=TRUE,dropHM=TRUE) {
  d <- read.csv(paste0("data/",LOC,"ints.csv")) %>%
    dplyr::mutate(STATE=iMvStates(STATE),
                  WATERS=iMvWaters(STATE),
                  FISHERY=iMvFishery(FISHERY),
                  STATUS=iMvStatus(STATUS),
                  RES=iMvResidency(RES),
                  DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%y"),
                  YEAR=lubridate::year(DATE),
                  MONTH=lubridate::month(DATE,label=TRUE,abbr=FALSE),
                  MDAY=lubridate::mday(DATE),
                  WDAY=lubridate::wday(DATE,label=TRUE,abbr=TRUE),
                  DAYTYPE=iMvDaytype(WDAY,MONTH,MDAY),
                  HOURS=iHndlHours(STARTHH,STARTMM,STOPHH,STOPMM,
                                   DATE,SDATE,FDATE))
  if (dropHM) d <- dplyr::select(d,-(STARTMM:STOPHH))
  if (dropCLS) {
    allNA <- sapply(d,function(x) all(is.na(x)))
    allNAv <- names(d)[allNA]
    cat("The following were removed (as requested) because they were all NAs:\n")
    cat(paste(allNAv[grepl("CLIP",allNAv)],collapse=", "),"\n")
    cat(paste(allNAv[grepl("LEN",allNAv)],collapse=", "),"\n")
    cat(paste(allNAv[grepl("SPEC",allNAv)],collapse=", "),"\n")
    d <- d[,!allNA]
  }
  d
}


readPressureCountData <- function(LOC,SDATE,FDATE,dropHM=TRUE) {
  d <- read.csv(paste0("data/",LOC,"cnts.csv")) %>%
    dplyr::mutate(DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%Y"),
                  YEAR=year(DATE),
                  MONTH=month(DATE,label=TRUE,abbr=FALSE),
                  MDAY=mday(DATE),
                  WDAY=wday(DATE,label=TRUE,abbr=TRUE),
                  DAYTYPE=iMvDaytype(WDAY,MONTH,MDAY),
                  COUNT=iConvNA20(COUNT),
                  WAIT=iHndlHours(STARTHH,STARTMM,STOPHH,STOPMM,
                                  DATE,SDATE,FDATE),
                  COUNT=COUNT*WAIT
    )
  if (dropHM) d <- dplyr::select(d,-(STARTMM:STOPHH))
  d
}

## Creates day lengths from month names
iMvDaylen <- function(x) {
  mos <- c("January","February","March","April","May","June",
           "July","August","September","October","November","December")
  lens <- c(0,0,0,0,16,16,16,16,16,0,0,0)
  x <- plyr::mapvalues(x,from=mos,to=lens,warn=FALSE)
  x  
}

## Convert DOW to weekend or weekdays ... with holidays as weekends
iMvDaytype <- function(wd,mon,md) {
  wd2 <- plyr::mapvalues(wd,from=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                         to=c("WEEKDAY","WEEKDAY","WEEKDAY","WEEKDAY","WEEKDAY",
                              "WEEKEND","WEEKEND"),warn=FALSE)
  wd2 <- iHndlHolidays(mon,md,wd,wd2)
  wd2 <- factor(wd2,levels=c("WEEKDAY","WEEKEND"))
  wd2  
}

## Convert fin-clip codes to words
iMvFinclips <- function(x) {
  tmp <- c('00: NONE','01: AD','02: AD+LV','03: AD+RV','04: AD+LP','05: AD+RP',
           '06: LV','07: RV','08: LP','09: RP','10: LV+RV','11: RP+LV',
           '12: AD+LV+RV','13: D','14: HATCHERY','15: LP+RV','16: D+RV',
           '17: D+RP','18: AD+RM','19: LP+RM','20: LP+LV','21: D+AD',
           '22: D+LV+RV','23: D+LP','24: D+LV','40: NOT EXAMINED')
  x <- plyr::mapvalues(x,from=c(0:24,40),to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x  
}

## Convert "fishery" codes to words
iMvFishery <- function(x) {
  tmp <- c("COLDWATER-OPEN","WARMWATER-OPEN","ICE-STREAM MOUTH","ICE-WARMWATER",
           "ICE-BOBBING","BAD RIVER","NON-FISHING","SHORE","TRIBAL","COMBINED")
  x <- plyr::mapvalues(x,from=1:10,to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x
}

## Convert residency codes to words
iMvResidency <- function(x) {
  tmp <- c("Resident","Non-Resident","Resident/Non-Resident")
  x <- plyr::mapvalues(x,from=1:3,to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x
}

## Convert fish species codes to words ... note possibly two sets of codes
iMvSpecies <- function(x,useFirst=TRUE) {
  num1 <- c('W09','W00','W02','W14','I22','W06','R01','M12','I04','I24','I23',
            'I28','L02','I21','I19','W04','I16','I14','I12','I20','J01','B01',
            'W12','W11','I05','I18','X15','X22','098','099')
  num2 <- c('002','003','004','005','006','007','011','013','033','034','035',
            '036','043','045','046','047','048','049','051','052','060','062',
            '066','067','070','071','076','078','098','099')
  nms <- c('BLUEGILL','SUNFISH SPP.','CRAPPIE SPP.','BLACK CRAPPIE',
           'BROOK TROUT','PUMPKINSEED','BURBOT','CARP','LAKE HERRING',
           'SISCOWET','LAKE TROUT','SPLAKE','NORTHERN PIKE','BROWN TROUT',
           'RAINBOW TROUT','ROCK BASS','CHINOOK','COHO SALMON','PINK SALMON',
           'ATLANTIC SALMON','SMELT','STURGEON','LARGEMOUTH BASS',
           'SMALLMOUTH BASS','LAKE WHITEFISH','ROUND WHITEFISH','YELLOW PERCH',
           'WALLEYE','CATFISH','NA')
  x <- plyr::mapvalues(x,from=ifelse(useFirst,num1,num2),to=nms,warn=FALSE)
  x <- factor(x,levels=nms)
  x
}

## Convert state codes to words
iMvStates <- function(x) {
  tmp <- c("WI","MN","MI","WI/MN","WI/MI")
  x <- plyr::mapvalues(x,from=1:5,to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x
}

## Convert status codes to words
iMvStatus <- function(x) {
  tmp <- c("Complete","Incomplete")
  x <- plyr::mapvalues(x,from=1:2,to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x
}

## Create "waters" variable to identify if the fished area was in WI or not
iMvWaters <- function(x) {
  x <- dplyr::case_when(
    x=="MN" ~ "NON-WISCONSIN",
    x=="MI" ~ "NON-WISCONSIN",
    TRUE ~ "WISCONSIN"
  )
  factor(x,levels=c("WISCONSIN","NON-WISCONSIN"))
}

## Converts missing values to zeroes
iConvNA20 <- function(x) {
  case_when(
    is.na(x) ~ 0,
    TRUE ~ x
  )
}

## Adds whether a fish was clipped or not
iFinclipped <- function(x) {
  fc <- rep("FINCLIP",length(x))
  fc[fc %in% c('00: NONE','40: NOT EXAMINED')] <- "NO FINCLIP"
  fc
}

## Make New Years, Memorial Day, July 4th, and Labor Day as WEEKENDS
iHndlHolidays <- function(MONTH,MDAY,WDAY,DAYTYPE) {
  case_when(
    MONTH=="January" & MDAY==1 ~ "WEEKEND",                 # New Years Day
    MONTH=="May" & MDAY>=25 & WDAY=="Mon" ~ "WEEKEND",      # Memorial Day
    MONTH=="July" & MDAY==4 ~ "WEEKEND",                    # 4th of July
    MONTH=="September" & MDAY<=7 & WDAY=="Mon" ~ "WEEKEND", # Labor Day
    TRUE ~ as.character(DAYTYPE)
  )
}

## Compute hours of effort, put NAs if before start or after end of survey
##   period or if stop time is before start time.
iHndlHours <- function(STARTHH,STARTMM,STOPHH,STOPMM,DATE,SDATE,FDATE) {
  START <- STARTHH*60+STARTMM
  STOP <- STOPHH*60+STOPMM
  case_when(
    DATE < SDATE ~ NA_real_,   # Date before start date
    DATE > FDATE ~ NA_real_,   # Date after end date
    STOP < START ~ NA_real_,   # Stopped before started
    TRUE ~ (STOP-START)/60     # OK ... calc hours of effort
  )
}



##

sumEffort <- function(ints) {
  ### Restrict to only the variables needed here
  ### All hours for WI/MN or WI/MI are cut in half and then included separately
  ###   for Wisconsin and Minnesota (and will be designated as NON-WISCONSIN).
  ###   The hours (or half the original hours) are duplicated for NON-WISCONSIN
  ###   waters below with fx.
  f <- ints %>%
    dplyr::select(MONTH,WATERS,FISHERY,STATE,STATUS,DAYTYPE,PERSONS,HOURS) %>%
    dplyr::mutate(HOURS=ifelse(STATE %in% c("WI/MN","WI/MI"),0.5*HOURS,HOURS))
  
  ### Get the other half of the WI/MN & WI/MI effort
  fx <- f %>%
    dplyr::filter(STATE %in% c("WI/MN","WI/MI")) %>%
    dplyr::mutate(WATERS="NON-WISCONSIN")
  
  ### Combine back with original to get all interviews corrected for location
  ### Compute people-hours of fishing effort (in INDHRS)
  ### Compute hours for only completed trips (in CHOURS)
  ### Remove some variables
  ### Re-arrange
  f <- rbind(f,fx) %>%
    dplyr::mutate(INDHRS=PERSONS*HOURS,
                  CHOURS=ifelse(STATUS=="Complete",HOURS,NA)) %>%
    dplyr::select(-STATE,-PERSONS,-STATUS) %>%
    dplyr::arrange(MONTH,WATERS,FISHERY,DAYTYPE)
  
  ### Summarize the effort data.frame by strata  
  fsum <- f %>%
    dplyr::group_by(FISHERY,WATERS,DAYTYPE,MONTH) %>%
    dplyr::summarize(N=n(),
                     MTRIP=mean(CHOURS,na.rm=TRUE),
                     VHOURS=sum(HOURS^2,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE),
                     CHOURS=sum(CHOURS,na.rm=TRUE)) %>%
    dplyr::select(FISHERY,WATERS,DAYTYPE,MONTH,N,
                  HOURS,INDHRS,CHOURS,MTRIP,VHOURS) %>%
    dplyr::ungroup()

  ### Summarize total interviewed hours by MONTH and DAYTYPE
  ### *** THis is nints in SAS/IYOB code
  fsum2 <- ints %>%
    dplyr::group_by(MONTH,DAYTYPE) %>%
    dplyr::summarize(THOURS=sum(HOURS,na.rm=TRUE)) %>%
    dplyr::ungroup()
  
  ### Combine the summarized effort with sumInts
  f <- merge(fsum,fsum2,by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(PROP=HOURS/THOURS,
                  PARTY=INDHRS/HOURS) %>%
    dplyr::select(MONTH,DAYTYPE,WATERS,FISHERY,N,HOURS,VHOURS,
                  MTRIP,PROP,PARTY) %>%
    dplyr::arrange(MONTH,DAYTYPE,WATERS,FISHERY)
  f
}
