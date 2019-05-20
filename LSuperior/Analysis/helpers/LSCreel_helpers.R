## Load Packages ----
suppressPackageStartupMessages(library(FSA))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tables))
suppressPackageStartupMessages(library(huxtable))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))


## Helper Functions ----

## Read and prepare the interview data file
readInterviewData <- function(LOC,SDATE,FDATE,
                              dropCLS=TRUE,dropHM=TRUE) {
  ## Add interview ID number
  ## Convert some codes to words
  ## Handle dates (find weekends and weekdays) & times (incl. hours of effort)
  d <- read.csv(paste0("data/",LOC,"ints.csv")) %>%
    dplyr::mutate(INTID=1:n(),
                  STATE=iMvStates(STATE),
                  WATERS=iMvWaters(STATE),
                  FISHERY=iMvFishery(FISHERY),
                  STATUS=iMvStatus(STATUS),
                  RES=iMvResidency(RES),
                  DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%y"),
                  YEAR=lubridate::year(DATE),
                  MONTH=lubridate::month(DATE,label=TRUE,abbr=FALSE),
                  WDAY=lubridate::wday(DATE,label=TRUE,abbr=TRUE),
                  DAYTYPE=iMvDaytype(WDAY,MONTH,DAY),
                  HOURS=iHndlHours(STARTHH,STARTMM,STOPHH,STOPMM,
                                   DATE,SDATE,FDATE))
  ## Drop hours and minutes variables if asked to
  if (dropHM) d <- dplyr::select(d,-(STARTMM:STOPHH))
  ## Drop CLIP, LEN, and SPEC variables that have no data, if asked to
  if (dropCLS) {
    allNA <- sapply(d,function(x) all(is.na(x)))
    allNAv <- names(d)[allNA]
    cat("The following were removed (as requested) because they were all NAs:\n")
    cat(paste(allNAv[grepl("CLIP",allNAv)],collapse=", "),"\n")
    cat(paste(allNAv[grepl("LEN",allNAv)],collapse=", "),"\n")
    cat(paste(allNAv[grepl("SPEC",allNAv)],collapse=", "),"\n")
    d <- d[,!allNA]
  }
  ## Rearrange variables
  d <- d %>%
    select(INTID,DATE,YEAR,MONTH,DAY,WDAY,DAYTYPE,
           WATERS,STATE,FISHERY,SITE,
           STATUS,RES,FISH,SUCCESS,PERSONS,HOURS,
           contains("SPEC"),contains("CLIP"),contains("LEN"))
  d
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
    dplyr::summarize(NINTS=n(),
                     MTRIP=mean(CHOURS,na.rm=TRUE),
                     VHOURS=sum(HOURS^2,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE),
                     CHOURS=sum(CHOURS,na.rm=TRUE)) %>%
    dplyr::select(FISHERY,WATERS,DAYTYPE,MONTH,NINTS,
                  HOURS,INDHRS,CHOURS,MTRIP,VHOURS) %>%
    dplyr::ungroup()
  
  ### Summarize total interviewed hours by MONTH and DAYTYPE
  ### *** This is nints in SAS/IYOB code
  fsum2 <- ints %>%
    dplyr::group_by(MONTH,DAYTYPE) %>%
    dplyr::summarize(THOURS=sum(HOURS,na.rm=TRUE)) %>%
    dplyr::ungroup()
  
  ### Combine the summarized effort with fsum2
  f <- merge(fsum,fsum2,by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(PROP=HOURS/THOURS,
                  PARTY=INDHRS/HOURS) %>%
    dplyr::select(MONTH,DAYTYPE,WATERS,FISHERY,NINTS,HOURS,VHOURS,
                  MTRIP,PROP,PARTY) %>%
    dplyr::arrange(MONTH,DAYTYPE,WATERS,FISHERY)
  f
}


## Read and prepare the interview data file
readPressureCountData <- function(LOC,SDATE,FDATE,dropHM=TRUE) {
  ###   Find various varsions of dates (note that DATE had to be handled
  ###     differently than above b/c four rather than two digits used here).
  ###   Convert missing COUNTs to zeroes
  ###   Calculate the "WAIT" time (hours at the site)
  ###   Convert average counts (the original COUNT variable) to "total effort"
  ###     during shift (by muliplying by the WAIT time) so that multiple shifts
  ###     on each day can be combined (from original SAS code).
  d <- read.csv(paste0("data/",LOC,"cnts.csv")) %>%
    dplyr::mutate(DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%Y"),
                  YEAR=lubridate::year(DATE),
                  MONTH=lubridate::month(DATE,label=TRUE,abbr=FALSE),
                  WDAY=lubridate::wday(DATE,label=TRUE,abbr=TRUE),
                  DAYTYPE=iMvDaytype(WDAY,MONTH,DAY),
                  COUNT=iConvNA20(COUNT),
                  WAIT=iHndlHours(STARTHH,STARTMM,STOPHH,STOPMM,
                                  DATE,SDATE,FDATE),
                  COUNT=COUNT*WAIT
    )
  ###   Drop hours and minutes variables if asked to do so
  if (dropHM) d <- dplyr::select(d,-(STARTMM:STOPHH))
  ###   Remove records with "bad" wait times
  ###   Narrow variable list down
  ###   Combine observations of WAIT and COUNT from multiple visits to the same
  ###     SITE within the same day
  d <- d %>%
    dplyr::filter(!is.na(WAIT)) %>%
    dplyr::select(DATE,YEAR,MONTH,DAY,WDAY,DAYTYPE,SITE,WAIT,COUNT) %>%
    dplyr::group_by(DATE,YEAR,MONTH,DAY,WDAY,DAYTYPE,SITE) %>%
    dplyr::summarize(WAIT=sum(WAIT),COUNT=sum(COUNT)) %>%
    dplyr::ungroup()
  d
}


##
expandPressureCounts <- function(counts,cal) {
  counts <- counts %>%
    ### Expands observed pressure counts to represent the entire day 
    dplyr::mutate(COUNT=COUNT*iMvDaylen(MONTH)/WAIT) %>%
    ### Computes daily pressure counts (across sites within days)
    dplyr::group_by(DATE,YEAR,MONTH,DAY,WDAY,DAYTYPE) %>%
    dplyr::summarize(WAIT=sum(WAIT),
                     COUNT=sum(COUNT)) %>%
    dplyr::ungroup() %>%
    ### Summarizes daily pressure counts by month and daytype
    dplyr::group_by(YEAR,MONTH,DAYTYPE) %>%
    dplyr::summarize(NCOUNT=n(),
                     VCOUNT=var(COUNT),
                     COUNT=mean(COUNT,na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    ### Expand by number of days in the month (in cal)
    merge(cal[,c("MONTH","DAYTYPE","DAYS")],by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(COUNT=COUNT*DAYS,
                  VCOUNT=VCOUNT/NCOUNT*(DAYS^2),
                  SDCOUNT=sqrt(VCOUNT)) %>%
    dplyr::select(YEAR,MONTH,DAYTYPE,NCOUNT,DAYS,COUNT,SDCOUNT,VCOUNT) %>%
    dplyr::arrange(YEAR,MONTH,DAYTYPE)
  counts
}



## Creates day lengths from month names
iMvDaylen <- function(x) {
  mos <- c("January","February","March","April","May","June",
           "July","August","September","October","November","December")
  lens <- c(0,0,0,0,16,16,16,16,16,0,0,0)
  x <- plyr::mapvalues(x,from=mos,to=lens,warn=FALSE)
  FSA::fact2num(x)
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
           '22: D+LV+RV','23: D+LP','24: D+LV','40: NOT EXAMINED',NA)
  x <- plyr::mapvalues(x,from=c(0:24,40,NA),to=tmp,warn=FALSE)
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
iMvSpecies <- function(x,which=1) {
  if (which==1) code <- c(2,3,4,5,6,7,11,13,33,34,35,36,43,45,46,47,48,49,
                          51,52,60,62,66,67,70,71,76,78,98,99)
  else if (which==2) code <- c('002','003','004','005','006','007','011','013',
                               '033','034','035','036','043','045','046','047',
                               '048','049','051','052','060','062','066','067',
                               '070','071','076','078','098','099')
  else code <- c('W09','W00','W02','W14','I22','W06','R01','M12',
                 'I04','I24','I23','I28','L02','I21','I19','W04',
                 'I16','I14','I12','I20','J01','B01','W12','W11',
                 'I05','I18','X15','X22','098','099')
  nms <- c('BLUEGILL','SUNFISH SPP.','CRAPPIE SPP.','BLACK CRAPPIE',
           'BROOK TROUT','PUMPKINSEED','BURBOT','CARP','LAKE HERRING',
           'SISCOWET','LAKE TROUT','SPLAKE','NORTHERN PIKE','BROWN TROUT',
           'RAINBOW TROUT','ROCK BASS','CHINOOK','COHO SALMON','PINK SALMON',
           'ATLANTIC SALMON','SMELT','STURGEON','LARGEMOUTH BASS',
           'SMALLMOUTH BASS','LAKE WHITEFISH','ROUND WHITEFISH','YELLOW PERCH',
           'WALLEYE','CATFISH','NA')
  x <- plyr::mapvalues(x,from=code,to=nms,warn=FALSE)
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
  dplyr::case_when(
    is.na(x) ~ 0,
    TRUE ~ x
  )
}

## Adds whether a fish was clipped or not
iFinclipped <- function(x) {
  tmp <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c('00: NONE','40: NOT EXAMINED') ~ "NO FINCLIP",
    TRUE ~ "FINCLIP"
  )
  factor(tmp,levels=c("FINCLIP","NO FINCLIP"))
}

## Make New Years, Memorial Day, July 4th, and Labor Day as WEEKENDS
iHndlHolidays <- function(mon,md,wd,dt) {
  dplyr::case_when(
    mon=="January" & md==1 ~ "WEEKEND",                # New Years Day
    mon=="May" & md>=25 & wd=="Mon" ~ "WEEKEND",       # Memorial Day
    mon=="July" & md==4 ~ "WEEKEND",                   # 4th of July
    mon=="September" & md<=7 & wd=="Mon" ~ "WEEKEND",  # Labor Day
    TRUE ~ as.character(dt)
  )
}

## Compute hours of effort, put NAs if before start or after end of survey
##   period or if stop time is before start time.
iHndlHours <- function(STARTHH,STARTMM,STOPHH,STOPMM,DATE,SDATE,FDATE) {
  START <- STARTHH*60+STARTMM
  STOP <- STOPHH*60+STOPMM
  dplyr::case_when(
    DATE < SDATE ~ NA_real_,   # Date before start date
    DATE > FDATE ~ NA_real_,   # Date after end date
    STOP < START ~ NA_real_,   # Stopped before started
    TRUE ~ (STOP-START)/60     # OK ... calc hours of effort
  )
}
