## Load Packages ----
library(FSA)
library(lubridate)
library(tables)
library(huxtable)
library(tidyr)
library(dplyr)


## Lookup Tables ----
### These are constants and lists that are used for lookups in the analysis code



## List months and the fishing day length (in hours)
mvDAYLEN <- list(MONTH= c("January","February","March","April","May","June",
                          "July","August","September","October","November",
                          "December"),
                 DAYLEN=c(0,0,0,0,16,16,16,16,16,0,0,0))


## Helper Functions ----

### Read and prepare the interview data file
###   Convert some codes to words
###   Handle dates (find weekends and weekdays) & times (incl. hours of effort)
readInterviewData <- function(LOC,SDATE,FDATE,
                              dropCLS=TRUE,dropHM=TRUE) {
  d <- read.csv(paste0("data/",LOC,"ints.csv")) %>%
    dplyr::mutate(STATE=iMvStates(STATE),
                  FISHERY=iMvFishery(FISHERY),
                  STATUS=iMvStatus(STATUS),
                  RES=iMvResidency(RES),
                  DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%y"),
                  YEAR=lubridate::year(DATE),
                  MONTH=lubridate::month(DATE,label=TRUE,abbr=FALSE),
                  MDAY=lubridate::mday(DATE),
                  WDAY=lubridate::wday(DATE,label=TRUE,abbr=TRUE),
                  DAYTYPE=iMvDaytype(WDAY),
                  DAYTYPE=iHndlHolidays(MONTH,MDAY,WDAY,DAYTYPE),
                  DAYTYPE=factor(DAYTYPE),
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


readPressureCountData <- function(LOC) {
  
}

## Convert DOW to weekend or weekdays
iMvDaytype <- function(x) {
  x <- plyr::mapvalues(x,from=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                       to=c("WEEKDAY","WEEKDAY","WEEKDAY","WEEKDAY","WEEKDAY",
                            "WEEKEND","WEEKEND"),warn=FALSE)
  x
}

## Convert "fishery" codes to "fishery" words
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


## Converts missing values to zeroes
convNA20 <- function(x) {
  case_when(
    is.na(x) ~ 0,
    TRUE ~ x
  )
}


## OLD STUFF FROM IYOB ----

#IYOB#> library(timeDate)
#IYOB#> library(zoo)
#IYOB#> library(plyr)
#IYOB#> library(htmlTable)
#IYOB#> library(reshape)
#IYOB#> library(Gmisc)




###############   FACTORS

#IYOB#> monthname <- factor(1:12,labels=month.name)

#IYOB#> f_daytype=factor(c(1:9),levels=c(1:9),labels=c("WEEKDAY","WEEKEND","TOTAL"),exclude=c(3:8))

#IYOB#> sppN1=c('W09','W00','W02','W14','I22','W06','R01','M12','I04','I24','I23','I28','L02','I21','I19','W04','I16','I14','I12','I20','J01','B01','W12','W11','I05','I18','X15','X22','098','099')
#IYOB#> sppN2=c('002','003','004','005','006','007','011','013','033','034','035','036','043','045','046','047','048','049','051','052','060','062','066','067','070','071','076','078','098','099')
#IYOB#> sppL=c('BLUEGILL','SUNFISH SPP.','CRAPPIE SPP.','BLACK CRAPPIE','BROOK TROUT','PUMPKINSEED','BURBOT','CARP','LAKE HERRING','SISCOWET','LAKE TROUT','SPLAKE','NORTHERN PIKE','BROWN TROUT','RAINBOW TROUT','ROCK BASS','CHINOOK','COHO SALMON','PINK SALMON','ATLANTIC SALMON','SMELT','STURGEON','LARGEMOUTH BASS','SMALLMOUTH BASS','LAKE WHITEFISH','ROUND WHITEFISH','YELLOW PERCH','WALLEYE','CATFISH','NA')
#IYOB#> spp=factor(sppN1,levels=sppN1,labels=sppL) ###### OR
#IYOB#> spp=factor(sppN2,levels=sppN2,labels=sppL)
#IYOB#> spp[which(sppN2=="099")]

#IYOB#> FINCLIPA_N=c(0:24,40)
#IYOB#> FINCLIPA_L=c('00: NONE','01: AD','02: AD+LV','03: AD+RV','04: AD+LP','05: AD+RP','06: LV','07: RV','08: LP','09: RP','10: LV+RV','11: RP+LV','12: AD+LV+RV','13: D','14: HATCHERY','15: LP+RV','16: D+RV','17: D+RP','18: AD+RM','19: LP+RM','20: LP+LV','21: D+AD','22: D+LV+RV','23: D+LP','24: D+LV','40: NOT EXAMINED')
#IYOB#> FINCLIPA=factor(FINCLIPA_N,levels=FINCLIPA_N,labels=FINCLIPA_L)
#IYOB#> FINCLIPA[which(FINCLIPA_N==40)]

#IYOB#> FINCLIPB<-factor(FINCLIPA_N,levels=FINCLIPA_N,labels=c("NO FINCLIP",rep("FINCLIP",24),"NO FINCLIP"))
#IYOB#> FINCLIPB[which(FINCLIPA_N==0)]

