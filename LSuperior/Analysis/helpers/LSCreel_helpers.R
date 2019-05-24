## Load Packages ----
suppressPackageStartupMessages(library(FSA))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tables))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

options(max.print=2000)

## Main Helpers ----

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
    select(INTID,DATE,YEAR,WATERS,STATE,DAYTYPE,FISHERY,MONTH,DAY,SITE,
           STATUS,HOURS,PERSONS,RES,FISH,SUCCESS,
           contains("SPEC"),contains("CLIP"),contains("LEN"))
  d
}


##
sumInterviewedEffort <- function(dints) {
  ### Separate into only one and split states
  ### All hours for split states are cut in half
  f1 <- dplyr::filter(dints,!STATE %in% c("WI/MN","WI/MI"))
  f2 <- dplyr::filter(dints,STATE %in% c("WI/MN","WI/MI")) %>%
    dplyr::mutate(HOURS=0.5*HOURS)
  ### Duplicated f2 to get other half of HOURS, label as NON-WISCONSIN
  f3 <- dplyr::mutate(f2,WATERS="NON-WISCONSIN")

  ### Combine to get all interviews corrected for location
  ### Compute people-hours of fishing effort (in INDHRS)
  ### Compute hours for only completed trips (in CHOURS)
  f <- rbind(f1,f2,f3) %>%
    dplyr::mutate(INDHRS=PERSONS*HOURS,
                  CHOURS=ifelse(STATUS=="Complete",HOURS,NA))
  
  ### Summarize interviewed effort data by WATERS, DAYTPE, FISHERY, MONTH  
  fsum <- f %>%
    dplyr::group_by(YEAR,WATERS,DAYTYPE,FISHERY,MONTH) %>%
    dplyr::summarize(NINTS=n(),
                     MTRIP=mean(CHOURS,na.rm=TRUE),
                     VHOURS=sum(HOURS^2,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE),
                     CHOURS=sum(CHOURS,na.rm=TRUE)) %>%
    dplyr::select(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,
                  NINTS,HOURS,INDHRS,CHOURS,MTRIP,VHOURS) %>%
    as.data.frame()
  
  ### Summarize total interviewed hours by MONTH and DAYTYPE
  ### *** This is nints in SAS/IYOB code
  fsum2 <- dints %>%
    dplyr::group_by(MONTH,DAYTYPE) %>%
    dplyr::summarize(THOURS=sum(HOURS,na.rm=TRUE)) %>%
    as.data.frame()
  
  ### Combine the summarized effort with fsum2
  f <- merge(fsum,fsum2,by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(PROP=HOURS/THOURS,
                  PARTY=INDHRS/HOURS) %>%
    dplyr::select(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,
                  NINTS,HOURS,VHOURS,MTRIP,PROP,PARTY) %>%
    dplyr::arrange(WATERS,DAYTYPE,FISHERY,MONTH)
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
    as.data.frame()
  d
}


##
expandPressureCounts <- function(dcnts,cal) {
  dcnts <- dcnts %>%
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
                  VCOUNT=VCOUNT/NCOUNT*(DAYS^2)) %>%
    dplyr::select(YEAR,DAYTYPE,MONTH,NCOUNT,DAYS,COUNT,VCOUNT) %>%
    dplyr::arrange(YEAR,DAYTYPE,MONTH)
  dcnts
}


## Rearrange fish information from interviews
rearrangeFishInfo <- function(dints) {
  ## Get the main information about the interview
  mainInts <- dplyr::select(dints,INTID:HOURS)
  
  ## Isolate species, clips, and lengths and make long format
  specInts <- dplyr::select(dints,INTID,contains("SPEC")) %>%
    tidyr::gather(tmp,SPECCODE,-INTID) %>%
    dplyr::select(-tmp)
  
  ## Isolate clips, make long, change code to word, add clipped variable
  clipInts <- dplyr::select(dints,INTID,contains("CLIP")) %>%
    tidyr::gather(tmp,CLIPCODE,-INTID) %>%
    dplyr::select(-tmp)
  
  ## Isolate lengths, make long
  lenInts <- dplyr::select(dints,INTID,contains("LEN")) %>%
    tidyr::gather(tmp,LEN,-INTID) %>%
    dplyr::select(-tmp)
  
  ## Put species, clips, and lengths back together
  ## Reduce to only those where a species, clip, and length was recorded
  sclInts <- cbind(specInts,
                   clipInts[,"CLIPCODE",drop=FALSE],
                   lenInts[,"LEN",drop=FALSE]) %>%
    dplyr::filter(!is.na(SPECCODE),!is.na(CLIPCODE),!is.na(LEN))
  
  ## Expand records that were only counts of fish (when CLIPCODE==99)
  if (any(sclInts$CLIPCODE==99)) {
    ### isolate records to be expanded
    tmp <- dplyr::filter(sclInts,CLIPCODE=99)
    ### determine how many of each row to repeat, repeat those rows, 
    ###   replace CLIPCODE with 40 (for not examined), replace LEN with NA
    reprows <- rep(seq_len(nrow(tmp)),tmp$LEN)
    tmp2 <- tmp[reprows,] %>%
      dplyr::mutate(CLIPCODE=40,LEN=NA)
    ### combine originals without records that needed expanding, with expanded
    sclInts <- rbind(dplyr::filter(sclInts,CLIPCODE!=99),tmp2)
  }
  
  ## Add words from codes
  sclInts <- sclInts %>%
    dplyr::mutate(SPECIES=iMvSpecies(SPECCODE),
                  CLIP=iMvFinclips(CLIPCODE),
                  CLIPPED=iFinclipped(CLIP)) %>%
    dplyr::select(INTID,SPECCODE,SPECIES,CLIPCODE,CLIP,CLIPPED,LEN)
  
  ## Join back on the main interview information
  dplyr::right_join(mainInts,sclInts,by="INTID") %>%
    dplyr::arrange(INTID)
}


## Summarize harvest
sumHarvest <- function(d) {
  ## Compute harvest for each interview
  harv <- d %>%
    group_by(INTID,YEAR,WATERS,STATE,DAYTYPE,FISHERY,MONTH,DATE,HOURS,SPECIES) %>%
    summarize(HARVEST=n()) %>%
    ungroup()

  ### Separate into only one and split states
  ### All hours and harvest for split states are cut in half
  h1 <- dplyr::filter(harv,!STATE %in% c("WI/MN","WI/MI"))
  h2 <- dplyr::filter(harv,STATE %in% c("WI/MN","WI/MI")) %>%
    dplyr::mutate(HOURS=0.5*HOURS,HARVEST=0.5*HARVEST)
  ### Duplicated h2 to get other half of HOURS/HARVEST, label as NON-WISCONSIN
  h3 <- dplyr::mutate(h2,WATERS="NON-WISCONSIN")
  ### Combine to get all interviews corrected for location
  ### Add COVAR variable
  harv <- rbind(h1,h2,h3) %>%
    mutate(COVAR=HARVEST*HOURS)
  ### Summarize harvest by strata and species
  harv <- group_by(harv,YEAR,WATERS,DAYTYPE,FISHERY,MONTH,SPECIES) %>%
    summarize(VHARVEST=sum(HARVEST^2),
              HARVEST=sum(HARVEST),
              COVAR=sum(COVAR)) %>%
    as.data.frame() %>%
    select(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,SPECIES,HARVEST,VHARVEST,COVAR)
  harv
}

## Summarize Harvest and Effort
##   Merge harvest and effort data.frames
##   Replace variables with 0 if NINTS is <1 or NA
##   Calculate
##   Reduce variables and sort
sumHarvestEffort <- function(h,f) {
  hf <- merge(h,f,by=c("YEAR","WATERS","DAYTYPE","FISHERY","MONTH"),all=TRUE) %>%
    dplyr::mutate(HOURS=ifelse(is.na(NINTS) | NINTS==0,0,HOURS),
                  VHOURS=ifelse(is.na(NINTS) | NINTS==0,0,VHOURS),
                  HARVEST=ifelse(is.na(NINTS) | NINTS==0,0,HARVEST),
                  VHARVEST=ifelse(is.na(NINTS) | NINTS==0,0,VHARVEST),
                  COVAR=ifelse(is.na(NINTS) | NINTS==0,0,COVAR),
                  VHOURS=ifelse(NINTS==0,NA,
                                (VHOURS-(HOURS^2)/NINTS)/(NINTS-1)),
                  VHARVEST=ifelse(NINTS==0,NA,
                                  (VHARVEST-(HARVEST^2)/NINTS)/(NINTS-1)),
                  COVAR=ifelse(NINTS==0,NA,
                               (COVAR-HARVEST*HOURS/NINTS)/(NINTS-1)),
                  HRATE=HARVEST/HOURS,
                  MHOURS=HOURS/NINTS,
                  MHARV=HARVEST/NINTS,
                  VHRATE=ifelse(MHARV==0 & NINTS>1,0,
                                (VHARVEST/(MHARV^2))+(VHOURS/(MHOURS^2))-
                                  2*COVAR/MHARV/MHOURS),
                  VHRATE=(HRATE^2)*VHRATE/NINTS,
                  HARVEST=PHOURS*HRATE,
                  VHARVEST=(PHOURS^2)*VHRATE+(HRATE^2)*VPHOURS+VHRATE*VPHOURS) %>%
    dplyr::select(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,SPECIES,
                  NINTS,HARVEST,VHARVEST,INDHRS,HRATE,VHRATE) %>%
    dplyr::arrange(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,SPECIES)
  hf
}

## Summarize lengths by species, month, and var
## Note that this is generalized so that var can be either CLIPPED or CLIP
sumLengths <- function(d,var) {
  tmp <- rlang::quo_name(rlang::enquo(var))
  ## summarize by YEAR, SPECIES, MONTH, and "clip"
  lenSum1 <- dplyr::group_by(d,YEAR,SPECIES,MONTH,!!rlang::enquo(var)) %>% iSumLen()
  ## summarize by YEAR, SPECIES, and MONTH (across all "clip"s)
  lenSum2 <- group_by(d,YEAR,SPECIES,MONTH) %>% iSumLen() %>%
    mutate(TMP="TOTAL")
  names(lenSum2)[length(names(lenSum2))] <- tmp
  lenSum2 <- select(lenSum2,names(lenSum1))
  ## summarize by YEAR, SPECIES, and "clip" (across all MONTHs)
  lenSum3 <- group_by(d,YEAR,SPECIES,!!enquo(var)) %>% iSumLen() %>%
    mutate(MONTH="TOTAL") %>%
    select(names(lenSum1))
  ## summarize by YEAR and SPECIES (across all MONTHs and "clip"s)
  lenSum4 <- group_by(d,YEAR,SPECIES) %>% iSumLen() %>%
    mutate(MONTH="TOTAL",TMP="TOTAL")
  names(lenSum4)[length(names(lenSum4))] <- tmp
  lenSum4 <- select(lenSum4,names(lenSum1))
  ## put them all together
  lenSum <- rbind(lenSum1,lenSum2,lenSum3,lenSum4)
  if (tmp=="CLIPPED") {
    lenSum <- lenSum %>%
      mutate(CLIPPED=factor(CLIPPED,levels=c("NO FINCLIP","FINCLIP","TOTAL"))) %>%
      arrange(YEAR,SPECIES,MONTH,CLIPPED)
  } else {
    lenSum <- arrange(lenSum,YEAR,SPECIES,MONTH,CLIP)
  }
  ## remove rows that are duplicates of the previous row and the "clip" variable
  ## is "TOTAL" ... this indicates that the "clip" and TOTAL rows are the same
  dupes <- duplicated(lenSum[,-which(names(lenSum)==tmp)])
  dupes[lenSum[,which(names(lenSum)==tmp)]!="TOTAL"] <- FALSE
  lenSum <- dplyr::filter(lenSum,!dupes)
  ## return data.frame
  lenSum
}


writeDF <- function(x,wdir) {
  x1 <- deparse(substitute(x))
  write.csv(x,file=paste0(wdir,"/",x1,".csv"),row.names=FALSE,quote=FALSE)
}


## Internals for Mains ----

iSumLen <- function(dgb) {
  dplyr::summarize(dgb,n=n(),mnLen=mean(LEN,na.rm=TRUE),
                   seLen=FSA::se(LEN,na.rm=TRUE),varLen=var(LEN,na.rm=TRUE),
                   minLen=min(LEN,na.rm=TRUE),maxLen=max(LEN,na.rm=TRUE)) %>%
    as.data.frame()
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
           '22: D+LV+RV','23: D+LP','24: D+LV','40: NOT EXAMINED',
           '40: NOT EXAMINED',NA)
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=c(0:24,40,99,NA),
                                          to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp[!duplicated(tmp)])
  x  
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

## Convert "fishery" codes to words
iMvFishery <- function(x) {
  tmp <- c("COLDWATER-OPEN","WARMWATER-OPEN","ICE-STREAM MOUTH","ICE-WARMWATER",
           "ICE-BOBBING","BAD RIVER","NON-FISHING","SHORE","TRIBAL","COMBINED")
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=1:10,to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x
}

## Convert residency codes to words
iMvResidency <- function(x) {
  tmp <- c("Resident","Non-Resident","Resident/Non-Resident")
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=1:3,to=tmp,warn=FALSE)
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
  lvls <- c('LAKE TROUT','SISCOWET','ATLANTIC SALMON','BROOK TROUT',
            'BROWN TROUT','COHO SALMON','CHINOOK','PINK SALMON','RAINBOW TROUT',
            'SPLAKE','BURBOT','LAKE HERRING','LAKE WHITEFISH','ROUND WHITEFISH',
            'SMELT','STURGEON','WALLEYE','YELLOW PERCH','NORTHERN PIKE',
            'BLUEGILL','PUMPKINSEED','SUNFISH SPP.','BLACK CRAPPIE',
            'CRAPPIE SPP.','LARGEMOUTH BASS','ROCK BASS','SMALLMOUTH BASS',
            'CATFISH','CARP')
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=code,to=nms,warn=FALSE)
  x <- factor(x,levels=lvls)
  x
}

## Convert state codes to words
iMvStates <- function(x) {
  tmp <- c("WI","MN","MI","WI/MN","WI/MI")
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=1:5,to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x
}

## Convert status codes to words
iMvStatus <- function(x) {
  tmp <- c("Complete","Incomplete")
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=1:2,to=tmp,warn=FALSE)
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


showRoundedNumerics <- function(d,digits=3) {
  d[sapply(d,is.numeric)] <- lapply(d[sapply(d,is.numeric)],round,digits=digits)
  d
}
