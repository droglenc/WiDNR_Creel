#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Load Packages ---------------------------------------------------------------
rqrd <- c("FSA","lubridate","tidyr","dplyr","magrittr","huxtable",
          "kableExtra","captioner","ggplot2","patchwork","sugrrants","grid",
          "haven","knitr","here","readxl","tibble","RColorBrewer")
for (i in seq_along(rqrd)) suppressPackageStartupMessages(library(rqrd[i],
                                  character.only=TRUE))



## Main Helpers ----------------------------------------------------------------

## Read and prepare the interview data file
readInterviewData <- function(FN,RDIR,LOC,SDATE,FDATE,
                              dropCLS=TRUE,dropHM=TRUE) {
  
  FN <- file.path(RDIR,FN)
  if (tools::file_ext(FN)=="sas7bdat") d <- haven::read_sas(FN)
  else d <- read.csv(FN)
  d <- d %>%
    ## Add interview ID number
    dplyr::mutate(INTID=1:dplyr::n(),
                  ## Convert some codes to words
                  STATE=iMvStates(STATE),
                  WATERS=iMvWaters(STATE),
                  FISHERY=iMvFishery(FISHERY),
                  STATUS=iMvStatus(STATUS),
                  RES=iMvResidency(RES),
                  ## Handle dates (find weekends/days) & times (incl. hrs of effort)
                  DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%y"),
                  YEAR=lubridate::year(DATE),
                  MONTH=lubridate::month(DATE,label=TRUE,abbr=TRUE),
                  WDAY=lubridate::wday(DATE,label=TRUE,abbr=TRUE),
                  ## Find weekends/days
                  DAYTYPE=iMvDaytype(WDAY,MONTH,DAY),
                  ## Find hrs of effort
                  HOURS=iHndlHours(STARTHH,STARTMM,STOPHH,STOPMM,
                                   DATE,SDATE,FDATE))
  ## Handle management unit variable ... will add MUNIT if it is missing.
  d %<>% mutate(MUNIT=iMvMgmtUnit(.,LOC))
  ## Drop CLIP, LEN, and SPEC variables that have no data, if asked to
  if (dropCLS) {
    allNA <- sapply(d,function(x) {
      if (is.character(x)) all(is.na(x) | x=="")
      else all(is.na(x))
    })
    d <- d[,!allNA]
  }
  ## Rearrange variables
  d %<>% dplyr::select(INTID,DATE,YEAR,WATERS,MUNIT,STATE,DAYTYPE,FISHERY,
                       MONTH,DAY,SITE,STATUS,HOURS,STARTHH,STARTMM,STOPHH,STOPMM,
                       PERSONS,RES,FISH,SUCCESS,contains("SPEC"),
                       contains("CLIP"),contains("LEN"))
  ## Drop hours and minutes variables if asked to
  if (dropHM)  d %<>% dplyr::select(-STARTHH,-STARTMM,-STOPHH,-STOPMM)
  ## Return data.frame
  as.data.frame(d)
}

##
sumInterviewedEffort <- function(dints) {
  # All records where fishing was in one state
  f1 <- dplyr::filter(dints,!STATE %in% c("WI/MN","WI/MI"))
  # All records where fishing was in two states
  # Cut the fishing effort (HOURS) in half (apportioned to each state)
  f2 <- dplyr::filter(dints,STATE %in% c("WI/MN","WI/MI")) %>%
    dplyr::mutate(HOURS=0.5*HOURS)
  # Duplicate f2 to get other half of HOURS, label as NON-WISCONSIN
  f3 <- dplyr::mutate(f2,
                      WATERS="Non-WI",
                      MUNIT=ifelse(STATE=="WI/MN","MN","MI"))
  
  ### Combine to get all interviews corrected for location
  ### Compute people-hours of fishing effort (in INDHRS)
  ### Compute hours for only completed trips (in CHOURS)
  f <- rbind(f1,f2,f3) %>%
    dplyr::mutate(INDHRS=PERSONS*HOURS,
                  CHOURS=ifelse(STATUS=="Complete",HOURS,NA))
  
  ### Summarize interviewed effort data by WATERS, DAYTPE, FISHERY, MONTH  
  fsum <- f %>%
    dplyr::group_by(YEAR,WATERS,MUNIT,DAYTYPE,FISHERY,MONTH) %>%
    dplyr::summarize(NINTS=dplyr::n(),
                     MTRIP=mean(CHOURS,na.rm=TRUE),
                     VHOURS=sum(HOURS^2,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE),
                     CHOURS=sum(CHOURS,na.rm=TRUE)) %>%
    dplyr::select(YEAR,WATERS,MUNIT,DAYTYPE,FISHERY,MONTH,
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
    dplyr::select(YEAR,WATERS,MUNIT,DAYTYPE,FISHERY,MONTH,
                  NINTS,HOURS,VHOURS,MTRIP,PROP,PARTY) %>%
    dplyr::arrange(WATERS,MUNIT,DAYTYPE,FISHERY,MONTH)
  ### Return data.frame
  as.data.frame(f)
}


## Read and prepare the pressure counts data file
##  Note that counts (for Lake Superior) from the original file are average
##    number of parties present during the wait time, not total effort seen
##    during the wait time.
readPressureCountData <- function(FN,RDIR,LOC,SDATE,FDATE,dropHM=TRUE) {
  FN <- file.path(RDIR,FN)
  if (tools::file_ext(FN)=="sas7bdat") d <- haven::read_sas(FN)
  else d <- read.csv(FN)
  d <- d %>%
    # Find various varsions of dates (note that DATE had to be handled
    #   differently than above b/c four rather than two digits used here).
    dplyr::mutate(DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%Y"),
                  YEAR=lubridate::year(DATE),
                  MONTH=lubridate::month(DATE,label=TRUE,abbr=TRUE),
                  WDAY=lubridate::wday(DATE,label=TRUE,abbr=TRUE),
                  DAYTYPE=iMvDaytype(WDAY,MONTH,DAY),
                  # Convert missing COUNTs to zeroes
                  COUNT=iConvNA20(COUNT),
                  # Calculate the "WAIT" time (hours at the site)
                  WAIT=iHndlHours(STARTHH,STARTMM,STOPHH,STOPMM,
                                  DATE,SDATE,FDATE),
                  # Convert average counts (the original COUNT variable) to
                  # "total effort" during shift (by muliplying by the WAIT time)
                  # so that multiple shifts on each day can be combined (from
                  # original SAS code).
                  COUNT=COUNT*WAIT
    )
  # Drop hours and minutes variables if asked to do so
  if (dropHM) d <- dplyr::select(d,-(STARTMM:STOPHH))
  d %<>% 
    # Remove records with "bad" wait times
    dplyr::filter(!is.na(WAIT)) %>%
    # Narrow variable list down
    dplyr::select(YEAR,MONTH,DAY,WDAY,DAYTYPE,SITE,WAIT,COUNT) %>%
    # Combine observations of WAIT and COUNT from multiple visits to the same
    #   SITE within the same day
    dplyr::group_by(YEAR,MONTH,DAY,WDAY,DAYTYPE,SITE) %>%
    dplyr::summarize(WAIT=sum(WAIT),COUNT=sum(COUNT))
  # Return data.frame
  as.data.frame(d)
}


## Expand pressure counts from observed days/times to daylengths & days/month
expandPressureCounts <- function(dcnts,cal) {
  ## Isolate daylengths for each month
  tmp <- cal[,c("MONTH","DAYLEN")]
  tmp <- tmp[!duplicated(tmp),]
  dcnts %<>%
    ### Adds a temporary day length variable (from cal) to each count
    right_join(tmp,by="MONTH") %>%
    ### Expands observed pressure counts to represent the entire day 
    dplyr::mutate(COUNT=COUNT*DAYLEN/WAIT) %>%
    ### Computes daily pressure counts (across sites within days)
    dplyr::group_by(YEAR,MONTH,DAY,WDAY,DAYTYPE) %>%
    dplyr::summarize(WAIT=sum(WAIT),
                     COUNT=sum(COUNT)) %>%
    dplyr::ungroup() %>%
    ### Summarizes daily pressure counts by month and daytype
    dplyr::group_by(YEAR,MONTH,DAYTYPE) %>%
    dplyr::summarize(NCOUNT=dplyr::n(),
                     VCOUNT=var(COUNT),
                     COUNT=mean(COUNT,na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    ### Expand by number of days in the month (in cal)
    merge(cal[,c("MONTH","DAYTYPE","DAYS")],by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(COUNT=COUNT*DAYS,
                  VCOUNT=VCOUNT/NCOUNT*(DAYS^2)) %>%
    dplyr::select(YEAR,MONTH,DAYTYPE,NCOUNT,DAYS,COUNT,VCOUNT)
  ## Find totals across DAYTYPEs
  dcnts1 <- dplyr::group_by(dcnts,YEAR,MONTH) %>%
    dplyr::summarize(NCOUNT=sum(NCOUNT,na.rm=TRUE),
                     DAYS=sum(DAYS,na.rm=TRUE),
                     COUNT=sum(COUNT,na.rm=TRUE),
                     VCOUNT=sum(VCOUNT,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(dcnts)) %>%
    as.data.frame()
  dcnts <- rbind(dcnts,dcnts1)
  ## Find totals across all MONTHs
  dcnts2 <- dplyr::group_by(dcnts,YEAR,DAYTYPE) %>%
    dplyr::summarize(NCOUNT=sum(NCOUNT,na.rm=TRUE),
                     DAYS=sum(DAYS,na.rm=TRUE),
                     COUNT=sum(COUNT,na.rm=TRUE),
                     VCOUNT=sum(VCOUNT,na.rm=TRUE)) %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(dcnts)) %>%
    as.data.frame()
  dcnts <- rbind(dcnts,dcnts2)
  ## Convert to SDs and rearrange variables
  dcnts %<>% dplyr::mutate(SDCOUNT=sqrt(VCOUNT)) %>%
    dplyr::select(YEAR,MONTH,DAYTYPE,NCOUNT,DAYS,COUNT,SDCOUNT,VCOUNT) %>%
    dplyr::mutate(DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All")),
                  MONTH=iOrderMonths(MONTH,addAll=TRUE)) %>%
    dplyr::arrange(YEAR,MONTH,DAYTYPE) %>%
    as.data.frame()
  ## Return data.frame
  dcnts
}


## Summarized total fishing effort by strata
sumEffort <- function(ieff,pct) {
  ## Remove 'All' DAYTYPE and MONTH rows from pressureCount results
  pct %<>% dplyr::filter(DAYTYPE!="All") %>%
    dplyr::filter(MONTH!="All")
  # Combine interview effort and pressure counts with new calculations ...
  #   intermediate (non-returned) calculations are described here
  #   * NCOUNT: Number of days clerk estimated pressure counts
  #   * DAYS: Number of days in the month
  #   * COUNT: Total pressure count (number of boats)
  #   * VCOUNT: Variance of pressure count (SD^2)
  #   * PROP: Proportion of total interviewed effort for month-daytype that is in
  #           a given waters-fishery.
  eff <- merge(ieff,pct,by=c("YEAR","MONTH","DAYTYPE")) %>%
    dplyr::mutate(PHOURS=COUNT*PROP,
                  VPHOURS=VCOUNT*(PROP^2),
                  TRIPS=PHOURS/MTRIP,
                  VTRIPS=VPHOURS/(MTRIP^2),
                  INDHRS=PHOURS*PARTY,
                  VINDHRS=VPHOURS*(PARTY^2)) %>%
    dplyr::group_by(YEAR,WATERS,MUNIT,FISHERY,MONTH,DAYTYPE) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     VHOURS=sum(VHOURS,na.rm=TRUE),
                     PHOURS=sum(PHOURS,na.rm=TRUE),
                     VPHOURS=sum(VPHOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE),
                     VINDHRS=sum(VINDHRS,na.rm=TRUE),
                     TRIPS=sum(TRIPS,na.rm=TRUE),
                     VTRIPS=sum(VTRIPS,na.rm=TRUE)) %>%
    as.data.frame()
  
  ## Summarize across Daytypes
  eff1 <- dplyr::group_by(eff,YEAR,WATERS,MUNIT,FISHERY,MONTH) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     VHOURS=sum(VHOURS,na.rm=TRUE),
                     PHOURS=sum(PHOURS,na.rm=TRUE),
                     VPHOURS=sum(VPHOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE),
                     VINDHRS=sum(VINDHRS,na.rm=TRUE),
                     TRIPS=sum(TRIPS,na.rm=TRUE),
                     VTRIPS=sum(VTRIPS,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(eff)) %>%
    as.data.frame()
  ## Combine those summaries to original data.frame
  eff <- rbind(eff,eff1)
  
  ## Summarize across Months
  eff2 <- dplyr::group_by(eff,YEAR,WATERS,MUNIT,FISHERY,DAYTYPE) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     VHOURS=sum(VHOURS,na.rm=TRUE),
                     PHOURS=sum(PHOURS,na.rm=TRUE),
                     VPHOURS=sum(VPHOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE),
                     VINDHRS=sum(VINDHRS,na.rm=TRUE),
                     TRIPS=sum(TRIPS,na.rm=TRUE),
                     VTRIPS=sum(VTRIPS,na.rm=TRUE)) %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(eff)) %>%
    as.data.frame()
  ## Combine those summaries to original data.frame
  eff <- rbind(eff,eff2)
  
  ## Summarize across Fisheries
  eff3 <- dplyr::group_by(eff,YEAR,WATERS,MUNIT,MONTH,DAYTYPE) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     VHOURS=sum(VHOURS,na.rm=TRUE),
                     PHOURS=sum(PHOURS,na.rm=TRUE),
                     VPHOURS=sum(VPHOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE),
                     VINDHRS=sum(VINDHRS,na.rm=TRUE),
                     TRIPS=sum(TRIPS,na.rm=TRUE),
                     VTRIPS=sum(VTRIPS,na.rm=TRUE)) %>%
    dplyr::mutate(FISHERY="All") %>%
    dplyr::select(names(eff)) %>%
    as.data.frame()
  ## Combine those summaries to original data.frame
  eff <- rbind(eff,eff3) %>%
    ## Add on Persons per party and mean trip length
    dplyr::mutate(PARTY=INDHRS/PHOURS,MTRIP=PHOURS/TRIPS) %>%
    ## Convert variances to standard deviations
    dplyr::mutate(SDPHOURS=sqrt(VPHOURS),
                  SDINDHRS=sqrt(VINDHRS),
                  SDTRIPS=sqrt(VTRIPS)) %>%
    ## Put variables in specific order
    dplyr::select(YEAR,WATERS,MUNIT,FISHERY,MONTH,DAYTYPE,
                  PHOURS,SDPHOURS,PARTY,INDHRS,SDINDHRS,MTRIP,TRIPS,SDTRIPS,
                  NINTS,HOURS,VHOURS,VPHOURS,VINDHRS,VTRIPS) %>%
    ## Arrange
    dplyr::arrange(YEAR,WATERS,MUNIT,FISHERY,MONTH,DAYTYPE)
  # return final data.frame
  eff
}


## Rearrange fish information from interviews
##   Note that FINCLIP=99 means length field has number of fish harvested.
rearrangeFishInfo <- function(dints) {
  ## Get the main information about the interview
  mainInts <- dplyr::select(dints,INTID:HOURS)
  
  ## Isolate species, clips, and lengths and make long format
  ## Change missing SPECCODEs to actual NAs
  specInts <- dplyr::select(dints,INTID,contains("SPEC")) %>%
    tidyr::gather(tmp,SPECCODE,-INTID) %>%
    dplyr::select(-tmp) %>%
    dplyr::mutate(SPECCODE=ifelse(SPECCODE=="",NA,SPECCODE))
  
  ## Isolate clips, make long, change code to word, add clipped variable
  clipInts <- dplyr::select(dints,INTID,contains("CLIP")) %>%
    tidyr::gather(tmp,CLIPCODE,-INTID) %>%
    dplyr::select(-tmp)
  
  ## Isolate lengths, make long
  lenInts <- dplyr::select(dints,INTID,contains("LEN")) %>%
    tidyr::gather(tmp,LEN,-INTID) %>%
    dplyr::select(-tmp)
  
  ## Put species, clips, and lengths back together
  ## Reduce to only those where a species, clip, or length was recorded (this
  ##   allows for one to be missing ... most commonly a length)
  sclInts <- cbind(specInts,
                   clipInts[,"CLIPCODE",drop=FALSE],
                   lenInts[,"LEN",drop=FALSE]) %>%
    dplyr::filter(!(is.na(SPECCODE) & is.na(CLIPCODE) & is.na(LEN)))
  
  ## Expand records that were only counts of fish (when CLIPCODE==99 or when
  ## CLIPCODE='099' ... extra work is needed here because the CLIPCODE can be
  ## either numeric or character depending on original file type)
  if (is.character(sclInts$CLIPCODE)) {
    if (any(sclInts$CLIPCODE=='099',na.rm=TRUE)) {
      ### isolate records to be expanded
      tmp <- dplyr::filter(sclInts,CLIPCODE=='099')
      ### determine how many of each row to repeat, repeat those rows, 
      ###   replace CLIPCODE with '040' (for not examined), replace LEN with NA
      reprows <- rep(seq_len(nrow(tmp)),tmp$LEN)
      tmp2 <- tmp[reprows,] %>%
        dplyr::mutate(CLIPCODE='040',LEN=NA)
      ### combine originals without records that needed expanding, with expanded
      sclInts <- rbind(dplyr::filter(sclInts,CLIPCODE!='099'),tmp2)
    }
  } else {
    if (any(sclInts$CLIPCODE==99,na.rm=TRUE)) {
      ### isolate records to be expanded
      tmp <- dplyr::filter(sclInts,CLIPCODE==99)
      ### determine how many of each row to repeat, repeat those rows, 
      ###   replace CLIPCODE with 40 (for not examined), replace LEN with NA
      reprows <- rep(seq_len(nrow(tmp)),tmp$LEN)
      tmp2 <- tmp[reprows,] %>%
        dplyr::mutate(CLIPCODE=40,LEN=NA)
      ### combine originals without records that needed expanding, with expanded
      sclInts <- rbind(dplyr::filter(sclInts,CLIPCODE!=99),tmp2)
    }
  }
  
  ## Add words from codes
  sclInts %<>% dplyr::mutate(SPECIES=iMvSpecies(SPECCODE),
                             CLIP=iMvFinclips(CLIPCODE),
                             CLIPPED=iFinclipped(CLIP)) %>%
    dplyr::select(INTID,SPECCODE,SPECIES,CLIPCODE,CLIP,CLIPPED,LEN)
  
  ## Join back on the main interview information
  tmp <- dplyr::right_join(mainInts,sclInts,by="INTID") %>%
    dplyr::arrange(INTID)
  
  ## Return data.frame
  as.data.frame(tmp)
}


## Summarize observed harvest by strata and species
sumObsHarvest <- function(d) {
  ## Compute harvest for each interview
  harv <- d %>%
    dplyr::group_by(INTID,YEAR,WATERS,MUNIT,STATE,DAYTYPE,
                    FISHERY,MONTH,DATE,HOURS,SPECIES) %>%
    dplyr::summarize(HARVEST=dplyr::n()) %>%
    dplyr::ungroup()

  ## Harvest for when fishing only in one state
  h1 <- dplyr::filter(harv,!STATE %in% c("WI/MN","WI/MI"))
  ## Harvest for when fishing in more than one state ... harvest cut in half
  h2 <- dplyr::filter(harv,STATE %in% c("WI/MN","WI/MI")) %>%
    dplyr::mutate(HOURS=0.5*HOURS,HARVEST=0.5*HARVEST)
  ### Duplicated h2 to get other half of HOURS/HARVEST, label as Non-WI
  h3 <- dplyr::mutate(h2,
                      WATERS="Non-WI",
                      MUNIT=ifelse(STATE=="WI/MN","MN","MI"))
  ### Combine to get all interviews corrected for location
  ### Add COVAR variable
  harv <- rbind(h1,h2,h3) %>%
    dplyr::mutate(COVAR=HARVEST*HOURS)
  ### Summarize harvest by strata and species
  harv %<>% dplyr::group_by(YEAR,WATERS,MUNIT,DAYTYPE,FISHERY,MONTH,SPECIES) %>%
    dplyr::summarize(VHARVEST=sum(HARVEST^2,na.rm=TRUE),
                     HARVEST=sum(HARVEST,na.rm=TRUE),
                     COVAR=sum(COVAR,na.rm=TRUE)) %>%
    as.data.frame() %>%
    dplyr::select(YEAR,WATERS,MUNIT,DAYTYPE,FISHERY,MONTH,SPECIES,
                  HARVEST,VHARVEST,COVAR)
  ### Return data.frame
  as.data.frame(harv)
}

## Summarize Harvest and Effort
sumHarvestEffort <- function(h,f) {
  ##   Merge harvest and effort data.frames
  hf <- merge(h,f,by=c("YEAR","WATERS","MUNIT","DAYTYPE","FISHERY","MONTH"),
              all=TRUE) %>%
    ## Remove for SPECIES that do not exist
    dplyr::filter(!is.na(SPECIES)) %>%
    ## Replace variables with 0 if NINTS is =0 or NA
    ## Calculate the appropriate variances and covariances
    ##   NINTS= Number of interviews
    ##   HARVEST= Total estimated harvest
    ##   VHARVEST= Variance of total estimated harvest
    ##   INDHRS= Hours of fishing effort for all individuals
    dplyr::mutate(HOURS=ifelse(is.na(NINTS) | NINTS==0,NA,HOURS),
                  VHOURS=ifelse(is.na(NINTS) | NINTS==0,NA,VHOURS),
                  VHOURS=(VHOURS-(HOURS^2)/NINTS)/(NINTS-1),
                  HARVEST=ifelse(is.na(NINTS) | NINTS==0,NA,HARVEST),
                  VHARVEST=ifelse(is.na(NINTS) | NINTS==0,NA,VHARVEST),
                  VHARVEST=(VHARVEST-(HARVEST^2)/NINTS)/(NINTS-1),
                  COVAR=ifelse(is.na(NINTS) | NINTS==0,NA,COVAR),
                  COVAR=(COVAR-HARVEST*HOURS/NINTS)/(NINTS-1),
                  MHOURS=HOURS/NINTS,
                  MHARV=HARVEST/NINTS,
                  HRATE=HARVEST/HOURS,
                  VHRATE=ifelse(MHARV==0 & NINTS>1,0,
                                (VHARVEST/(MHARV^2))+(VHOURS/(MHOURS^2))-
                                  2*COVAR/MHARV/MHOURS),
                  VHRATE=(HRATE^2)*VHRATE/NINTS,
                  HARVEST=PHOURS*HRATE,
                  VHARVEST=(PHOURS^2)*VHRATE+(HRATE^2)*VPHOURS+VHRATE*VPHOURS) %>%
    ## Note that HRATE, VHRATE, MHOURS, MHARV are intermediate values &
    ##   are not returned:
    dplyr::select(YEAR,WATERS,MUNIT,DAYTYPE,FISHERY,MONTH,SPECIES,
                  HARVEST,VHARVEST,INDHRS) %>%
    as.data.frame() %>%
    droplevels()

  ## Summarizes across day-types
  hf1 <- dplyr::group_by(hf,YEAR,WATERS,MUNIT,FISHERY,SPECIES,MONTH) %>%
    dplyr::summarize(HARVEST=sum(HARVEST,na.rm=TRUE),
                     VHARVEST=sum(VHARVEST,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(hf)) %>%
    as.data.frame()
  ## Combine those two summaries to original data.frame
  hf <- rbind(hf,hf1)
  ## Summarizes across months
  hf2 <- dplyr::group_by(hf,YEAR,WATERS,MUNIT,FISHERY,SPECIES,DAYTYPE) %>%
    dplyr::summarize(HARVEST=sum(HARVEST,na.rm=TRUE),
                     VHARVEST=sum(VHARVEST,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(hf)) %>%
    as.data.frame()
  ## Combine those two summaries to original data.frame
  hf <- rbind(hf,hf2)
  ## Summarizes across fisheries
  hf3 <- dplyr::group_by(hf,YEAR,WATERS,MUNIT,SPECIES,MONTH,DAYTYPE) %>%
    dplyr::summarize(HARVEST=sum(HARVEST,na.rm=TRUE),
                     VHARVEST=sum(VHARVEST,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    dplyr::mutate(FISHERY="All") %>%
    dplyr::select(names(hf)) %>%
    as.data.frame()
  ## Combine those two summaries to original data.frame
  hf <- rbind(hf,hf3) %>%
    ## Add on harvest rate and SD of harvees
    dplyr::mutate(HRATE=HARVEST/INDHRS,
                  SDHARVEST=sqrt(VHARVEST)) %>%
    ## Rearrange variables and rows
    dplyr::select(YEAR:SPECIES,INDHRS,HARVEST,SDHARVEST,VHARVEST,HRATE) %>%
    dplyr::arrange(YEAR,WATERS,MUNIT,DAYTYPE,FISHERY,SPECIES,MONTH)
  ## Return data.frame
  hf
}

## Summarize lengths by species, month, and var
## Note that this is generalized so that var can be either CLIPPED or CLIP
sumLengths <- function(d,var) {
  tmp <- rlang::quo_name(rlang::enquo(var))
  ## summarize by YEAR, SPECIES, MONTH, and "clip"
  lenSum1 <- dplyr::group_by(d,YEAR,SPECIES,MONTH,!!rlang::enquo(var)) %>%
    iSumLen()
  ## summarize by YEAR, SPECIES, and MONTH (across all "clip"s)
  lenSum2 <- dplyr::group_by(d,YEAR,SPECIES,MONTH) %>% iSumLen() %>%
    dplyr::mutate(TMP="All")
  names(lenSum2)[length(names(lenSum2))] <- tmp
  lenSum2 <- dplyr::select(lenSum2,names(lenSum1))
  ## summarize by YEAR, SPECIES, and "clip" (across all MONTHs)
  lenSum3 <- dplyr::group_by(d,YEAR,SPECIES,!!enquo(var)) %>% iSumLen() %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(lenSum1))
  ## summarize by YEAR and SPECIES (across all MONTHs and "clip"s)
  lenSum4 <- dplyr::group_by(d,YEAR,SPECIES) %>% iSumLen() %>%
    dplyr::mutate(MONTH="All",TMP="All")
  names(lenSum4)[length(names(lenSum4))] <- tmp
  lenSum4 <- dplyr::select(lenSum4,names(lenSum1))
  ## put them all together
  lenSum <- rbind(lenSum1,lenSum2,lenSum3,lenSum4) %>%
    mutate(SPECIES=iMvSpecies(SPECIES),
           MONTH=iOrderMonths(MONTH,addAll=TRUE))
  if (tmp=="CLIPPED") {
    lenSum %<>%
      dplyr::mutate(CLIPPED=factor(CLIPPED,levels=c("No Clip","Clip","All"))) %>%
      dplyr::arrange(YEAR,SPECIES,MONTH,CLIPPED)
  } else {
    lenSum %<>% dplyr::arrange(YEAR,SPECIES,MONTH,CLIP)
  }
  ## Remove rows that are duplicates of a previous row for just the summary vars
  lenSum %<>% filter(FSA::repeatedRows2Keep(.,
                      cols2use=c("n","mnLen","sdLen","seLen","minLen","maxLen")))
  ## return data.frame
  as.data.frame(lenSum)
}

addWeights <- function(d,RDIR,YEAR) {
  ## Read in the length-weight regression results
  lwregs <- readxl::read_excel(file.path(RDIR,
                               paste0("LWRegressions_",YEAR,".xlsx"))) %>%
    ## make sure capitalization is the same as in SPECIES
    mutate(SPECIES2=FSA::capFirst(SPECIES)) %>%
    select(SPECIES2,a,b)
  
  ## Temporarily create SPECIES2 to handle different eqnfor clipped lake trout
  d %<>% mutate(SPECIES2=as.character(SPECIES))
  d$SPECIES2[d$SPECIES2=="Lake Trout" & d$CLIPPED=="Clip"] <- "Lake Trout (hatchery)"
  ## Temporarily append a and b values for each fish according to its species
  d %<>% left_join(lwregs,by="SPECIES2") %>%
    mutate(WT=round(exp(a)*(LEN*25.4)^b,0)) %>%
    select(-SPECIES2,-a,-b)
}


## Tables ----------------------------------------------------------------------
tableCaptions <- function() {
  tables <- captioner::captioner(prefix="Table")
  tables(name="Table1",
         caption=paste("Number of days by day type and assumed fishing day",
                       "length (h) for each month during the sampling period.",
                       "These values were used to expand sampled observations",
                       "to entire population."))
  tables(name="Table2",
         caption=paste("Number of interviews (N) and hours of interviewed effort",
                       "(Hrs) by state, day type, type of fishery, and month."))
  tables(name="Table3",
         caption=paste("Number of day sampled and total party-hours of pressure",
                       "by month and day type (includes non-fishing effort)."))
  tables(name="Table4",
         caption=paste("Total Party-hours, persons per party, total individual-hours,",
                       "mean trip length, and total number of trips by waters,",
                       "fishery type, month, and day type. 'All' results are",
                       "shown only for those species found in multiple months."))
  tables(name="Table5",
         caption=paste("Total harvest and harvest rates based on fishery-specific",
                       "angling hours by waters, fishery, species, month and",
                       "day type. 'All' results are shown only for those",
                       "species found in multiple months."))
  tables(name="Table6",
         caption=paste("Summary statistics of all measured fish by species,",
                       "month, and whether clipped or not. 'All' results are",
                       "shown only in those months where both clipped and",
                       "unclipped fish were observed."))
  tables(name="Table7",
         caption=paste0("Summary statistics of measured fish by species, month,",
                        " and whether clipped or not. Only species for which some",
                        " fin-clipped fish were recorded are shown (otherwise see ",
                        tables('Table6',display='cite'),"). 'All' results are",
                        " shown only in those months where both clipped and",
                        " unclipped fish were observed."))
  tables(name="Table8",
         caption=paste("Number of fish examined for fin clips by species,",
                       "month, and clip. 'All' results are shown only for those",
                       "species where both clipped and unclipped fish were",
                       "observed."))
  tables(name="Table9",
         caption="Detailed listing of all measured fish.")
  tables
}
table1 <- function(fnpre,calSum) {
  ## Prepare data.frame for huxtable
  calTbl1 <- calSum %>%
    ## Remove the year
    dplyr::select(-YEAR) %>%
    ## Create columns of weekdays and weekends
    tidyr::spread(DAYTYPE,DAYS) %>%
    ## Add a TOTAL days column
    ## Convert months to a character type (needed to add the total row later)
    dplyr::mutate(All=Weekday+Weekend,
                  MONTH=as.character(MONTH)) %>%
    ## Rearrange and rename some columns
    dplyr::select(MONTH,Weekday,Weekend,All,DAYLEN) %>%
    dplyr::rename(`Length`=DAYLEN) %>%
    ##  Add on a total row with a 'All' label
    dplyr::bind_rows(dplyr::summarize_at(.,vars(Weekday:All),'sum')) %>%
    dplyr::mutate(MONTH=replace(MONTH,is.na(MONTH),"All"))
  names(calTbl1)[which(names(calTbl1)=="All")] <- "All Days"
  
  ## Make the huxtable
  calTbl2 <- as_hux(calTbl1,add_colnames=TRUE) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    # right align all but leftmost column
    set_align(row=everywhere,col=-1,value="right") %>%
    # Top label covers columns 2-4, centered, line underneath it
    rbind(c("","DAY TYPE","","","Day"),.) %>%
    merge_cells(row=1,col=2:4) %>% 
    set_align(row=1,col=everywhere,value="center") %>%
    set_bottom_border(row=1,col=2,value=1) %>%
    # Extra space above last (TOTAL) row
    set_top_padding(row=final(1),col=everywhere,value=10) %>%
    # Sets table & column widths
    set_width(0.4) %>%
    set_col_width(col=c(.2,.2,.2,.2,.2)) %>%
    iFinishTable(labelsRow=2,labelsCol=1)
  calTbl2
}


table2 <- function(d) {
  # Prepare data.frame for huxtable
  ## Summarizes the number of OBSERVED interviews (NINTS) and total reported
  ## hours of fishing effort (HOURS) by "strata" (!!STATE!!, DAYTYPE, FISHERY,
  ## MONTH). This is observed data, not yet expanded to all days in month/year.
  tmp <- d %>%
    dplyr::group_by(STATE,DAYTYPE,FISHERY,MONTH,.drop=FALSE) %>%
    dplyr::summarize(NINTS=n(),HOURS=sum(HOURS)) %>%
    dplyr::select(STATE,DAYTYPE,FISHERY,MONTH,NINTS,HOURS) %>%
    as.data.frame()

  ## Summarize by month
  tmp1 <- dplyr::group_by(tmp,STATE,DAYTYPE,FISHERY) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE)) %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(tmp)) %>%
    as.data.frame()
  tmp <- rbind(tmp,tmp1)
  ## Summarize by fishery
  tmp1 <- dplyr::group_by(tmp,STATE,DAYTYPE,MONTH) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE)) %>%
    dplyr::mutate(FISHERY="All") %>%
    dplyr::select(names(tmp)) %>%
    as.data.frame()
  tmp <- rbind(tmp,tmp1)
  ## Summarize by daytype
  tmp1 <- dplyr::group_by(tmp,STATE,FISHERY,MONTH) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(tmp)) %>%
    as.data.frame()
  tmp <- rbind(tmp,tmp1)
  ## Summarize by state (make a catch if only one state)
  if (length(unique(tmp$STATE))>1) {
    tmp1 <- dplyr::group_by(tmp,DAYTYPE,FISHERY,MONTH) %>%
      dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                       HOURS=sum(HOURS,na.rm=TRUE)) %>%
      dplyr::mutate(STATE="All") %>%
      dplyr::select(names(tmp)) %>%
      as.data.frame()
    tmp <- rbind(tmp,tmp1)
  }
  
  mos <- as.character(unique(tmp$MONTH))
  nms <- paste(rep(mos,each=2),c("NINTS","HOURS"),sep=".")
  
  ## Convert to wide format
  tmpTbl1 <- tmp %>%
    tidyr::gather(temp,value,NINTS:HOURS) %>%
    tidyr::unite(temp1,MONTH,temp,sep=".") %>%
    tidyr::spread(temp1,value) %>%
    dplyr::select(STATE,DAYTYPE,FISHERY,nms) %>%
    arrange(STATE,DAYTYPE,FISHERY) %>%
    ## Remove repeated items in the first three variables
    dplyr::mutate(STATE=ifelse(!FSA::repeatedRows2Keep(.,cols2use="STATE"),
                                "",as.character(STATE)),
                  DAYTYPE=ifelse(!FSA::repeatedRows2Keep(.,cols2use="DAYTYPE"),
                                 "",as.character(DAYTYPE)))
  
  ## Rows with "All" (except last) get extra space below
  allRows <- which(tmpTbl1$FISHERY=="All")
  allRows <- allRows[-length(allRows)]
  ## Rows that have a state name (except first) get extra space above
  breakRows <- which(tmpTbl1$STATE!="")[-1]

  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_left_padding(row=everywhere,col=ends_with("NINTS"),value=15) %>%
    set_bottom_padding(row=allRows,col=everywhere,value=10) %>%
    set_top_padding(row=breakRows,col=everywhere,value=15) %>%
    # Convert NAs to dashes
    set_na_string(row=everywhere,col=-(1:3),value="--") %>%
    # No decimals on NINTS or HOURS
    set_number_format(row=everywhere,col=ends_with("NINTS"),value=0) %>%
    set_number_format(row=everywhere,col=ends_with("HOURS"),value=0) %>%
    # Extra label at the top
    rbind(c("STATE","DAY TYPE","FISHERY",rep(c("N","Hrs"),length(mos))),.) %>%
    rbind(c("","","",c(rbind(mos,""))),.) %>%
    # Right align values
    set_align(row=-1,col=-(1:3),value="right") %>%
    # Sets table width
    set_width(0.99) %>%
    iFinishTable(labelsRow=2,labelsCol=3)
  
  ## Creates month labels over N and Sum ... generic for different #s of months
  for (i in 1:length(mos)) {
    tmp <- 2*(i+1)
    tmpTbl2 <- merge_cells(tmpTbl2,1,tmp:(tmp+1)) %>%
      set_align(1,tmp,"center")
  }
  tmpTbl2
}

table3 <- function(pressureCount) {
  ## Prepare data.frame for huxtable
  tmp <- pressureCount %>%
    ## Select only variables for the table
    dplyr::select(-YEAR,-DAYS,-VCOUNT) %>%
    droplevels() %>%
    ## Convert to wide format
    tidyr::gather(temp,value,NCOUNT:SDCOUNT) %>%
    tidyr::unite(temp1,DAYTYPE,temp,sep=".") %>%
    tidyr::spread(temp1,value) %>%
    dplyr::select(MONTH,
                  Weekday.NCOUNT,Weekday.COUNT,Weekday.SDCOUNT,
                  Weekend.NCOUNT,Weekend.COUNT,Weekend.SDCOUNT,
                  All.NCOUNT,All.COUNT,All.SDCOUNT) %>%
    dplyr::arrange(MONTH)

  ## Make the huxtable
  tmpTbl2 <- as_hux(tmp) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_top_padding(row=final(1),col=everywhere,value=10) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_left_padding(row=everywhere,col=ends_with(".NCOUNT"),value=15) %>%
    # No decimals on NCOUNT and COUNT
    set_number_format(row=everywhere,col=ends_with(".NCOUNT"),value=0) %>%
    set_number_format(row=everywhere,col=ends_with(".COUNT"),value=0) %>%
    # One decimal on SD
    set_number_format(row=everywhere,col=ends_with(".SDCOUNT"),value=1) %>%
    rbind(c("MONTH",rep(c("Sampled","Total","SD"),3)),.) %>%
    rbind(c("",rep(c("Days","Party Hours",""),3)),.) %>%
    rbind(c("","Weekday","","","Weekend","","","All Days","",""),.) %>%
    merge_cells(row=1,col=2:4) %>%
    merge_cells(row=1,col=5:7) %>%
    merge_cells(row=1,col=8:10) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    merge_cells(row=2,col=3:4) %>%
    merge_cells(row=2,col=6:7) %>%
    merge_cells(row=2,col=9:10) %>%
    set_bottom_border(row=2,col=c(3,6,9),value=1) %>%
    set_align(row=2,col=everywhere,value="center") %>%
    set_align(row=3,col=c(4,5,7,8,9,10),value="center") %>%
    # Right align values for all but first three rows and first column
    set_align(row=-(1:3),col=-1,value="right") %>%
    set_width(0.5) %>%
    iFinishTable(labelsRow=3,labelsCol=1)
  tmpTbl2
}

table4 <- function(fnpre) {
  ## Prepare data.frame for huxtable
  tmp <- read.csv(paste0(fnpre,"ttlEffort.csv")) %>%
    ## Make proper order of MONTHs, WATERS, FISHERYs, and SPECIES
    dplyr::mutate(WATERS=factor(WATERS,levels=c("WI","Non-WI","All")),
                  FISHERY=iMvFishery(FISHERY,addAll=TRUE),
                  MONTH=iOrderMonths(MONTH,addAll=TRUE),
                  DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All"))) %>%
    ## Select only variables for the table
    dplyr::select(WATERS,FISHERY,MONTH,DAYTYPE,PHOURS,SDPHOURS,PARTY,
                  INDHRS,SDINDHRS,MTRIP,TRIPS,SDTRIPS) %>%
    ## Drop unused levels
    droplevels() %>%
    dplyr::arrange(WATERS,FISHERY,MONTH,DAYTYPE) %>%
    ## Remove repeated items in the first three variables
    dplyr::mutate(WATERS=ifelse(!FSA::repeatedRows2Keep(.,cols2use="WATERS"),
                                "",as.character(WATERS)),
                  FISHERY=ifelse(!FSA::repeatedRows2Keep(.,cols2use="FISHERY"),
                                 "",as.character(FISHERY)),
                  MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use="MONTH"),
                                 "",as.character(MONTH))) %>%
    ## Remove rows that are repeats (for the numeric variables) of the row above
    ## it, which happens if the species was captured in only one month
    dplyr::filter(FSA::repeatedRows2Keep(.,cols2ignore=c("WATERS","FISHERY",
                                                         "MONTH","DAYTYPE"))) %>%
  as.data.frame()

  ## Rows with a MONTH (except first) get extra space above
  breakRows1 <- which(tmp$MONTH!="")[-1]
  ## Rows that have a fishery name (except first) get extra space above
  breakRows2 <- which(tmp$FISHERY!="")[-1]
  
  ## Make the huxtable
  tmpTbl2 <- as_hux(tmp) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=breakRows1,col=everywhere,value=10) %>%
    set_top_padding(row=breakRows2,col=everywhere,value=15) %>%
    # Change all NAs to dashes
    set_na_string(row=everywhere,col=-(1:4),value="--") %>%
    # Round these to columns
    set_number_format(row=everywhere,col=c("PHOURS","INDHRS","TRIPS"),value=0) %>%
    set_number_format(row=everywhere,col=c("SDPHOURS","SDINDHRS","SDTRIPS"),
                      value=1) %>%
    set_number_format(row=everywhere,col=c("PARTY","MTRIP"),value=2) %>%
    # Creating column headers
    rbind(c("WATERS","FISHERY","MONTH","DAY TYPE","Total","SD","Party",
            "Total","SD","Length","Total","SD"),.) %>%
    rbind(c("","","","","Party Hours","","per","Ind. Hours","","Trip",
            "Trips",""),.) %>%
    rbind(c("","","","","","","Persons","","","Mean","",""),.) %>%
    merge_cells(row=2,col=5:6) %>%
    set_bottom_border(row=2,col=5,value=1) %>%
    merge_cells(row=2,col=8:9) %>%
    set_bottom_border(row=2,col=8,value=1) %>%
    merge_cells(row=2,col=11:12) %>%
    set_bottom_border(row=2,col=11,value=1) %>%
    set_align(row=2,col=everywhere,value="center") %>%
    set_align(row=2,col=c(7,10),value="right") %>%
    set_align(row=-(1:2),col=-(1:4),value="right") %>%
    set_width(0.8) %>%
    iFinishTable(labelsRow=3,labelsCol=4)
  tmpTbl2
}


table5 <- function(fnpre) {
  ## Prepare data.frame for huxtable
  tmp <- read.csv(paste0(fnpre,"ttlHarvest.csv")) %>%
    ## Make proper order of MONTHs, WATERS, FISHERYs, and SPECIES
    dplyr::mutate(MONTH=iOrderMonths(MONTH,addAll=TRUE),
                  WATERS=factor(WATERS,levels=c("WI","Non-WI","All")),
                  DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All")),
                  FISHERY=iMvFishery(FISHERY,addAll=TRUE),
                  SPECIES=iMvSpecies(SPECIES)) %>%
    ## Select only variables for the table
    dplyr::select(WATERS,FISHERY,SPECIES,MONTH,DAYTYPE,
                  HARVEST,SDHARVEST,HRATE) %>%
    ## Drop unused levels
    droplevels() %>%
    ## Convert to wide format
    tidyr::gather(temp,value,HARVEST:HRATE) %>%
    tidyr::unite(temp1,DAYTYPE,temp,sep=".") %>%
    tidyr::spread(temp1,value) %>%
    dplyr::select(WATERS,FISHERY,SPECIES,MONTH,
                  Weekday.HARVEST,Weekday.SDHARVEST,Weekday.HRATE,
                  Weekend.HARVEST,Weekend.SDHARVEST,Weekend.HRATE,
                  All.HARVEST,All.SDHARVEST,All.HRATE,) %>%
    dplyr::arrange(WATERS,FISHERY,SPECIES,MONTH) %>%
    ## Remove repeated items in the first three variables
    dplyr::mutate(WATERS=ifelse(!FSA::repeatedRows2Keep(.,cols2use="WATERS"),
                                "",as.character(WATERS)),
                  FISHERY=ifelse(!FSA::repeatedRows2Keep(.,cols2use="FISHERY"),
                                 "",as.character(FISHERY)),
                  SPECIES=ifelse(!FSA::repeatedRows2Keep(.,cols2use="SPECIES"),
                                 "",as.character(SPECIES))) %>%
    ## Remove rows that are repeats (for the numeric variables) of the row above
    ## it, which happens if the species was captured in only one month
    dplyr::filter(FSA::repeatedRows2Keep(.,cols2ignore=c("WATERS","FISHERY",
                                                         "SPECIES","MONTH")))
  
  ## Rows with a SPECIES name (except first) get extra space above
  breakRows1 <- which(tmp$SPECIES!="")[-1]
  ## Rows that have a fishery name (except first) get extra space above
  breakRows2 <- which(tmp$FISHERY!="")[-1]
  
  ## Make the huxtable
  tmpTbl2 <- as_hux(tmp) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=breakRows1,col=everywhere,value=10) %>%
    set_top_padding(row=breakRows2,col=everywhere,value=25) %>%
    # Convert NAs to dashes
    set_na_string(row=everywhere,col=-(1:4),value="--") %>%
    # Set decimals
    set_number_format(row=everywhere,col=ends_with(".HARVEST"),value=0) %>%
    set_number_format(row=everywhere,col=ends_with(".SDHARVEST"),value=1) %>%
    set_number_format(row=everywhere,col=ends_with(".HRATE"),value=4) %>%
    # Create nice column headers
    rbind(c("WATERS","FISHERY","SPECIES","MONTH","Number","SD","Angler-Hr",
            "Number","SD","Angler-Hr","Number","SD","Angler-Hr"),.) %>%
    rbind(c("","","","","Harvest","","Harvest/","Harvest","","Harvest/",
            "Harvest","","Harvest/"),.) %>%
    rbind(c("","","","","Weekday","","","Weekend","","","All Days","",""),.) %>%
    merge_cells(row=1,col=5:7) %>%
    merge_cells(row=1,col=8:10) %>%
    merge_cells(row=1,col=11:13) %>%
    merge_cells(row=2,col=5:6) %>%
    merge_cells(row=2,col=8:9) %>%
    merge_cells(row=2,col=11:12) %>%
    set_bottom_border(row=1:2,col=c(5,8,11),value=1) %>%
    set_align(row=1:2,col=everywhere,value="center") %>%
    set_align(row=-(1:2),col=-(1:2),value="right") %>%
    set_width(0.7) %>%
    iFinishTable(labelsRow=3,labelsCol=4)
  tmpTbl2
}


table6 <- function(dlen) {
  ## Prepare data.frame for huxtable
  tmp <- dlen %>%
    ##   Remove fish for which a length was not recorded
    dplyr::filter(!is.na(LEN)) %>%
    ##   Summarize lengths by whether clipped or not
    sumLengths(CLIPPED) %>%
    droplevels() %>%
    ## Remove repeated items in the first three variables
    dplyr::mutate(SPECIES=ifelse(!FSA::repeatedRows2Keep(.,cols2use="SPECIES"),
                                "",as.character(SPECIES)),
                  MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use="MONTH"),
                                 "",as.character(MONTH))) %>%
    ##   Remove the YEAR variable
    dplyr::select(-YEAR)

  ## Add some more space above where SPECIES & MONTY name is (except for first)
  breakRows1 <- which(tmp$MONTH!="")[-1]
  breakRows2 <- which(tmp$SPECIES!="")[-1]
  
  ## Make the huxtable
  tmpTbl2 <- as_hux(tmp) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=breakRows1,col=everywhere,10) %>%
    set_top_padding(row=breakRows2,col=everywhere,25) %>%
    # Change NAs to dashes
    set_na_string(row=everywhere,col=-(1:4),value="--") %>%
    # Round n to 0; mean, SE, and VAR to 2; and Min and Max to 1 decimal
    set_number_format(row=everywhere,col="n",0) %>%
    set_number_format(row=everywhere,col=c("mnLen","sdLen","seLen"),2) %>%
    set_number_format(row=everywhere,col=c("minLen","maxLen"),1) %>%
    # Add column labels
    rbind(c("SPECIES","MONTH","Clipped?","N","Mean","SD","SE","Min","Max"),.) %>%
    rbind(c("","","","Length (in.)","","","","",""),.) %>%
    # Top label should extend across 4-9 columns with line underneath
    merge_cells(row=1,col=4:9) %>%
    set_bottom_border(row=1,col=4,1) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    set_width(0.5) %>%
    iFinishTable(labelsRow=2,labelsCol=3)
  tmpTbl2
}


table7 <- function(dlen) {
  ## Prepare data.frame for huxtable
  tmp <- dlen %>%
    ##   Remove fish for which a length was not recorded
    dplyr::filter(!is.na(LEN))
  ## Find only those species for which a fin-slip was recorded
  specClipped <- as.character(unique(dplyr::filter(lengths,CLIPPED=="Clip")$SPECIES))
  ##   Summarize lengths by whether clipped or not
  tmp <- sumLengths(filter(tmp,SPECIES %in% specClipped),CLIP) %>%
    droplevels() %>%
    ## Remove repeated items in the first three variables
    dplyr::mutate(SPECIES=ifelse(!FSA::repeatedRows2Keep(.,cols2use="SPECIES"),
                                 "",as.character(SPECIES)),
                  MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use="MONTH"),
                               "",as.character(MONTH))) %>%
    ##   Remove the YEAR variable
    dplyr::select(-YEAR)

  ## Which rows to add some more space above
  breakRows1 <- which(tmp$MONTH!="")[-1]
  breakRows2 <- which(tmp$SPECIES!="")[-1]

  ## Make the huxtable
  tmpTbl2 <- as_hux(tmp) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=breakRows1,col=everywhere,value=10) %>%
    set_top_padding(row=breakRows2,col=everywhere,value=25) %>%
    # Change NAs to dashes
    set_na_string(row=everywhere,col=-(1:4),value="--") %>%
    # Round n to 0; mean, SE, and VAR to 2; and Min and Max to 1 decimal
    set_number_format(row=everywhere,col="n",value=0) %>%
    set_number_format(row=everywhere,col=c("mnLen","sdLen","seLen"),value=2) %>%
    set_number_format(row=everywhere,col=c("minLen","maxLen"),1) %>%
    # Add column labels
    rbind(c("SPECIES","MONTH","Clip","N","Mean","SD","SE","Min","Max"),.) %>%
    rbind(c("","","","Length (in.)","","","","",""),.) %>%
    # Top label should extend across 4-9 columns with line underneath
    merge_cells(row=1,col=4:9) %>%
    set_bottom_border(row=1,col=4,value=1) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    set_width(0.4) %>%
    iFinishTable(labelsRow=2,labelsCol=3)
  tmpTbl2
}

table8 <- function(dlen) {
  ## Prepare data.frame for huxtable
  tmp <- dlen %>%
    ## Remove fish that were not checked for clips
    dplyr::filter(!is.na(CLIP)) %>%
    ## Reduce to only variables needed for the table
    dplyr::select(MONTH,SPECIES,CLIP) %>%
    ##   drop unused levels
    droplevels() %>%
    group_by(SPECIES,CLIP,MONTH) %>%
    summarize(n=n()) %>%
    as.data.frame()
  
  ## Summarize across months
  tmp1 <- group_by(tmp,SPECIES,CLIP) %>%
    summarize(n=sum(n)) %>%
    mutate(MONTH="All") %>%
    select(names(tmp)) %>%
    as.data.frame()
  tmp <- rbind(tmp,tmp1)
  ## Summarize across clips
  tmp1 <- group_by(tmp,SPECIES,MONTH) %>%
    summarize(n=sum(n)) %>%
    mutate(CLIP="All") %>%
    select(names(tmp)) %>%
    as.data.frame()
  tmp <- rbind(tmp,tmp1)

  ## Find number of months (for in huxtable)
  mos <- length(levels(tmp$MONTH))

  ## Convert to wide format
  tmpTbl1 <- tmp %>%
    tidyr::spread(MONTH,n) %>%
    arrange(SPECIES,CLIP) %>%
    ## Remove repeated items in the species variables
    dplyr::mutate(SPECIES=ifelse(!FSA::repeatedRows2Keep(.,cols2use="SPECIES"),
                               "",as.character(SPECIES))) %>%
    ## Remove repeated rows (for numeric variables)
    dplyr::filter(FSA::repeatedRows2Keep(.,cols2ignore=c("SPECIES","CLIP")))
  
  ## Find rows that need more space
  breakRows <- which(tmpTbl1$SPECIES!="")[-1]

  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=breakRows,col=everywhere,value=15) %>%
    # Add column labels
    rbind(names(tmpTbl1),.) %>%
    rbind(c("","","MONTH",rep("",mos-1)),.) %>%
    # Top label should extend across MONTHs columns with line underneath
    merge_cells(row=1,col=3:(mos+2)) %>%
    set_bottom_border(row=1,col=3,value=1) %>%
    set_align(row=everywhere,col=3:(mos+2),value="right") %>%
    set_align(row=1,col=everywhere,value="center") %>%
    set_width(0.35) %>%
    iFinishTable(labelsRow=2,labelsCol=2)
  tmpTbl2
}


table9 <- function(dlen) {
  ## Prepare data.frame for huxtable
  tmp <- dlen %>%
    dplyr::select(SPECIES,STATE,SITE,FISHERY,DATE,LEN,CLIP) %>%
    dplyr::arrange(SPECIES,DATE,LEN) %>%
    dplyr::rename(`TL (in)`=LEN) %>%
    droplevels() %>%
    dplyr::mutate(SPECIES=ifelse(duplicated(SPECIES),"",levels(SPECIES)[SPECIES]))
  ## Find rows for extra space above
  breakRows <- which(tmp$SPECIES!="")[-1]
  ## Make huxtable
  tmpTbl2 <- as_hux(tmp,add_colnames=TRUE) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_top_padding(row=breakRows,col=everywhere,15) %>%
    # Put extra spacing between columns
    set_left_padding(row=everywhere,col=everywhere,15) %>%
    # Round lengths to one decimal (what they were recorded in)
    set_number_format(row=everywhere,col="TL (in)",1) %>%
    set_width(0.5) %>%
    iFinishTable(labelsRow=1,labelsCol=1)
  tmpTbl2
}


## Figures ---------------------------------------------------------------------
figureCaptions <- function() {
  figures <- captioner::captioner(prefix="Figure")
  figures(name="Figure1",
          caption=paste("Total effort (angler-hours) for WISCONSIN waters only",
                        "by month and day type. Errors bars are +/-1SD on total",
                        "effort across both day types. Also see Table 4."))
  figures(name="Figure2",
          caption=paste("Total effort (angler-hours) for WISCONSIN waters only",
                        "by month, fishery type, and day type. Errors bars are",
                        "+/-1SD on total effort across both day types. Also",
                        "see Table 4."))
  figures(name="Figure3",
          caption=paste("Total harvest of all species observed for WISCONSIN",
                        "waters only. Errors bars are +/-1SD on total harvest.",
                        "Also see Table 5."))
  figures(name="Figure4",
          caption=paste("Total harvest of the most commonly harvested",
                        "fish separated by fishery type, day type, and month",
                        "for WISCONSIN waters only. Errors bars are +/-1SD on",
                        "the total harvest across both day types. Also see",
                        "Table 5."))
  figures(name="Figure5",
          caption=paste("Harvest rate (total harvest per angler-hour) by month",
                        "of the most commonly harvested fish for",
                        "WISCONSIN waters only. Also see Table 5."))
  figures(name="Figure6",
          caption=paste("Density plot of total length by month for the",
                        "most commonly measured fish. Sample size is for",
                        "combined clipped and not clipped fish. Also see",
                        "Tables 6 and 9."))
  figures
}

theme_creel <- function() {
  thm <- theme_bw(base_size=14) +
    theme(
      legend.position="bottom",
      legend.title=element_blank(),
      legend.text=element_text(size=rel(0.75)),
      legend.spacing.x=unit(3,"mm")
    )
  thm <- list(thm,
              scale_color_brewer(palette="Dark2",direction=-1),
              scale_fill_brewer(palette="Dark2",direction=-1))
  thm
}

figure1 <- function(d) {
  tmp <- d %>%
    ## Select only variables for the figure
    dplyr::select(WATERS,FISHERY,MONTH,DAYTYPE,INDHRS,SDINDHRS) %>%
    ## Select only WI waters and not All months or fisheries
    dplyr::filter(WATERS=="WI",MONTH!="All") %>%
    ## Drop unused levels
    droplevels()
  ## Remove DAYTYPE and FISHERY total rows (i.e., == "All)
  tmp1 <- dplyr::filter(tmp,DAYTYPE!="All") %>%
    dplyr::filter(FISHERY!="All")
  ## Get the DAYTYPE and FISHERYtotal rows (for error bars in ggplot)
  tmp2 <- dplyr::filter(tmp,DAYTYPE=="All",FISHERY=="All") %>%
    dplyr::mutate(DAYTYPE=NA,FISHERY=NA)
  
  ## Make the plot
  p <- ggplot(data=tmp1,aes(x=MONTH,y=INDHRS,fill=DAYTYPE)) +
    geom_bar(stat="identity") +
    geom_errorbar(data=tmp2,aes(ymin=INDHRS-SDINDHRS,ymax=INDHRS+SDINDHRS),
                  width=0.2) +
    xlab("Month") +
    ylab("Total Effort (Angler-Hrs)") +
    scale_y_continuous(expand=expand_scale(mult=c(0,0.1))) +
    theme_creel()
  p
}
  

figure2 <- function(d) {
  tmp <- d %>%
    ## Select only variables for the figuree
    dplyr::select(WATERS,FISHERY,MONTH,DAYTYPE,INDHRS,SDINDHRS) %>%
    ## Select only WI waters and not All months or Fisheries
    dplyr::filter(WATERS=="WI",MONTH!="All") %>%
    dplyr::filter(FISHERY!="All") %>%
    ## Drop unused levels
    droplevels()
  ## Remove DAYTYPE total rows (i.e., == "All)
  tmp1 <- dplyr::filter(tmp,DAYTYPE!="All")
  ## Get the DAYTYPE total rows (for error bars in ggplot)
  tmp2 <- dplyr::filter(tmp,DAYTYPE=="All") %>%
    dplyr::mutate(DAYTYPE=NA)
  
  ## Make the plot
  p <- ggplot(data=tmp1,aes(x=MONTH,y=INDHRS,fill=DAYTYPE)) +
    geom_bar(stat="identity") +
    geom_errorbar(data=tmp2,aes(ymin=INDHRS-SDINDHRS,ymax=INDHRS+SDINDHRS),
                  width=0.2) +
    facet_grid(FISHERY~.) +
    xlab("Month") +
    ylab("Total Effort (Angler-Hrs)") +
    scale_y_continuous(expand=expand_scale(mult=c(0,0.1))) +
#    scale_color_discrete(breaks=c("Weekday","Weekend")) +
    theme_creel()
  p
}

figure3 <- function(d) {
  tmp <- d %>%
    ## Just WI waters
    dplyr::filter(WATERS=="WI") %>%
    ## Get the total total total rows (to find topN)
    dplyr::filter(MONTH=="All",DAYTYPE=="All",FISHERY=="All")
  
  ## Make the plot
  p <- ggplot(data=tmp,aes(x=SPECIES,y=HARVEST)) +
    geom_bar(stat="identity",fill=brewer.pal(3,"Dark2")[1]) +
    geom_errorbar(data=tmp,aes(ymin=HARVEST-SDHARVEST,ymax=HARVEST+SDHARVEST),
                  width=0.2) +
    ylab("Total Harvest") +
    scale_y_continuous(expand=expand_scale(mult=c(0,0.1))) +
    theme_creel() +
    theme(
      axis.title.x.bottom=element_blank(),
      axis.text.x=element_text(angle=90,vjust=0.25,hjust=1)
    )
  p
}

figure4 <- function(d,topN=3) {
  tmp <- d %>%
    ## Just WI waters
    dplyr::filter(WATERS=="WI")
  ## Remove MONTH, DAYTYPE, and FISHERY total rows (i.e., == "All)
  tmp1 <- dplyr::filter(tmp,MONTH!="All",DAYTYPE!="All",FISHERY!="All")
  ## Get the DAYTYPE total rows (for error bars in ggplot)
  tmp2 <- dplyr::filter(tmp,MONTH!="All",DAYTYPE=="All",FISHERY!="All") %>%
    dplyr::mutate(DAYTYPE=NA)
  ## Get the total total total rows (to find topN)
  tmp3 <- dplyr::filter(tmp,MONTH=="All",DAYTYPE=="All",FISHERY=="All")
  
  ## Find topN species by total harvest
  TOPN <- dplyr::top_n(tmp3,topN,HARVEST)$SPECIES
  
  ## Reduce to only the topN species (by HARVEST)
  tmp1 %<>% dplyr::filter(SPECIES %in% TOPN)
  tmp2 %<>% dplyr::filter(SPECIES %in% TOPN)
  
  ## Make the plot
  p <- ggplot(data=tmp1,aes(x=MONTH,y=HARVEST,fill=DAYTYPE)) +
    geom_bar(stat="identity") +
    geom_errorbar(data=tmp2,aes(ymin=HARVEST-SDHARVEST,ymax=HARVEST+SDHARVEST),
                  width=0.2) +
    facet_grid(SPECIES~FISHERY,scales="free") +
    xlab("Month") +
    ylab("Total Harvest") +
    scale_y_continuous(expand=expand_scale(mult=c(0,0.1))) +
#    scale_color_discrete(breaks=c("Weekday","Weekend")) +
    theme_creel()
  p
}

figure5 <- function(d,topN=3) {
  tmp <- d %>%
    ## Just WI waters
    dplyr::filter(WATERS=="WI")
  ## Get the DAYTYPE nd FISHERY total rows (for error bars in ggplot)
  tmp1 <- dplyr::filter(tmp,MONTH!="All",DAYTYPE=="All",FISHERY=="All") %>%
    dplyr::mutate(DAYTYPE=NA)
  ## Get the total total total rows (to find topN)
  tmp2 <- dplyr::filter(tmp,MONTH=="All",DAYTYPE=="All",FISHERY=="All")
  
  ## Find topN species by total harvest
  TOPN <- dplyr::top_n(tmp2,topN,HARVEST)$SPECIES
  
  ## Reduce to only the topN species (by HARVEST)
  tmp1 %<>% dplyr::filter(SPECIES %in% TOPN)

  ## Make the plot
  p <- ggplot(data=tmp1,aes(x=MONTH,y=HRATE,group=SPECIES,color=SPECIES)) +
    geom_line(size=1) +
    geom_point(size=2,shape=21,fill="white") +
    xlab("Month") +
    ylab("Harvest per Angler-Hr") +
    scale_y_continuous(limits=c(0,NA),expand=expand_scale(mult=c(0,0.1))) +
    theme_creel()
  p
}

figure6 <- function(dlen,topN=3) {
  # Sample size by species and month
  SUM <- dlen %>%
    dplyr::group_by(SPECIES,MONTH) %>%
    dplyr::summarize(n=dplyr::n())
  # Find topN species by sample size
  TOPN <- dplyr::summarize(SUM,n=sum(n)) %>%
    dplyr::top_n(topN,n)
  # Reduce summaries to the topN species
  #   add dummy CLIPPED variable for plotting
  SUM %<>% filter(SPECIES %in% TOPN$SPECIES) %>%
    mutate(CLIPPED=NA)
  # Reduce original data.frame to the topN species
  dlen %<>% filter(SPECIES %in% TOPN$SPECIES)
  # Make the plot
  p <- ggplot(data=dlen,aes(x=LEN,fill=CLIPPED)) +
    geom_histogram(binwidth=1,na.rm=TRUE) +
    xlab("Length (Inches)") +
    ylab("Relative Frequency") +
    scale_y_continuous(expand=expand_scale(mult=c(0,0.1))) +
    facet_grid(MONTH~SPECIES,scales='free') +
    geom_text(data=SUM,aes(x=Inf,y=Inf,label=paste0("n=",n)),
              size=5,vjust=1.1,hjust=1.1) +
    theme_creel() +
    theme(
      axis.text.y.left = element_blank()
    )
  p
}


## Internals for Mains ----
## Make filename prefix
fnPrefix <- function(RDIR,LOC,SDATE) {
  paste0(RDIR,"/",iMvLoc(LOC),"_",lubridate::year(SDATE),"_")
}

## Convenience function for making a file of the data.frame in x
writeDF <- function(x,fnpre) {
  x1 <- deparse(substitute(x))
  write.csv(x,file=paste0(fnpre,x1,".csv"),
            row.names=FALSE,quote=FALSE,na="")
}


## Creates day lengths from month names
iMvDaylen <- function(x,DAY_LENGTH) {
  daylens <- DAY_LENGTH[as.character(x)]
  names(daylens) <- NULL
  daylens
}

## Convert DOW to weekend or weekdays ... with holidays as weekends
iMvDaytype <- function(wd,mon,md) {
  wd2 <- FSA::mapvalues(wd,from=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                         to=c("Weekday","Weekday","Weekday","Weekday","Weekday",
                              "Weekend","Weekend"),warn=FALSE)
  wd2 <- iHndlHolidays(mon,md,wd,wd2)
  wd2 <- factor(wd2,levels=c("Weekday","Weekend"))
  wd2  
}

## Make New Years, Memorial Day, July 4th, and Labor Day as WEEKENDS
##   Note that only official holidays are New Years, Memorial Day, July Fourth,
##     and Labor Day (Thanksgiving and Christmas are not included).
iHndlHolidays <- function(mon,md,wd,dt) {
  dplyr::case_when(
    mon=="Jan" & md==1 ~ "Weekend",              # New Years Day
    mon=="May" & md>=25 & wd=="Mon" ~ "Weekend", # Memorial Day
    mon=="Jul" & md==4 ~ "Weekend",              # 4th of July
    mon=="Sep" & md<=7 & wd=="Mon" ~ "Weekend",  # Labor Day
    TRUE ~ as.character(dt)
  )
}


## Convert state codes to words
iMvStates <- function(x) {
  tmp <- c("WI","MN","MI","WI/MN","WI/MI")
  if (is.numeric(x)) x <- FSA::mapvalues(x,from=1:5,to=tmp,warn=FALSE)
  factor(x,levels=tmp)
}

## Create "waters" variable to identify if the fished area was in WI or not
iMvWaters <- function(x) {
  x <- dplyr::case_when(
    x=="MN" ~ "Non-WI",
    x=="MI" ~ "Non-WI",
    TRUE ~ "WI"
  )
  factor(x,levels=c("WI","Non-WI"))
}

## Convert "fishery" codes to words
##   left "old" in just in case return to legacy names
iMvFishery <- function(x,addAll=FALSE) {
  old <- c("COLDWATER-OPEN","WARMWATER-OPEN","ICE-STREAM MOUTH","ICE-WARMWATER",
           "ICE-BOBBING","BAD RIVER","NON-FISHING","SHORE","TRIBAL","COMBINED")
  tmp <- c("Cold-Open","Warm-Open","Ice-Mouth","Ice-Warm","Ice-Bob",
           "Bad R.","Non-Fishing","Shore","Tribal","Combined")
  if (is.numeric(x)) x <- FSA::mapvalues(x,from=1:10,to=tmp,warn=FALSE)
  if (addAll) tmp <- c(tmp,"All")
  factor(x,levels=tmp)
}

iMvMgmtUnit <- function(d,LOC,addALL=FALSE) {
  ## If unit variable does not exist, initiate it with NAs
  if (!"MUNIT" %in% names(d)) unit <- as.character(rep(NA,length(d$SITE)))
  ## If unit is cpw then must handle filling blank units differently
  if (LOC=="cpw") {
    ### NEED TO WORK THIS OUT
  } else {
    ### Complete mgmt units based on location of creel
    unit[is.na(unit)] <- dplyr::case_when(
      LOC %in% c("ash","byf","lsb","rdc","sax","wsh") ~ "WI-2",
      LOC=="sup" ~ "WI-1"
    )
  }
  ## However, correct if fishing state was not at least partially WI
  unit[d$STATE %in% c("MI","MN")] <- as.character(d$STATE[d$STATE %in% c("MI","MN")])
  ## Create factor with ordered levels
  tmp <- c("WI-1","WI-2","MI","MN")
  if (addALL) tmp <- c(tmp,"All")
  factor(unit,levels=tmp)
}


## Convert status codes to words
iMvStatus <- function(x) {
  tmp <- c("Complete","Incomplete")
  if (is.numeric(x)) x <- FSA::mapvalues(x,from=1:2,to=tmp,warn=FALSE)
  factor(x,levels=tmp)
}

## Convert residency codes to words
##   left "old" in just in case return to legacy names
iMvResidency <- function(x) {
  old <- c("Resident","Non-Resident","Resident/Non-Resident")
  tmp <- c("Res","Non-Res","Res/Non-Res")
  if (is.numeric(x)) x <- FSA::mapvalues(x,from=1:3,to=tmp,warn=FALSE)
  factor(x,levels=tmp)
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

## Convert fish species codes to words ... note possibly three sets of codes
iMvSpecies <- function(x) {
  if (is.numeric(x)) code <- c(2,3,4,5,6,7,11,13,33,34,35,36,43,45,46,47,48,49,
                               51,52,60,62,66,67,70,71,76,78,98,99)
  else {
    tmp <- substr(x,1,1)
    if (any(tmp %in% c("W","M","I","B","R","X")))
      code <- c('W09','W00','W02','W14','I22','W06','R01','M12','I04','I24',
                'I23','I28','L02','I21','I19','W04','I16','I14','I12','I20',
                'J01','B01','W12','W11','I05','I18','X15','X22','098','099')
    else code <- c('002','003','004','005','006','007','011','013','033','034',
                   '035','036','043','045','046','047','048','049','051','052',
                   '060','062','066','067','070','071','076','078','098','099')
  }
  nms <- c('BLUEGILL','SUNFISH SPP.','CRAPPIE SPP.','BLACK CRAPPIE',
           'BROOK TROUT','PUMPKINSEED','BURBOT','CARP','LAKE HERRING',
           'SISCOWET','LAKE TROUT','SPLAKE','NORTHERN PIKE','BROWN TROUT',
           'RAINBOW TROUT','ROCK BASS','CHINOOK','COHO','PINK SALMON',
           'ATLANTIC SALMON','SMELT','STURGEON','LARGEMOUTH BASS',
           'SMALLMOUTH BASS','LAKE WHITEFISH','ROUND WHITEFISH','YELLOW PERCH',
           'WALLEYE','CATFISH','NA')
  lvls <- c('LAKE TROUT','SISCOWET','ATLANTIC SALMON','BROOK TROUT',
            'BROWN TROUT','COHO','CHINOOK','PINK SALMON','RAINBOW TROUT',
            'SPLAKE','BURBOT','LAKE HERRING','LAKE WHITEFISH','ROUND WHITEFISH',
            'SMELT','STURGEON','WALLEYE','YELLOW PERCH','NORTHERN PIKE',
            'BLUEGILL','PUMPKINSEED','SUNFISH SPP.','BLACK CRAPPIE',
            'CRAPPIE SPP.','LARGEMOUTH BASS','ROCK BASS','SMALLMOUTH BASS',
            'CATFISH','CARP')
  nms <- FSA::capFirst(nms)
  nms[length(nms)] <- NA
  lvls <- FSA::capFirst(lvls)
  if (!any(x %in% nms)) x <- FSA::mapvalues(x,from=code,to=nms,warn=FALSE)
  factor(x,levels=lvls)
}

## Convert fin-clip codes to words
##   99 is mapped to 40 as this happens after 99 is expanded to multiple lengths
iMvFinclips <- function(x,addAll=TRUE) {
  tmp <- c('00: NONE','01: AD','02: AD+LV','03: AD+RV','04: AD+LP','05: AD+RP',
           '06: LV','07: RV','08: LP','09: RP','10: LV+RV','11: RP+LV',
           '12: AD+LV+RV','13: D','14: HATCHERY','15: LP+RV','16: D+RV',
           '17: D+RP','18: AD+RM','19: LP+RM','20: LP+LV','21: D+AD',
           '22: D+LV+RV','23: D+LP','24: D+LV','40: NOT EXAMINED',
           '40: NOT EXAMINED',NA)
  if (is.numeric(x)) x <- FSA::mapvalues(x,from=c(0:24,40,99,NA),
                                          to=tmp,warn=FALSE)
  if (addAll) tmp <- c(tmp,"All")
  factor(x,levels=tmp[!duplicated(tmp)])
}

## Adds whether a fish was clipped or not
iFinclipped <- function(x) {
  tmp <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c('00: NONE','40: NOT EXAMINED') ~ "No Clip",
    TRUE ~ "Clip"
  )
  factor(tmp,levels=c("Clip","No Clip"))
}


## Create long name from abbreviated location name
iMvLoc <- function(x) {
  codes <- c("ash","byf","cpw","lsb","rdc","sax","sup","wsh")
  names <- c("Ashland","Bayfield","Corny-Port Wing","Little Sand Bay",
             "Red Cliff","Saxon","Superior","Washburn")
  FSA::mapvalues(x,from=codes,to=names,warn=FALSE)
}

## Converts months to an ordered factor
iOrderMonths <- function(x,addAll=FALSE) {
  if (addAll) ordered(x,levels=c(format(ISOdate(2004,1:12,1),"%b"),"All"))
  else ordered(x,levels=format(ISOdate(2004,1:12,1),"%b"))
}


iSumLen <- function(dgb) {
  tmp <- dplyr::summarize(dgb,n=dplyr::n(),mnLen=mean(LEN,na.rm=TRUE),
                          sdLen=sd(LEN,na.rm=TRUE),seLen=FSA::se(LEN,na.rm=TRUE),
                          minLen=min(LEN,na.rm=TRUE),maxLen=max(LEN,na.rm=TRUE)) %>%
    as.data.frame()
  tmp$sdLen[is.nan(tmp$sdLen)] <- NA
  tmp
}


## Converts missing values to zeroes
iConvNA20 <- function(x) {
  dplyr::case_when(
    is.na(x) ~ 0,
    TRUE ~ x
  )
}


## Add final characteristics to all tables
iFinishTable <- function(h,labelsRow,labelsCol) {
  h <- h %>%
    # Line above top and below bottom of table
    set_top_border(row=1,col=everywhere,2) %>%
    set_bottom_border(row=final(),col=everywhere,2) %>%
    # Line below variable names
    set_bottom_border(row=labelsRow,col=everywhere,1) %>%
    # Bold rows and columns of labels
    set_bold(row=1:labelsRow,col=everywhere,TRUE) %>%
    set_bold(row=everywhere,col=1:labelsCol,TRUE)
  h
}
