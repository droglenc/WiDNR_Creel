#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE (unless you know what you are doing)!!!
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Load Packages ---------------------------------------------------------------
rqrd <- c("FSA","lubridate","tidyr","dplyr","magrittr","huxtable",
          "kableExtra","captioner","ggplot2","patchwork","sugrrants","grid",
          "haven","knitr","here","readxl","tibble","RColorBrewer")
for (i in seq_along(rqrd)) suppressPackageStartupMessages(library(rqrd[i],
                                  character.only=TRUE))



## Constants -------------------------------------------------------------------
lvlsFISHERY <- c("Open-Water Cold","Open-Water Cool","Stream","Pleasure",
                 "Shore","Tribal","Open-Water Whitefish","SMB Only")
lvlsSPECIES <- c('lake trout','siscowet lake trout','splake','brook trout',
                 'brown trout','rainbow trout','Chinook salmon','coho salmon',
                 'atlantic salmon','lake whitefish','round whitefish',
                 'lake herring','bloater','kiyi','shortjaw cisco',
                 'cisco crosses','chubs','mule whitefish','walleye',
                 'yellow perch','ruffe','muskellunge','northern pike',
                 'bluegill','pumpkinseed sunfish','largemouth bass',
                 'smallmouth bass','rock bass','crappie','rainbow smelt',
                 'burbot','lake sturgeon','common carp','Channel Catfish',
                 'black bullhead','brown bullhead','yellow bullhead',
                 'round goby','tube nose goby','sea lamprey','alewife',
                 'white perch','All Fish')
lvlsSPECIES <- FSA::capFirst(lvlsSPECIES)
lvlsCLIP <- c("Native","Adipose","LRAd","RRAd","LFAd","RFAd","LR","RR","LF",
              "RF","BR","RFLR","BRAd","D","LFRR","RRD","RFD","RFLF","LFLR",
              "DAd","BRD","LFD","LRD","RFM","LMAd","LFM","Hatchery/Curled Fin",
              "RRRFAD","LFRRAd","RFBR","LRRR","BF","RFRR","Not checked")



## Main Helpers ----------------------------------------------------------------
## Computes hours of effort from the start and stop times recorded by the clerk.
## Will NA if recorded data is before start or after end of survey period or if
## stop time is before start time.
hndlHours <- function(STARTHH,STARTMM,STOPHH,STOPMM,DATE,SDATE,FDATE) {
  START <- STARTHH*60+STARTMM
  STOP <- STOPHH*60+STOPMM
  dplyr::case_when(
    DATE < SDATE ~ NA_real_,   # Date before start date
    DATE > FDATE ~ NA_real_,   # Date after end date
    STOP < START ~ NA_real_,   # Stopped before started
    TRUE ~ (STOP-START)/60     # OK ... calc hours of effort
  )
}

## Expands observed pressure counts from the sampled days/times (in opc) to all
## days and times according to day lengths and number of days per month month
## (in cal)
expandPressureCounts <- function(opc,cal) {
  ## Isolate daylengths for each month from the calendar
  tmp <- cal[,c("MONTH","DAYLEN")]
  tmp <- tmp[!duplicated(tmp),]
  opc %<>%
    ### Adds a temporary day length variable (from cal) to each count
    dplyr::right_join(tmp,by="MONTH") %>%
    ### Expand observed pressure counts to represent the entire day at each site 
    dplyr::mutate(COUNT=COUNT*DAYLEN/WAIT) %>%
    ### Compute daily pressure counts (across sites within days)
    dplyr::group_by(YEAR,MONTH,DAY,DAYTYPE) %>%
    dplyr::summarize(WAIT=sum(WAIT),COUNT=sum(COUNT)) %>%
    dplyr::ungroup() %>%
    ### Computed n, variance, and mean daily pressure counts by daytype and month
    dplyr::group_by(YEAR,DAYTYPE,MONTH) %>%
    dplyr::summarize(NCOUNT=dplyr::n(),
                     VCOUNT=var(COUNT,na.rm=TRUE),
                     MCOUNT=mean(COUNT,na.rm=TRUE)) %>%
    ### Converts variance of individuals to a variance of means (i.e., SE^s)
    dplyr::mutate(VCOUNT=VCOUNT/NCOUNT) %>%
    dplyr::ungroup() %>%
    ### Expand by number of days in the month (in cal)
    merge(cal[,c("MONTH","DAYTYPE","DAYS")],by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(TCOUNT=MCOUNT*DAYS,VCOUNT=VCOUNT*(DAYS^2)) %>%
    dplyr::select(YEAR,DAYTYPE,MONTH,NCOUNT,DAYS,TCOUNT,VCOUNT)
  ## Find totals across DAYTYPEs (within each month)
  opc1 <- dplyr::group_by(opc,YEAR,MONTH) %>%
    dplyr::summarize(NCOUNT=sum(NCOUNT,na.rm=TRUE),
                     DAYS=sum(DAYS,na.rm=TRUE),
                     TCOUNT=sum(TCOUNT,na.rm=TRUE),
                     VCOUNT=sum(VCOUNT,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(opc)) %>%
    as.data.frame()
  ## Combine DAYTYPE (within MONTHs) summaries with original values
  opc <- rbind(opc,opc1)
  ## Find totals across all MONTHs (within each DAYTYPE)
  opc2 <- dplyr::group_by(opc,YEAR,DAYTYPE) %>%
    dplyr::summarize(NCOUNT=sum(NCOUNT,na.rm=TRUE),
                     DAYS=sum(DAYS,na.rm=TRUE),
                     TCOUNT=sum(TCOUNT,na.rm=TRUE),
                     VCOUNT=sum(VCOUNT,na.rm=TRUE)) %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(opc)) %>%
    as.data.frame()
  ## Combine MONTH (within DAYTYPE) summaries with original values
  opc <- rbind(opc,opc2)
  ## Convert to SEs and rearrange variables
  opc %<>% dplyr::mutate(SDCOUNT=sqrt(VCOUNT)) %>%
    dplyr::select(YEAR,DAYTYPE,MONTH,NCOUNT,DAYS,TCOUNT,SDCOUNT,VCOUNT) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH,addAll=TRUE),
                  DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All"))) %>%
    dplyr::arrange(YEAR,MONTH,DAYTYPE) %>%
    as.data.frame()
  ## Return data.frame
  opc
}


##
sumInterviewedEffort <- function(dints) {
  ## Correct for effort in "two" states
  ### All records where fishing was in one state
  f1 <- dplyr::filter(dints,!STATE %in% c("Wisconsin/Minnesota","Wisconsin/Michigan"))
  ### All records where fishing was in two states
  f2 <- dplyr::filter(dints,STATE %in% c("Wisconsin/Minnesota","Wisconsin/Michigan")) %>%
    ### Cut the fishing effort (HOURS) in half (apportioned to each state)
    dplyr::mutate(HOURS=0.5*HOURS)
  ### Duplicate f2 to get other half of HOURS (& label MUNIT as other state)
  f3 <- f2 %>%
    dplyr::mutate(MUNIT=ifelse(STATE=="Wisconsin/Minnesota","MN","MI"))
  ### Combine to get all interviews corrected for location
  f <- rbind(f1,f2,f3)
  
  fsum <- f  %>%
    ### Compute people-hours of fishing effort (in INDHRS) ... only used to find
    ###   mean party size (in PARTY) below
    ### Compute hours for only completed trips (in CHOURS) ... only used to find
    ###   mean length of a COMPLETED trip (in MTRIP) below.
    dplyr::mutate(INDHRS=PERSONS*HOURS,
                  CHOURS=ifelse(STATUS=="Complete",HOURS,NA)) %>%
    ## Summarize interviewed effort data by MUNIT, DAYTPE, FISHERY, MONTH
    ##   across all interviews, sites, and days
    dplyr::group_by(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH) %>%
    dplyr::summarize(NINTS=dplyr::n(),
                     MTRIP=mean(CHOURS,na.rm=TRUE),
                     USSHOURS=sum(HOURS^2,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    ## !! This was how PARTY was calculated in the SAS code ... it is NOT the same
    ## !! as calculating the mean of PERSONS ... not clear why computed this way.
    ## !! Note that USSHours is the uncorrected SS of HOURS ... this is used later
    ## !! to find the variance of the HOURS
    dplyr::mutate(PARTY=INDHRS/HOURS) %>%
    dplyr::select(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,
                  NINTS,HOURS,MTRIP,USSHOURS,PARTY) %>%
    as.data.frame()
  
  ## Find total interviewed hours by MONTH and DAYTYPE (nints in SAS code)
  fsum2 <- dints %>%
    dplyr::group_by(MONTH,DAYTYPE) %>%
    dplyr::summarize(THOURS_MD=sum(HOURS,na.rm=TRUE)) %>%
    as.data.frame()
  
  ## Combine the summarized effort with fsum2 to compute PROP
  f <- merge(fsum,fsum2,by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(PROP=HOURS/THOURS_MD) %>%
    dplyr::select(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,
                  NINTS,HOURS,USSHOURS,MTRIP,PROP,PARTY) %>%
    dplyr::arrange(MUNIT,FISHERY,DAYTYPE,MONTH)
  ## Return data.frame (not a tibble)
  as.data.frame(f)
}



## Summarized total fishing effort by strata
sumEffort <- function(ieff,pct) {
  ## Remove 'All' DAYTYPE and MONTH rows from pressureCount results
  pct %<>% dplyr::filter(DAYTYPE!="All") %>%
    dplyr::filter(MONTH!="All")
  # Combine interview effort and pressure counts with new calculations ...
  eff <- merge(ieff,pct,by=c("YEAR","MONTH","DAYTYPE")) %>%
    dplyr::mutate(PHOURS=TCOUNT*PROP,
                  VPHOURS=VCOUNT*(PROP^2),
                  TRIPS=PHOURS/MTRIP,
                  VTRIPS=VPHOURS/(MTRIP^2),
                  INDHRS=PHOURS*PARTY,
                  VINDHRS=VPHOURS*(PARTY^2)) %>%
    dplyr::select(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,
                  NINTS,HOURS,USSHOURS,PHOURS,VPHOURS,
                  INDHRS,VINDHRS,TRIPS,VTRIPS) %>%
    ## Convert NAs to zeroes in the variance variables
    dplyr::mutate(VPHOURS=ifelse(is.na(VPHOURS),0,VPHOURS),
                  VINDHRS=ifelse(is.na(VINDHRS),0,VINDHRS),
                  VTRIPS=ifelse(is.na(VTRIPS),0,VTRIPS)) %>%
    as.data.frame()
  
  ## Summarize across Daytypes
  eff1 <- dplyr::group_by(eff,YEAR,MUNIT,FISHERY,MONTH) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     USSHOURS=sum(USSHOURS,na.rm=TRUE),
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
  eff2 <- dplyr::group_by(eff,YEAR,MUNIT,FISHERY,DAYTYPE) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     USSHOURS=sum(USSHOURS,na.rm=TRUE),
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
  eff3 <- dplyr::group_by(eff,YEAR,MUNIT,MONTH,DAYTYPE) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     USSHOURS=sum(USSHOURS,na.rm=TRUE),
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
    ## Add on mean persons per party and mean trip length
    dplyr::mutate(PARTY=INDHRS/PHOURS,MTRIP=PHOURS/TRIPS) %>%
    ## Convert variances to standard deviations
    dplyr::mutate(SDPHOURS=sqrt(VPHOURS),
                  SDINDHRS=sqrt(VINDHRS),
                  SDTRIPS=sqrt(VTRIPS)) %>%
    ## Put variables in specific order
    dplyr::select(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,
                  PHOURS,SDPHOURS,PARTY,INDHRS,SDINDHRS,MTRIP,TRIPS,SDTRIPS,
                  NINTS,HOURS,USSHOURS,VPHOURS,VINDHRS,VTRIPS) %>%
    ## Arrange
    dplyr::arrange(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH)
  # return final data.frame
  eff
}


## Summarize observed harvest by strata and species
sumObsHarvest <- function(d) {
  ## Remove records of interviews not related to fishing
  harv <- d %>%
    dplyr::filter(FISHERY!="non-fishing")

  ## Correct for interviews in "two" states
  ### Harvest for when fishing only in one state
  h1 <- dplyr::filter(harv,!STATE %in% c("Wisconsin/Minnesota","Wisconsin/Michigan"))
  ### Harvest for when fishing in more than one state ... harvest cut in half
  h2 <- dplyr::filter(harv,STATE %in% c("Wisconsin/Minnesota","Wisconsin/Michigan")) %>%
    dplyr::mutate(HOURS=0.5*HOURS,HARVEST=0.5*HARVEST)
  ### Duplicated h2 to get other half of HOURS/HARVEST, label MUNITS
  h3 <- h2 %>%
    dplyr::mutate(MUNIT=ifelse(STATE=="Wisconsin/Minnesota","MN","MI"))
  ### Combine to get all interviews corrected for location
  harv <- rbind(h1,h2,h3)
  
  ## Summarize harvest by strata and species
  ###   USSHARVEST is uncorrected SS for computer variance of HARVEST later
  ###   UCOVAR is intermediate value to compute HARVEST HOURS covariance later
  harv %<>% dplyr::group_by(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,SPECIES) %>%
    dplyr::summarize(USSHARVEST=sum(HARVEST^2,na.rm=TRUE),
                     UCOVAR=sum(HARVEST*HOURS,na.rm=TRUE),
                     HARVEST=sum(HARVEST,na.rm=TRUE)) %>%
    ### Note that HOURS is not returned but it is merged from the summarized
    ### effort data (by waters, fishery, daytype, and month) in the next function
    dplyr::select(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,SPECIES,
                  HARVEST,USSHARVEST,UCOVAR)
  ## Return data.frame (not a tibble)
  as.data.frame(harv)
}

## Summarize Harvest and Effort
sumHarvestEffort <- function(h,f) {
  ##   Merge harvest and effort data.frames
  hf <- merge(h,f,by=c("YEAR","MUNIT","FISHERY","DAYTYPE","MONTH"),
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
                  USSHOURS=ifelse(is.na(NINTS) | NINTS==0,NA,USSHOURS),
                  VHOURS=(USSHOURS-(HOURS^2)/NINTS)/(NINTS-1),
                  HARVEST=ifelse(is.na(NINTS) | NINTS==0,NA,HARVEST),
                  USSHARVEST=ifelse(is.na(NINTS) | NINTS==0,NA,USSHARVEST),
                  VHARVEST=(USSHARVEST-(HARVEST^2)/NINTS)/(NINTS-1),
                  ## Mean harvest rate by a party
                  PHRATE=HARVEST/HOURS,
                  UCOVAR=ifelse(is.na(NINTS) | NINTS==0,NA,UCOVAR),
                  COVAR=(UCOVAR-HARVEST*HOURS/NINTS)/(NINTS-1),
                  MHOURS=HOURS/NINTS,
                  MHARV=HARVEST/NINTS,
                  VPHRATE=ifelse(MHARV==0 & NINTS>1,0,
                                 (VHARVEST/(MHARV^2))+(VHOURS/(MHOURS^2))-
                                  2*COVAR/(MHARV*MHOURS)),
                  VPHRATE=(PHRATE^2)*VPHRATE/NINTS,
                  ## Total harvest
                  HARVEST=PHOURS*PHRATE,
                  VHARVEST=(PHOURS^2)*VPHRATE+(PHRATE^2)*VPHOURS+VPHRATE*VPHOURS) %>%
    ## Note HRATE, VHRATE, MHOURS, MHARV are intermediates & are not returned
    dplyr::select(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,SPECIES,
                  NINTS,HARVEST,VHARVEST,INDHRS) %>%
    as.data.frame() %>%
    droplevels()

  ## Summarizes across day-types
  hf1 <- dplyr::group_by(hf,YEAR,MUNIT,FISHERY,SPECIES,MONTH) %>%
    dplyr::summarize(NINTS=sum(NINTS),
                     HARVEST=sum(HARVEST,na.rm=TRUE),
                     VHARVEST=ifelse(NINTS==1,NA,sum(VHARVEST,na.rm=TRUE)),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(hf)) %>%
    as.data.frame()
  ## Combine those two summaries to original data.frame
  hf <- rbind(hf,hf1)

  ## Summarizes across months
  hf2 <- dplyr::group_by(hf,YEAR,MUNIT,FISHERY,SPECIES,DAYTYPE) %>%
    dplyr::summarize(NINTS=sum(NINTS),
                     HARVEST=sum(HARVEST,na.rm=TRUE),
                     VHARVEST=ifelse(NINTS==1,NA,sum(VHARVEST,na.rm=TRUE)),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(hf)) %>%
    as.data.frame()
  ## Combine those two summaries to original data.frame
  hf <- rbind(hf,hf2)
  
  ## Summarizes across fisheries
  hf3 <- dplyr::group_by(hf,YEAR,MUNIT,SPECIES,DAYTYPE,MONTH) %>%
    dplyr::summarize(NINTS=sum(NINTS),
                     HARVEST=sum(HARVEST,na.rm=TRUE),
                     VHARVEST=ifelse(NINTS==1,NA,sum(VHARVEST,na.rm=TRUE)),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    dplyr::mutate(FISHERY="All") %>%
    dplyr::select(names(hf)) %>%
    as.data.frame()
  ## Combine those two summaries to original data.frame
  hf <- rbind(hf,hf3) %>%
    ## Add on harvest rate and SD of harvest
    dplyr::mutate(HRATE=HARVEST/INDHRS,
                  SDHARVEST=sqrt(VHARVEST)) %>%
    ## Rearrange variables and rows
    dplyr::select(YEAR:SPECIES,INDHRS,HARVEST,SDHARVEST,VHARVEST,HRATE) %>%
    dplyr::arrange(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,SPECIES)
  ## Return data.frame
  hf
}

## Summarize lengths by species, month, and var
## Note that this is generalized so that var can be either CLIPPED or CLIP
sumLengths <- function(d,var) {
  tmp <- rlang::quo_name(rlang::enquo(var))
  ## summarize by MUNIT, SPECIES, MONTH, and "clip"
  lenSum1 <- dplyr::group_by(d,MUNIT,SPECIES,MONTH,!!rlang::enquo(var)) %>%
    iSumLen()
  ## summarize by MUNIT, SPECIES, and MONTH (across all "clip"s)
  lenSum2 <- dplyr::group_by(d,MUNIT,SPECIES,MONTH) %>% iSumLen() %>%
    dplyr::mutate(TMP="All")
  names(lenSum2)[length(names(lenSum2))] <- tmp
  lenSum2 <- dplyr::select(lenSum2,names(lenSum1))
  ## summarize by MUNIT, SPECIES, and "clip" (across all MONTHs)
  lenSum3 <- dplyr::group_by(d,MUNIT,SPECIES,!!enquo(var)) %>% iSumLen() %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(lenSum1))
  ## summarize by MUNIT and SPECIES (across all MONTHs and "clip"s)
  lenSum4 <- dplyr::group_by(d,MUNIT,SPECIES) %>% iSumLen() %>%
    dplyr::mutate(MONTH="All",TMP="All")
  names(lenSum4)[length(names(lenSum4))] <- tmp
  lenSum4 <- dplyr::select(lenSum4,names(lenSum1))
  ## put them all together
  lenSum <- rbind(lenSum1,lenSum2,lenSum3,lenSum4) %>%
    mutate(SPECIES=factor(SPECIES,levels=lvlsSPECIES),
           MONTH=iOrderMonths(MONTH,addAll=TRUE))
  if (tmp=="CLIPPED") {
    lenSum %<>%
      dplyr::mutate(CLIPPED=factor(CLIPPED,levels=c("No Clip","Clip",
                                                    "Not Checked","All"))) %>%
      dplyr::arrange(MUNIT,SPECIES,MONTH,CLIPPED)
  } else {
    lenSum %<>% 
      dplyr::mutate(CLIP=droplevels(factor(CLIP,levels=c(lvlsCLIP,"All")))) %>%
      dplyr::arrange(MUNIT,SPECIES,MONTH,CLIP)
  }
  ## Remove rows that are duplicates of a previous row for just the summary vars
  lenSum %<>% filter(FSA::repeatedRows2Keep(.,
                      cols2use=c("n","mnLen","sdLen","seLen","minLen","maxLen")))
  ## return data.frame
  as.data.frame(lenSum)
}

## Adds a variable that is a fish's weight predicted from its length using the
## provided length-weight regressions
addWeights <- function(d,RDIR,YEAR) {
  ## Read in the length-weight regression results
  lwregs <- 
    readxl::read_excel(file.path(RDIR,"data",
                                 paste0("LWRegressions_",YEAR,".xlsx"))) %>%
    ## make sure capitalization is the same as in SPECIES
    mutate(SPECIES2=FSA::capFirst(SPECIES)) %>%
    select(SPECIES2,a,b)
  
  ## Temporarily create SPECIES2 to handle different eqn for clipped lake trout
  d %<>% mutate(SPECIES2=as.character(SPECIES))
  d$SPECIES2[d$SPECIES2=="Lake Trout" & d$CLIPPED=="Clip"] <- "Lake Trout (hatchery)"
  ## Add predicted weights based on length and species
  d %<>% 
    ## Temporarily append a and b values for each fish according to its species
    left_join(lwregs,by="SPECIES2") %>%
    ## Predict weights
    mutate(WEIGHT=round(exp(a)*(LENGTH*25.4)^b,0)) %>%
    ## Remove unneeded temporary variables
    select(-SPECIES2,-a,-b)
}


## Convenience function for making a file of the data.frame in x
writeDF <- function(x,fnpre) {
  x1 <- deparse(substitute(x))
  write.csv(x,file=paste0(fnpre,x1,".csv"),
            row.names=FALSE,quote=FALSE,na="")
}


## Combines the three types of CSV files in the RDIR directory that were 
## created after sourcing the LS_Analyzer script. Essentially combines summary
## results across routes within a year.
combineCSV <- function(RDIR,YEAR,removeOrigs=TRUE) {
  types <- c("ttlEffort","ttlHarvest","lengths")
  for (i in types) {
    ## Get list of CSV files of that type in RDIR
    tmp <- list.files(RDIR,pattern=paste0(i,".csv"))
    ## But don't include the COMBINED TYPES
    tmp <- tmp[!grepl("Combined",tmp)]
    for (j in seq_along(tmp)) {
      ## Read and combine the files
      fn <- file.path(RDIR,tmp[j])
      if (j==1) d <- read.csv(fn)
      else d <- rbind(d,read.csv(fn))
    }
    ## Write out the combined file
    fn <- paste0("Combined_Open_",YEAR,"_",i,".csv")
    write.csv(d,file=file.path(RDIR,fn),row.names=FALSE,quote=FALSE,na="")
    ## Remove the original files
    if (removeOrigs) file.remove(file.path(RDIR,tmp))
  }
}



## Internals for Mains ----

## Converts months to an ordered factor
iOrderMonths <- function(x,addAll=FALSE) {
  if (addAll) ordered(x,levels=c(format(ISOdate(2004,1:12,1),"%b"),"All"))
  else ordered(x,levels=format(ISOdate(2004,1:12,1),"%b"))
}

## Convenience function for summarizing lengths
iSumLen <- function(dgb) {
  tmp <- dplyr::summarize(dgb,n=dplyr::n(),
                          mnLen=mean(LENGTH,na.rm=TRUE),
                          sdLen=sd(LENGTH,na.rm=TRUE),
                          seLen=FSA::se(LENGTH,na.rm=TRUE),
                          minLen=min(LENGTH,na.rm=TRUE),
                          maxLen=max(LENGTH,na.rm=TRUE)) %>%
    as.data.frame()
  tmp$sdLen[is.nan(tmp$sdLen)] <- NA
  tmp
}
