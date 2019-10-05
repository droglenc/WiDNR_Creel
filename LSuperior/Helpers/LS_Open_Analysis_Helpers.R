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
## Expand pressure counts from observed days/times to daylengths & days/month
expandPressureCounts <- function(dcnts,cal) {
  ## Isolate daylengths for each month from the calendar
  tmp <- cal[,c("MONTH","DAYLEN")]
  tmp <- tmp[!duplicated(tmp),]
  dcnts %<>%
    ### Adds a temporary day length variable (from cal) to each count
    right_join(tmp,by="MONTH") %>%
    ### Expand observed pressure counts to represent the entire day at each site 
    dplyr::mutate(COUNT=COUNT*DAYLEN/WAIT) %>%
    ### Compute daily pressure counts (across sites within days)
    dplyr::group_by(YEAR,MONTH,DAY,DAYTYPE) %>%
    dplyr::summarize(WAIT=sum(WAIT),COUNT=sum(COUNT)) %>%
    dplyr::ungroup() %>%
    ### Summarizes daily pressure counts by daytype and month
    dplyr::group_by(YEAR,DAYTYPE,MONTH) %>%
    dplyr::summarize(NCOUNT=dplyr::n(),
                     VCOUNT=var(COUNT,na.rm=TRUE),
                     MCOUNT=mean(COUNT,na.rm=TRUE)) %>%
    dplyr::mutate(VCOUNT=VCOUNT/NCOUNT) %>%
    dplyr::ungroup() %>%
    ### Expand by number of days in the month (in cal)
    merge(cal[,c("MONTH","DAYTYPE","DAYS")],by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(TCOUNT=MCOUNT*DAYS,VCOUNT=VCOUNT*(DAYS^2)) %>%
    dplyr::select(YEAR,DAYTYPE,MONTH,NCOUNT,DAYS,TCOUNT,VCOUNT)
  ## Find totals across DAYTYPEs
  dcnts1 <- dplyr::group_by(dcnts,YEAR,MONTH) %>%
    dplyr::summarize(NCOUNT=sum(NCOUNT,na.rm=TRUE),
                     DAYS=sum(DAYS,na.rm=TRUE),
                     TCOUNT=sum(TCOUNT,na.rm=TRUE),
                     VCOUNT=sum(VCOUNT,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(dcnts)) %>%
    as.data.frame()
  dcnts <- rbind(dcnts,dcnts1)
  ## Find totals across all MONTHs
  dcnts2 <- dplyr::group_by(dcnts,YEAR,DAYTYPE) %>%
    dplyr::summarize(NCOUNT=sum(NCOUNT,na.rm=TRUE),
                     DAYS=sum(DAYS,na.rm=TRUE),
                     TCOUNT=sum(TCOUNT,na.rm=TRUE),
                     VCOUNT=sum(VCOUNT,na.rm=TRUE)) %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(dcnts)) %>%
    as.data.frame()
  dcnts <- rbind(dcnts,dcnts2)
  ## Convert to SDs and rearrange variables
  dcnts %<>% dplyr::mutate(SDCOUNT=sqrt(VCOUNT)) %>%
    dplyr::select(YEAR,DAYTYPE,MONTH,NCOUNT,DAYS,TCOUNT,SDCOUNT,VCOUNT) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH,addAll=TRUE),
                  DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All"))) %>%
    dplyr::arrange(YEAR,MONTH,DAYTYPE) %>%
    as.data.frame()
  ## Return data.frame
  dcnts
}


##
sumInterviewedEffort <- function(dints) {
  # All records where fishing was in one state
  f1 <- dplyr::filter(dints,!STATE %in% c("Wisconsin/Minnesota","Wisconsin/Michigan"))
  # All records where fishing was in two states
  f2 <- dplyr::filter(dints,STATE %in% c("Wisconsin/Minnesota","Wisconsin/Michigan")) %>%
    ## Cut the fishing effort (HOURS) in half (apportioned to each state)
    dplyr::mutate(HOURS=0.5*HOURS)
  # Duplicate f2 to get other half of HOURS
  f3 <- f2 %>%
    ## label MUNIT as other state
    dplyr::mutate(MUNIT=ifelse(STATE=="Wisconsin/Minnesota","MN","MI"))
  
  # Combine to get all interviews corrected for location
  f <- rbind(f1,f2,f3) %>%
    ## Compute people-hours of fishing effort (in INDHRS) ... only used to find
    ##   mean party size (in PARTY) below
    ## Compute hours for only completed trips (in CHOURS) ... only used to find
    ##   mean length of a COMPLETED trip (in MTRIP) below.
    dplyr::mutate(INDHRS=PERSONS*HOURS,
                  CHOURS=ifelse(STATUS=="Complete",HOURS,NA))
  
  # Summarize interviewed effort data by MUNIT, DAYTPE, FISHERY, MONTH
  #   across all interviews, sites, and days
  # !! This was how PARTY was calculated in the SAS code ... it is NOT the same
  # !! as calculating the mean of PERSONS ... not clear why computed this way.
  # !! Note that USSHours is the uncorrected SS of HOURS ... this is used later
  # !! to find the variance of the HOURS
  fsum <- f %>%
    dplyr::group_by(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH) %>%
    dplyr::summarize(NINTS=dplyr::n(),
                     MTRIP=mean(CHOURS,na.rm=TRUE),
                     USSHOURS=sum(HOURS^2,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    dplyr::mutate(PARTY=INDHRS/HOURS) %>%
    dplyr::select(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,
                  NINTS,HOURS,MTRIP,USSHOURS,PARTY) %>%
    as.data.frame()
  
  # Find total interviewed hours by MONTH and DAYTYPE (nints in SAS code)
  fsum2 <- dints %>%
    dplyr::group_by(MONTH,DAYTYPE) %>%
    dplyr::summarize(THOURS_MD=sum(HOURS,na.rm=TRUE)) %>%
    as.data.frame()
  
  # Combine the summarized effort with fsum2 to compute PROP
  f <- merge(fsum,fsum2,by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(PROP=HOURS/THOURS_MD) %>%
    dplyr::select(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,
                  NINTS,HOURS,USSHOURS,MTRIP,PROP,PARTY) %>%
    dplyr::arrange(MUNIT,FISHERY,DAYTYPE,MONTH)
  ### Return data.frame
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
    dplyr::mutate(VPHOURS=iConvNA20(VPHOURS),
                  VINDHRS=iConvNA20(VINDHRS),
                  VTRIPS=iConvNA20(VTRIPS)) %>%
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
  ## Compute harvest for each interview
  harv <- d %>%
    dplyr::filter(FISHERY!="non-fishing")

  ## Harvest for when fishing only in one state
  h1 <- dplyr::filter(harv,!STATE %in% c("Wisconsin/Minnesota","Wisconsin/Michigan"))
  ## Harvest for when fishing in more than one state ... harvest cut in half
  h2 <- dplyr::filter(harv,STATE %in% c("Wisconsin/Minnesota","Wisconsin/Michigan")) %>%
    dplyr::mutate(HOURS=0.5*HOURS,HARVEST=0.5*HARVEST)
  ## Duplicated h2 to get other half of HOURS/HARVEST, label MUNITS
  h3 <- h2 %>%
    dplyr::mutate(MUNIT=ifelse(STATE=="Wisconsin/Minnesota","MN","MI"))
  ## Combine to get all interviews corrected for location
  harv <- rbind(h1,h2,h3)
  ## Summarize harvest by strata and species
  ##   USSHARVEST is uncorrected SS for computer variance of HARVEST later
  ##   UCOVAR is intermediate value to compute HARVEST HOURS covariance later
  harv %<>% dplyr::group_by(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,SPECIES) %>%
    dplyr::summarize(USSHARVEST=sum(HARVEST^2,na.rm=TRUE),
                     UCOVAR=sum(HARVEST*HOURS,na.rm=TRUE),
                     HARVEST=sum(HARVEST,na.rm=TRUE)) %>%
    ## Note that HOURS is not returned but it is merged from the summarized
    ## effort data (by waters, fishery, daytype, and month) in the next function
    dplyr::select(YEAR,MUNIT,FISHERY,DAYTYPE,MONTH,SPECIES,
                  HARVEST,USSHARVEST,UCOVAR)
  ### Return data.frame
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
    mutate(SPECIES=iHndlSpecies(SPECIES),
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

## Adds a variable that is a fish's weight predicted from its length using the
## provided length-weight regressions
addWeights <- function(d,RDIR,YEAR) {
  ## Read in the length-weight regression results
  lwregs <- readxl::read_excel(file.path(RDIR,
                               paste0("LWRegressions_",YEAR,".xlsx"))) %>%
    ## make sure capitalization is the same as in SPECIES
    mutate(SPECIES2=FSA::capFirst(SPECIES)) %>%
    select(SPECIES2,a,b)
  
  ## Temporarily create SPECIES2 to handle different eqn for clipped lake trout
  d %<>% mutate(SPECIES2=as.character(SPECIES))
  d$SPECIES2[d$SPECIES2=="Lake Trout" & d$CLIPPED=="Clip"] <- "Lake Trout (hatchery)"
  ## Temporarily append a and b values for each fish according to its species
  d %<>% left_join(lwregs,by="SPECIES2") %>%
    mutate(WEIGHT=round(exp(a)*(LENGTH*25.4)^b,0)) %>%
    select(-SPECIES2,-a,-b)
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
    tmp <- tmp[!grepl("COMBINED",tmp)]
    for (j in seq_along(tmp)) {
      ## Read and combine the files
      fn <- file.path(RDIR,tmp[j])
      if (j==1) d <- read.csv(fn)
      else d <- rbind(d,read.csv(fn))
    }
    ## Write out the combined file
    fn <- paste0("COMBINED_",YEAR,"_",i,".csv")
    write.csv(d,file=file.path(RDIR,fn),row.names=FALSE,quote=FALSE,na="")
    ## Remove the original files
    if (removeOrigs) file.remove(file.path(RDIR,tmp))
  }
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
    dplyr::arrange(STATE,DAYTYPE,FISHERY) %>%
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
                  Weekday.NCOUNT,Weekday.TCOUNT,Weekday.SDCOUNT,
                  Weekend.NCOUNT,Weekend.TCOUNT,Weekend.SDCOUNT,
                  All.NCOUNT,All.TCOUNT,All.SDCOUNT) %>%
    dplyr::arrange(MONTH)

  ## Make the huxtable
  tmpTbl2 <- as_hux(tmp) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_top_padding(row=final(1),col=everywhere,value=10) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_left_padding(row=everywhere,col=ends_with(".NCOUNT"),value=15) %>%
    # No decimals on NCOUNT and TCOUNT
    set_number_format(row=everywhere,col=ends_with(".NCOUNT"),value=0) %>%
    set_number_format(row=everywhere,col=ends_with(".TCOUNT"),value=0) %>%
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
    dplyr::mutate(WATERS=iHndlWaters(WATERS,addAll=TRUE),
                  FISHERY=iHndlFishery(FISHERY,addAll=TRUE),
                  MONTH=iOrderMonths(MONTH,addAll=TRUE),
                  DAYTYPE=iHndlDaytype(DAYTYPE,addAll=TRUE)) %>%
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
                  WATERS=iHndlWaters(WATERS,addAll=TRUE),
                  DAYTYPE=iHndlDaytype(DAYTYPE,addAll=TRUE),
                  FISHERY=iHndlFishery(FISHERY,addAll=TRUE),
                  SPECIES=iHndlSpecies(SPECIES)) %>%
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
    dplyr::filter(!is.na(LENGTH)) %>%
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
    dplyr::filter(!is.na(LENGTH))
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
    dplyr::select(SPECIES,MUNIT,SITE,FISHERY,DATE,LENGTH,CLIP) %>%
    dplyr::arrange(SPECIES,DATE,LENGTH) %>%
    dplyr::rename(`TL (in)`=LENGTH) %>%
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
    dplyr::filter(WATERS=="Wisconsin",MONTH!="All") %>%
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
    dplyr::filter(WATERS=="Wisconsin",MONTH!="All") %>%
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
    dplyr::filter(WATERS=="Wisconsin") %>%
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
    dplyr::filter(WATERS=="Wisconsin")
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
    dplyr::filter(WATERS=="Wisconsin")
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
  p <- ggplot(data=dlen,aes(x=LENGTH,fill=CLIPPED)) +
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
## Convenience function for making a file of the data.frame in x
writeDF <- function(x,fnpre) {
  x1 <- deparse(substitute(x))
  write.csv(x,file=paste0(fnpre,x1,".csv"),
            row.names=FALSE,quote=FALSE,na="")
}

# Turn month numbers into abbreviations and have Dec be first
iHndlMonth <- function(x,droplevels) {
  x <- factor(month.abb[x],levels=month.abb,ordered=TRUE)
  if (droplevels) x <- droplevels(x)
  x
}

# Simplify "Weekend/Holiday" to "Weekend"
iMvDaytype <- function(x) {
  FSA::mapvalues(x,from="Weekend/Holiday",to="Weekend")
}

iHndlDaytype <- function(x,addAll=FALSE,droplevels=FALSE) {
  tmp <- c("Weekday","Weekend")
  if (addAll) tmp <- c(tmp,"All")
  x <- factor(x,levels=tmp)
  if (droplevels) x <- droplevels(x)
  x
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

# Make more useful site description (site number plus site description)
iHndlSiteDesc <- function(s,d) { paste0(s,"-",FSA::capFirst(d)) }

# Convert NAs to zeroes
iNA2zero <- function(x) { ifelse(is.na(x),0,x) }

## Creates day lengths from month names
iMvDaylen <- function(x,DAY_LENGTH) {
  daylens <- DAY_LENGTH[as.character(x)]
  names(daylens) <- NULL
  daylens
}

## Create "waters" variable to identify if the fished area was in WI or not
iMvWaters <- function(x) {
  x <- dplyr::case_when(
    x=="MN" ~ "Non-Wisconsin",
    x=="MI" ~ "Non-Wisconsin",
    TRUE ~ "Wisconsin"
  )
  x
}

iHndlWaters <- function(x,addAll=TRUE,droplevels=FALSE) {
  tmp <- c("Wisconsin","Non-Wisconsin")
  if (addAll) tmp <- c(tmp,"All")
  x <- factor(x,levels=tmp)
  if (droplevels) x <- droplevels(x)
  x
}

## Set levels for the fisheries
## DEREK --> ASK WHAT THIS ENTIRE LIST WILL BE
iHndlFishery <- function(x,addAll=FALSE,droplevels=FALSE) {
  tmp <- c("Open-Water Cold","Open-Water Cool","Pleasure","Shore","Stream",
           "Tribal")
  if (addAll) tmp <- c(tmp,"All")
  x <- factor(x,levels=tmp)
  if (droplevels) x <- droplevels(x)
  x
}

iMvMgmtUnit <- function(x,LOC,addALL=FALSE,droplevels=FALSE) {
  tmp <- c("WI-1","WI-2","MI","MN")
  if (addALL) tmp <- c(tmp,"All")
  x <- factor(x,levels=tmp)
  if (droplevels) x <- droplevels(x)
  x
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
iHndlSpecies <- function(x,droplevels=FALSE) {
  tmp <- c('Lake Trout','Siscowet Lake Trout','Atlantic Salmon','Brook Trout',
           'Brown Trout','Coho Salmon','Chinook Salmon','Pink Salmon',
           'Rainbow Trout','Splake','Burbot','Lake Herring','Lake Whitefish',
           'Round Whitefish','Smelt','Lake Sturgeon',
           'Walleye','Yellow Perch','Ruffe','White Perch',
           'Northern Pike',"Muskellunge",'Bluegill','Pumpkinseed','Sunfish Spp.',
           'Black Crappie','Crappie','Largemouth Bass','Rock Bass',
           'Smallmouth Bass','Channel Catfish','Common Carp','All Fish')
  x <- factor(FSA::capFirst(x),levels=tmp)
  if (droplevels) x <- droplevels(x)
  x
}

## Converts months to an ordered factor
iOrderMonths <- function(x,addAll=FALSE) {
  if (addAll) ordered(x,levels=c(format(ISOdate(2004,1:12,1),"%b"),"All"))
  else ordered(x,levels=format(ISOdate(2004,1:12,1),"%b"))
}


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
