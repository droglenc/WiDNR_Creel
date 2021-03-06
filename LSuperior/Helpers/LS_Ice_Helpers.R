#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE (unless you know what you are doing)!!!
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Load Packages ---------------------------------------------------------------
rqrd <- c("FSA","tidyr","dplyr","huxtable","captioner","knitr","here","readxl")
for (i in seq_along(rqrd)) 
  suppressPackageStartupMessages(library(rqrd[i],character.only=TRUE))



## Constants -------------------------------------------------------------------
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

lvlsFISHERY <- c('< 60 ft - Shallow','> 60 ft - Bobbing','Pleasure',
                 'Dec Open-Water','Tribal','NOP Spearing')

## Main Helpers ----------------------------------------------------------------
# Convert NAs to zeroes
NA2zero <- function(x) ifelse(is.na(x),0,x)


expandPressureCount <- function(pc,intvs,pints) {
  ## Number of interviews at each SITE within MONTH, DAYTYPE, FISHERY
  tmp1 <- intvs %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,DAYTYPE,SITE,FISHERY) %>%
    dplyr::summarize(NINTS=dplyr::n())
  ## Number of interviews at each SITE within MONTH, DAYTYPE
  tmp2 <- tmp1 %>%
    dplyr::summarize(TTLINTS=sum(NINTS)) %>%
    as.data.frame()
  ## Combine last two data.frames and then compute proportion of interviews at
  ## a SITE within each FISHERY by MONTH, DAYTYPE
  obsPropIntsInFshry <- 
    dplyr::left_join(as.data.frame(tmp1),tmp2,
                     by=c("SURVEY","ROUTE","UNIT","MONTH","DAYTYPE","SITE")) %>%
    dplyr::mutate(pIntsInFishery=NINTS/TTLINTS)
  
  # Find number of vehicles at each SITE within each FISHERY by MONTH, DAYTYPE
  obsNumVehInFshry <- 
    dplyr::full_join(obsPropIntsInFshry,pc,
                     by=c("SURVEY","ROUTE","MONTH","DAYTYPE","SITE")) %>%
    dplyr::rename(ttlVehSite=ttlVeh) %>%
    dplyr::mutate(UNIT=unique(UNIT[!is.na(UNIT)])) %>%
    dplyr::select(-cntDays,-TTLDAYS,-cntdVeh)
  
  # Separate into those ...
  # ... without observed interviews
  tmp <- filter(obsNumVehInFshry,is.na(pIntsInFishery)) %>%
    select(-FISHERY,-pIntsInFishery)
  # ... with observed interviews
  obsNumVehInFshry <- filter(obsNumVehInFshry,!is.na(pIntsInFishery))
  # Join the pints from pints file onto those without observed interviews
  tmp <- 
    left_join(tmp,pints,by=c("SURVEY","ROUTE","MONTH","DAYTYPE","SITE")) %>%
    filter(pIntsInFishery>0) %>%
    select(names(obsNumVehInFshry))
  
  # Combine those that had interviews with the new without interviews
  tmp <- 
    rbind(obsNumVehInFshry,tmp) %>%
    # rearrange variables
    arrange(SURVEY,ROUTE,MONTH,SITE,FISHERY,DAYTYPE) %>%
    # Compute total vehicles at each SITE in each FISHERY
    dplyr::mutate(ttlVehSiteFshry=ttlVehSite*pIntsInFishery)
  
  # Return the data.frame  
  as.data.frame(tmp)
}



sumInterviewedEffort <- function(dints) {
  fsum <- dints %>%
    ### Compute people-hours of fishing effort (in INDHRS) ... only used to find
    ###   mean party size (in PARTY) below
    dplyr::mutate(INDHRS=PERSONS*HOURS) %>%
    ## Summarize interviewed effort data by MUNIT, DAYTPE, FISHERY, MONTH
    ##   across all interviews, sites, and days
    dplyr::group_by(SURVEY,ROUTE,UNIT,FISHERY,DAYTYPE,MONTH) %>%
    dplyr::summarize(NINTS=dplyr::n(),
                     MTRIP=mean(HOURS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     USSHOURS=sum(HOURS^2,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    ## !! This was how PARTY was calculated in the SAS OW code ... it is NOT the same
    ## !! as calculating the mean of PERSONS ... not clear why computed this way.
    ## !! Note that USSHours is the uncorrected SS of HOURS ... this is used later
    ## !! to find the variance of the HOURS
    dplyr::mutate(PARTY=INDHRS/HOURS) %>%
    as.data.frame()

  ## Find total interviewed hours by MONTH and DAYTYPE (nints in SAS OW code)
  fsum2 <- dints %>%
    dplyr::group_by(MONTH,DAYTYPE) %>%
    dplyr::summarize(THOURS_MD=sum(HOURS,na.rm=TRUE)) %>%
    as.data.frame()

  ## Combine the summarized effort with fsum2 to compute PROP
  f <- merge(fsum,fsum2,by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(PROP=HOURS/THOURS_MD) %>%
    dplyr::select(SURVEY,ROUTE,UNIT,FISHERY,DAYTYPE,MONTH,
                  NINTS,HOURS,USSHOURS,MTRIP,PROP,PARTY) %>%
    dplyr::arrange(SURVEY,ROUTE,UNIT,FISHERY,DAYTYPE,MONTH)
  ## Return data.frame (not a tibble)
  as.data.frame(f)
}


## Summarized total fishing effort by strata
sumEffort <- function(ieff,pct) {
  # Combine interview effort and pressure counts with new calculations ...
  eff <- merge(ieff,pct,by=c("SURVEY","ROUTE","UNIT","MONTH","FISHERY","DAYTYPE")) %>%
    dplyr::mutate(PHOURS=ttlVehFshry*MTRIP,
                  INDHRS=PHOURS*PARTY) %>%
    dplyr::rename(NINTS=NINTS.x) %>%
    dplyr::select(SURVEY,ROUTE,UNIT,FISHERY,DAYTYPE,MONTH,ttlVehFshry,
                  NINTS,HOURS,USSHOURS,PHOURS,INDHRS) %>%
    as.data.frame()
  
  ## Summarize across Daytypes
  eff1 <- dplyr::group_by(eff,SURVEY,ROUTE,UNIT,FISHERY,MONTH) %>%
    dplyr::summarize(ttlVehFshry=sum(ttlVehFshry,na.rm=TRUE),
                     NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     USSHOURS=sum(USSHOURS,na.rm=TRUE),
                     PHOURS=sum(PHOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(eff)) %>%
    as.data.frame()
  ## Combine those summaries to original data.frame
  eff <- rbind(eff,eff1)
  
  ## Summarize across Months
  eff2 <- dplyr::group_by(eff,SURVEY,ROUTE,UNIT,FISHERY,DAYTYPE) %>%
    dplyr::summarize(ttlVehFshry=sum(ttlVehFshry,na.rm=TRUE),
                     NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     USSHOURS=sum(USSHOURS,na.rm=TRUE),
                     PHOURS=sum(PHOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(eff)) %>%
    as.data.frame()
  ## Combine those summaries to original data.frame
  eff <- rbind(eff,eff2)
  
  ## Summarize across Fisheries
  eff3 <- dplyr::group_by(eff,SURVEY,ROUTE,UNIT,MONTH,DAYTYPE) %>%
    dplyr::summarize(ttlVehFshry=sum(ttlVehFshry,na.rm=TRUE),
                     NINTS=sum(NINTS,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     USSHOURS=sum(USSHOURS,na.rm=TRUE),
                     PHOURS=sum(PHOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE)) %>%
    dplyr::mutate(FISHERY="All") %>%
    dplyr::select(names(eff)) %>%
    as.data.frame()
  ## Combine those summaries to original data.frame
  eff <- rbind(eff,eff3) %>%
    ## Add on mean persons per party and mean trip length
    dplyr::mutate(PARTY=INDHRS/PHOURS,MTRIP=PHOURS/ttlVehFshry) %>%
    ## Put variables in specific order
    dplyr::select(SURVEY,ROUTE,UNIT,FISHERY,DAYTYPE,MONTH,ttlVehFshry,
                  PHOURS,PARTY,INDHRS,MTRIP,
                  NINTS,HOURS,USSHOURS) %>%
    ## Arrange
    dplyr::arrange(SURVEY,ROUTE,UNIT,FISHERY,DAYTYPE,MONTH)
  # return final data.frame
  eff
}

expandHarv <- function(intvsNF,intvsO,pcs) {
  ## Summarized observations about angling effort and success by MONTH, FISHERY,
  ## and DAYTYPE.
  f <- intvsNF %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(nInts=dplyr::n(),
                     obsAnglers=sum(PERSONS),
                     obsHours=sum(HOURS),
                     obsAnglerHours=sum(PERSONS*HOURS),
                     obsSuccAnglers=sum(SUCCESS)) %>%
    as.data.frame()
  
  ## Summarized observations about harvest of each SPECIES (including "All Fish")
  ## by MONTH, FISHERY, and DAYTYPE.
  h <- intvsO %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPECIES,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(obsHarvest=sum(NUM)) %>%
    as.data.frame()
  
  ## Create vector of variables to join by to be used below
  join_bys <- c("SURVEY","ROUTE","UNIT","MONTH","FISHERY","DAYTYPE")
  
  eh <- 
    ## Combine observed harvest and individual effort summaries
    dplyr::full_join(h,f,by=join_bys) %>%
    dplyr::mutate(
      # Include observed # successful anglers only for "All Fish" records
      obsSuccAnglers=ifelse(SPECIES=="All Fish",obsSuccAnglers,NA),
      # Compute harvest rate
      obsHrate=obsHarvest/obsAnglerHours,
      # Compute number of hours to catch a fish, only if fish were harvested
      obsHrsperfish=ifelse(obsHrate>0,1/obsHrate,NA),
      # Compute average trip length
      obsAvgTrip=obsHours/nInts,
      # Compute average number of hours fishing per angler
      obsAvgAnglerHours=obsAnglerHours/obsAnglers,
      # Compute average number of anglers per intereview
      obsAnglersPerInt=obsAnglers/nInts,
      # Compute percent of anglers that were successful
      obsPercSucc=obsSuccAnglers/obsAnglers) %>%
    ## Remove unneeded variable
    dplyr::select(-nInts) %>%
    ## Make a data.frame (not a tibble)
    as.data.frame() %>%
    ## Join on effort (total vehicles)
    dplyr::full_join(pcs,by=join_bys) %>%
    ## Remove unneeded variable
    dplyr::select(-NINTS) %>%
    ## Expand observed results to total results
    dplyr::mutate(ttlAnglers=ttlVehFshry*obsAnglersPerInt,
                  ttlAnglerHours=ttlAnglers*obsAvgAnglerHours,
                  ttlSuccAnglers=ttlAnglers*obsPercSucc,
                  ttlHarvest=ttlAnglerHours*obsHrate) %>%
    ## Rearrange variables
    dplyr::arrange(SURVEY,ROUTE,UNIT,SPECIES,MONTH,FISHERY,DAYTYPE)
  ## Return a data.frame (not a tibble)
  as.data.frame(eh)
}







## Summarize the expected harvest for the "All Fish" records only
sumExpHarvestAll <- function(eh) {
  # Helper function to compute tedious harvest summary calculations
  iSumHarv <- function(d) summarize(d,
                                    ttlVehFshry=sum(ttlVehFshry),
                                    ttlAnglers=sum(ttlAnglers),
                                    ttlAnglerHours=sum(ttlAnglerHours),
                                    ttlSuccAnglers=sum(ttlSuccAnglers),
                                    ttlHarvest=sum(ttlHarvest))
  # Start main function
  ## Isolate all fish and the total variables
  eh <- eh %>%
    filter(SPECIES=="All Fish") %>%
    select(SURVEY,ROUTE,UNIT,SPECIES,MONTH,FISHERY,DAYTYPE,
           ttlVehFshry,ttlAnglers,ttlAnglerHours,ttlSuccAnglers,ttlHarvest) %>%
    ## Make MONTH and DAYTYPE as characters to make new levels work below
    mutate(MONTH=as.character(MONTH),
           DAYTYPE=as.character(DAYTYPE))

  ## Find all possible "All"s (for FISHERY, MONTH, and DAYTYPE)
  ### Sum total variables across FISHERYs
  tmp1 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPECIES,DAYTYPE,MONTH) %>%
    iSumHarv() %>%
    mutate(FISHERY="All") %>%
    select(names(eh))
  ### ... and across MONTHs
  tmp2 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPECIES,FISHERY,DAYTYPE) %>%
    iSumHarv() %>%
    mutate(MONTH="All") %>%
    select(names(eh))
  ### ... and across DAYTYPEs
  tmp3 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPECIES,MONTH,FISHERY) %>%
    iSumHarv() %>%
    mutate(DAYTYPE="All") %>%
    select(names(eh))
  ### ... and across FISHERYs and MONTHs
  tmp4 <- tmp1 %>%
    iSumHarv() %>%
    mutate(FISHERY="All",MONTH="All") %>%
    select(names(eh))
  ### ... and across FISHERYs and DAYTYPEs
  tmp5 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPECIES,MONTH) %>%
    iSumHarv() %>%
    mutate(FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  ### ... and across MONTHs and DAYTYPEs
  tmp6 <- tmp2 %>%
    iSumHarv() %>%
    mutate(MONTH="All",DAYTYPE="All") %>%
    select(names(eh))
  ### ... and across MONTHs, DAYTYPES, and FISHERYs
  tmp7 <- tmp6 %>%
    iSumHarv() %>%
    mutate(MONTH="All",FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  
  ## Combine the original data.frame with all of the harvest summaries
  ehAll <- rbind(eh,as.data.frame(tmp1),as.data.frame(tmp2),as.data.frame(tmp3),
                 as.data.frame(tmp4),as.data.frame(tmp5),as.data.frame(tmp6),
                 as.data.frame(tmp7)) %>%
    ### Add an "All" to the MONTH, FISHERY, and DAYTYPE levels
    mutate(MONTH=factor(MONTH,levels=c(month.abb[c(12,1:11)],"All")),
           FISHERY=factor(FISHERY,levels=c(lvlsFISHERY,"All")),
           DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All")),
           ### Calculate the "rates" from the totals
           anglersPerInt=ttlAnglers/ttlVehFshry,
           avgAnglerHours=ttlAnglerHours/ttlAnglers,
           percSucc=ttlSuccAnglers/ttlAnglers*100,
           harvestRate=ttlHarvest/ttlAnglerHours) %>%
    ### Order the records
    arrange(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE,SPECIES) %>%
    ### Order and select the variables
    select(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE,SPECIES,
           ttlVehFshry,anglersPerInt,ttlAnglers,avgAnglerHours,
           ttlAnglerHours,percSucc,ttlSuccAnglers,harvestRate,ttlHarvest)
  ## Return a data.frame (not a tibble)
  as.data.frame(ehAll)
}


## Summarize the expected harvest for the individual species records only
sumExpHarvestSPP <- function(eh,seha) {
  ## Isolate just the ttlAnglerHours from summary of overall harvest
  seha <- select(seha,SURVEY:DAYTYPE,ttlAnglerHours)
  
  ## Isolate individual fish species and the total variables in expHarv
  eh <- eh %>%
    filter(SPECIES!="All Fish") %>%
    select(SURVEY,ROUTE,UNIT,SPECIES,MONTH,FISHERY,DAYTYPE,ttlHarvest)
  
  ## Find all possible "All"s (for FISHERY, MONTH, and DAYTYPE)
  ### Sum total variables across FISHERYs
  tmp1 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPECIES,DAYTYPE,MONTH) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(FISHERY="All") %>%
    select(names(eh))
  ### ... and across MONTHs
  tmp2 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPECIES,FISHERY,DAYTYPE) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(MONTH="All") %>%
    select(names(eh))
  ### ... and across DAYTYPEs
  tmp3 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPECIES,MONTH,FISHERY) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(DAYTYPE="All") %>%
    select(names(eh))
  ### ... and across FISHERYs and MONTHs
  tmp4 <- tmp1 %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(FISHERY="All",MONTH="All") %>%
    select(names(eh))
  ### ... and across FISHERYs and DAYTYPEs
  tmp5 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPECIES,MONTH) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  ### ... and across MONTHs and DAYTYPEs
  tmp6 <- tmp2 %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(MONTH="All",DAYTYPE="All") %>%
    select(names(eh))
  ### ... and across MONTHs, DAYTYPES, and FISHERYs
  tmp7 <- tmp6 %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(MONTH="All",FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  
  ## Combine original data.frame and summaries just found
  ehSPP <- rbind(eh,as.data.frame(tmp1),as.data.frame(tmp2),as.data.frame(tmp3),
                 as.data.frame(tmp4),as.data.frame(tmp5),as.data.frame(tmp6),
                 as.data.frame(tmp7)) %>%
    ### Append on ttlAnglerHrs
    dplyr::full_join(seha,by=c("SURVEY","ROUTE","UNIT","MONTH",
                                 "FISHERY","DAYTYPE")) %>%
    ### Adjust the levels of MONTH, FISHERY, and DAYTYPE to include "All"
    dplyr::mutate(MONTH=factor(MONTH,levels=c(month.abb[c(12,1:11)],"All")),
                  FISHERY=factor(FISHERY,levels=c(lvlsFISHERY,"All")),
                  DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All")),
                  SPECIES=factor(SPECIES,levels=lvlsSPECIES),
                  ### calculate the "rates" from the totals
                  harvestRate=ttlHarvest/ttlAnglerHours) %>%
    ### Order the fields
    arrange(SURVEY,ROUTE,UNIT,SPECIES,FISHERY,MONTH,DAYTYPE) %>%
    ### Arrange and select the final variables
    select(SURVEY,ROUTE,UNIT,SPECIES,FISHERY,MONTH,DAYTYPE,harvestRate,ttlHarvest)
  ## Return a data.frame (and not a tibble)
  as.data.frame(ehSPP)
}


sumLengths <- function(f) {
  # Helper convenience function
  iSumLengths <- function(d) {
    ## Compute summaries
    tmp <- dplyr::summarize(d,n=dplyr::n(),mnLen=mean(LENGTH,na.rm=TRUE),
                    sdLen=sd(LENGTH,na.rm=TRUE),minLen=min(LENGTH,na.rm=TRUE),
                    maxLen=max(LENGTH,na.rm=TRUE))
    ## Convert NaNs to NAs (helps in huxtable)
    tmp$sdLen[is.nan(tmp$sdLen)] <- NA
    ## Return a data.frame (not a tibble)
    as.data.frame(tmp)
  }
  
  # Main function
  ## Summarize lengths by MONTH, FISHERY, ORIGIN
  tmp1 <- f %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPECIES,MONTH,FISHERY,ORIGIN) %>%
    iSumLengths() %>%
    arrange(SURVEY,ROUTE,UNIT,SPECIES,MONTH,FISHERY,ORIGIN) %>%
    as.data.frame()
  ### ... and across ORIGINs
  tmp2 <- f %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPECIES,MONTH,FISHERY) %>%
    iSumLengths() %>%
    dplyr::mutate(ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  ### ... and across FISHERYs
  tmp3 <- f %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPECIES,MONTH,ORIGIN) %>%
    iSumLengths() %>%
    dplyr::mutate(FISHERY="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  ### ... and across ORIGINs and FISHERYs
  tmp4 <- f %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPECIES,MONTH) %>%
    iSumLengths() %>%
    dplyr::mutate(FISHERY="All",ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  ### ... and across MONTHs
  tmp5 <- f %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPECIES,FISHERY,ORIGIN) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  ### ... and across MONTHs and ORIGINs
  tmp6 <- f %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPECIES,FISHERY) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All",ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  ### ... and across MONTHs and FISHERYs
  tmp7 <- f %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPECIES,ORIGIN) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All",FISHERY="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  ### ... and across MONTHs, FISHERYs, and ORIGINs
  tmp8 <- f %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPECIES) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All",FISHERY="All",ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  ## Combine all the summaries
  tmp <- rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8) %>%
    ## Properly factor ORIGIN and FISHERY
    dplyr::mutate(ORIGIN=factor(ORIGIN,levels=c("Native","Hatchery","All")),
                  FISHERY=factor(FISHERY,levels=c(lvlsFISHERY,"All"))) %>%
    ## Sort the rows
    dplyr::arrange(SURVEY,ROUTE,UNIT,SPECIES,MONTH,FISHERY,ORIGIN) %>%
    ## Arrange the variables to return
    dplyr::select(SURVEY,ROUTE,UNIT,MONTH,FISHERY,SPECIES,ORIGIN,n:maxLen)
  ## Return a data.frame (not a tibble)
  as.data.frame(tmp)
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
    fn <- paste0("Combined_Ice_",YEAR,"_",i,".csv")
    write.csv(d,file=file.path(RDIR,fn),row.names=FALSE,quote=FALSE,na="")
    ## Remove the original files
    if (removeOrigs) file.remove(file.path(RDIR,tmp))
  }
}
