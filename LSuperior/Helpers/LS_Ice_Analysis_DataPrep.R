#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Load Packages ---------------------------------------------------------------
rqrd <- c("FSA","tidyr","dplyr","huxtable","captioner","knitr","here","readxl")
for (i in seq_along(rqrd))
  suppressPackageStartupMessages(library(rqrd[i],character.only=TRUE))



#### Helper Functions ----------------------------------------------------------
# Turn month numbers into abbreviations and have Dec be first
iHndlMonth <- function(x) factor(month.abb[x],levels=month.abb[c(12,1:11)])

# Simplify "Weekend/Holiday" to "Weekend"
iHndlDaytype <- function(x) FSA::mapvalues(x,from="Weekend/Holiday",to="Weekend")

# Make more useful site description (site number plus site description)
iHndlSiteDesc <- function(s,d) paste0(s,"-",FSA::capFirst(d))

# Convert NAs to zeroes
iNA2zero <- function(x) ifelse(is.na(x),0,x)

# Helps compute tedious harvest summary calculations
iSumHarv <- function(d) summarize(d,
                                  ttlVehFshry=sum(ttlVehFshry),
                                  ttlAnglers=sum(ttlAnglers),
                                  ttlAnglerHours=sum(ttlAnglerHours),
                                  ttlSuccAnglers=sum(ttlSuccAnglers),
                                  ttlHarvest=sum(ttlHarvest))

iSumLengths <- function(d) {
  tmp <- dplyr::summarize(d,
                          n=dplyr::n(),
                          mnLen=mean(LENGTH,na.rm=TRUE),
                          sdLen=sd(LENGTH,na.rm=TRUE),
                          minLen=min(LENGTH,na.rm=TRUE),
                          maxLen=max(LENGTH,na.rm=TRUE)) %>%
    as.data.frame()
  # Convert NaNs to NAs (helps in huxtable)
  tmp$sdLen[is.nan(tmp$sdLen)] <- NA
  tmp
}

iOrderSpecies <- function(spp) {
  lvls <- c("Lake Trout","Siscowet","Brook Trout","Splake",
            "Coho Salmon","Chinook Salmon","Rainbow Trout","Pink Salmon",
            "Brown Trout","Atlantic Salmon","Lake Whitefish","Round Whitefish",
            "Lake Herring","Rainbow Smelt","Burbot","Lake Sturgeon","Walleye",
            "Yellow Perch","Ruffe","Northern Pike",
            "Smallmouth Bass","Largemouth Bass",
            "Bluegill","Pumpkinseed","Sunfish spp","Black Crappie",
            "Crappie spp.","Rock Bass","White Perch","Catfish","Carp")
  if (any(!unique(spp) %in% lvls)) stop("A species not listed!!")
  factor(spp,levels=lvls)
}


## Main Analysis ----

## Pressure counts ----
fdays <- 
  ## Read FDAYS Excel file
  readxl::read_excel(file.path(RDIR,"qry_ice_fdays_4R.xlsx")) %>%
  ## Filter to the route in LOC
  dplyr::filter(ROUTE==LOC) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Change FDAYS variable to ttlDays
  dplyr::rename(ttlDays=FDAYS) %>%
  ## Factor MONTH and DAYTYPE, and create combined site name
  dplyr::mutate(MONTH=iHndlMonth(MONTH),
                DAYTYPE=iHndlDaytype(DAYTYPE),
                SITE=iHndlSiteDesc(SITE,SITEDESC)) %>%
  ## Remove variables that are not needed further below
  dplyr::select(-FDAYSKEY,-SITEDESC,-COMMENTS) %>%
  as.data.frame()

##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!
##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!
####### Temporarily and propInts for each site ... ultimately Dray will have
####### this incorporated into the FDAY file
#######    All but 204-2nd Landing will be all "<60 ft-Shallow"
fdays <- fdays %>%
  mutate('< 60 ft - Shallow'=case_when(
                             SITE=="204-2nd Landing" & MONTH=="Jan" ~ 0.8,
                             SITE=="204-2nd Landing" & MONTH=="Feb" ~ 0.7,
                             SITE=="204-2nd Landing" & MONTH=="Mar" ~ 0.6,
                             TRUE ~ 1),
         '> 60 ft - Bobbing'=case_when(SITE=="204-2nd Landing" & MONTH=="Jan" ~ 0.2,
                             SITE=="204-2nd Landing" & MONTH=="Feb" ~ 0.3,
                             SITE=="204-2nd Landing" & MONTH=="Mar" ~ 0.4,
                             TRUE ~ 0),
         Tribal=0,
         Pleasure=0,
         'Post LT Open-Water'=0)
##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!
##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!##!!


# RESULT: data.frame of the proportion of interviews at a given SITE that would
#         be allocated to the different FISHERYs in the instances where no
#         interviews were recorded at that site, by MONTH and DAYTYPE.
#   * pIntsInFishery: proportion of interviews at a given SITE that would
#                     be allocated to the different FISHERYs
# NOTES:
#   * Just a rearranged (to long format) of information in fdays data.frame
# USE: 
#   * XXX
# EXPORTED: Not exported.
pints <- 
  select(fdays,-ttlDays) %>%
  tidyr::gather(key="FISHERY",value="pIntsInFishery",-(SURVEY:SITE))


# RESULT: data.frame of fishable days at each SITE by MONTH and DAYTYPE.
#   * ttlDays: Number of fishable (i.e., "good ice") days
# NOTES:
#   * The fdays data.frame without the proportion of interviews at a SITE in
#     each FISHERY.
# USE: 
#   * To expand the observed pressure counts to all fishable days at site.
# EXPORTED: Not exported.
fdays <- select(fdays,SURVEY:ttlDays)



# RESULT: data.frame of pressure count data expanded to represent the number of
#         vehicles at each SITE by MONTH and DAYTYPE.
#   * cntDays: Days that vehicles were counted at that SITE, MONTH, DAYTYPE
#   * cntdVeh: Total number of vehicle counted at that SITE, MONTH, DAYTYPE
#   * ttlDays: Total fishable days at that SITE, MONTH, DAYTYPE (from fdays)
#   * ttlVeh: Expanded (by number of fishable days) number of vehicles at that
#             SITE, MONTH, DAYTYPE
# NOTES:
#   * Same (with some issues and slightly different format of variables) as
#     2019 ASHLAND ICE CREEL MONTHLY PRESSURE.XLSX
# USE:
#   * XXX
#   * Table 1
# EXPORTED: not exported
pressureCount <-
  # Read observed counts and finds total number of vehicles at each site and day
  readxl::read_excel(file.path(RDIR,"qry_ice_counts_4R.xlsx")) %>%
  ## Filter to the route in LOC
  dplyr::filter(ROUTE==LOC) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Factor MONTH and DAYTYPE, and create combined site name
  dplyr::mutate(MONTH=iHndlMonth(MONTH),
                DAYTYPE=iHndlDaytype(DAYTYPE),
                SITE=iHndlSiteDesc(SITE,SITEDESC),
                ## Sum all vehicle counts (treat NAs as 0s so they will sum)
                ALLVEH=iNA2zero(MORNVEHR)+iNA2zero(EVEVEHR)+
                  iNA2zero(MORNVEHNR)+iNA2zero(EVEVEHNR)) %>%
  ## Remove variables that are no longer needed (simplifies next join)
  dplyr::select(-COUNTKEY,-COUNT,-MORNVEHR,-EVEVEHR,-MORNVEHNR,-EVEVEHNR,
                -SITEDESC,-COMMENTS) %>%

  # Now, expand observed to estimated pressure count using fishable days
  ## Number of days and average number of vehicles at each SITE, MONTH, DAYTYPE
  dplyr::group_by(MONTH,DAYTYPE,SITE) %>%
  dplyr::summarize(cntDays=dplyr::n(),
                   cntdVeh=sum(ALLVEH,na.rm=TRUE)) %>%
  ## Append (from fdays) number of fishable days for each SITE, MONTH, DAYTYPE
  dplyr::right_join(fdays,by=c("MONTH","DAYTYPE","SITE")) %>%
  ## Expand to total number of vehicles for each SITE, MONTH, DAYTYPE
  dplyr::mutate(ttlVeh=(cntdVeh/cntDays)*ttlDays) %>%
  ## Rearrange and isolate specific variables
  dplyr::arrange(MONTH,DAYTYPE,SITE) %>%
  dplyr::select(SURVEY,ROUTE,MONTH,DAYTYPE,SITE,
                cntDays,cntdVeh,ttlDays,ttlVeh) %>%
  ## Remove grouping structure
  as.data.frame()



## Interviewed Effort ----
ints_ORIG <- 
  # Read original interview data
  readxl::read_excel(file.path(RDIR,"qry_ice_interviews_4R.xlsx")) %>%
  ## Filter to the route in LOC
  dplyr::filter(ROUTE==LOC) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Factor MONTH and DAYTYPE, and create combined site name
  ## Capitalize all SPP (for consistency) and compute HOURS of effort
  dplyr::mutate(MONTH=iHndlMonth(MONTH),
                DAYTYPE=iHndlDaytype(DAYTYPE),
                SITE=iHndlSiteDesc(SITE,SITEDESC),
                SPP=FSA::capFirst(SPP),
                HOURS=((STOPHH+STOPMM/60)-(STARTHH+STARTMM/60))) %>%
  ## Remove unneeded variables
  dplyr::select(-INTERVIEWKEY,-SITEDESC,-(STARTHH:STOPMM),-RES)


# Create a data.frame that has just the unique interview data
ints_NOFISH <-
  ints_ORIG %>%
  # remove variables related to the different species of fish
  dplyr::select(-SPP,-NUM) %>%
  # remove the rows that were duplcated for the different species
  dplyr::distinct()


# RESULT: data.frame (pcSite at end) of pressure count data expanded to represent
#          the number of vehicles at each SITE in each FISHERY by MONTH and DAYTYPE.
#   * NINTS: Number of observed interviews at SITE in FISHERY
#   * TTLINTS: Number of observed interview at SITE
#   * pIntsInFishery: Proportion of total interviews at SITE in each FISHERY
#   * ttlVehSite: Total vehicles at the SITE
#   * ttlVehSiteFshry: Total vehicles at the SITE in each FISHERY
# NOTES:
#   * This is the bulk of 2019 ASHLAND ICE CREEL EXPANDED PRESSURE.XLSX.
# USE:
#   * Summarized to pcSum below. This is intermediate calculation.
#   * Table 2 (used to compare to "*EPANDED PRESSURE.XLSX" file)
# EXPORTED: not exported

  ## Number of interviews at each SITE within MONTH, DAYTYPE, FISHERY
  tmp1 <- ints_NOFISH %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,DAYTYPE,SITE,FISHERY) %>%
    dplyr::summarize(NINTS=dplyr::n())
  ## Number of interviews at each SITE within MONTH, DAYTYPE
  tmp2 <- tmp1 %>%
    dplyr::summarize(TTLINTS=sum(NINTS))  
  ## Combine, compute proportion of interviews at a SITE within each FISHERY
  ## by MONTH, DAYTYPE
  obsPropIntsInFshry <- dplyr::left_join(as.data.frame(tmp1),
                                         as.data.frame(tmp2),
                                         by=c("SURVEY","ROUTE","UNIT",
                                              "MONTH","DAYTYPE","SITE")) %>%
    dplyr::mutate(pIntsInFishery=NINTS/TTLINTS)
  
  # Find number of vehicles at each SITE within each FISHERY by MONTH, DAYTYPE
  obsNumVehInFshry <- dplyr::full_join(obsPropIntsInFshry,pressureCount,
                                       by=c("SURVEY","ROUTE","MONTH",
                                            "DAYTYPE","SITE")) %>%
    dplyr::rename(ttlVehSite=ttlVeh) %>%
    dplyr::select(-cntDays,-ttlDays,-cntdVeh)
  
  # Separate into those ...
  # ... without observed interviews
  tmp <- filter(obsNumVehInFshry,is.na(pIntsInFishery)) %>%
    select(-FISHERY,-pIntsInFishery)
  # ... with observed interviews
  obsNumVehInFshry <- filter(obsNumVehInFshry,!is.na(pIntsInFishery))
  # Join the pints from pints file onto those without observed interviews
  tmp <- tmp %>%
    mutate(UNIT=unique(obsNumVehInFshry$UNIT)) %>%
    left_join(pints,by=c("SURVEY","ROUTE","MONTH","DAYTYPE","SITE")) %>%
    filter(pIntsInFishery>0) %>%
    select(names(obsNumVehInFshry))

pcSite <- 
  # Combine those that had interviews with the new without interviews
  rbind(obsNumVehInFshry,tmp) %>%
  # rearrange variables
  arrange(SURVEY,ROUTE,MONTH,SITE,FISHERY,DAYTYPE) %>%
  # Compute total vehicles at each SITE in each FISHERY
  dplyr::mutate(ttlVehSiteFshry=ttlVehSite*pIntsInFishery)



# RESULT: data.frame pressure count data expanded to represent the number of
#         vehicles in each FISHERY by MONTH and DAYTYPE (across SITEs).
#   * NINTS: Number of observed interviews in each FISHERY
#   * ttlVehFshry: Total vehicles in each FISHERY
# NOTES:
#   * This is the summaries in 2019 ASHLAND ICE CREEL EXPANDED PRESSURE.XLSX 
#     that are carried forward to 2019 ASHLAND ICE CREEL HARVEST.XLSX.
# USE:
#   * In calculation of harvest below.
# EXPORTED: not exported
pcSum <- 
  pcSite %>%
  dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
  dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                   ttlVehFshry=sum(ttlVehSiteFshry,na.rm=TRUE)) %>%
  as.data.frame()



## Harvest Overall ----
# RESULT: data.frame (expHarvthat ultimately contains expected harvest of each
#         SPP (including "ALL FISH") by MONTH, FISHERY, and DAYTYPE. However,
#         this does not have any summaries across MONTH, FISHERY, or DAYTYPE
#         (see below for those summaries).
#   * obsHarvest: Harvest (number of fish) observed in interviews
#   * obsAnglers: Number of anglers observed in interviews
#   * obsHours: Hours (per party) of fishing observed in interviews
#   * obsAnglerHours: Hours (per angler) of fishng oserved in interviews ...
#                     this is obsAnglers*obsHours.
#   * obsSuccAnglers: Number of successful (caught fish) anglers in interviews ...
#                     only computed for "All fish"
#   * obsHrate: Harvest per angler-hour in interviews ... this is
#               obsHarvest/obsAnglerHours
#   * obsHrsperfish: Hours required to harvest a fish in interviews ... this is
#                    1/obsHrate ...this is used nowhere else but was in Excel
#                    file (SHOULD IT BE DELETED??)
#   * obsAvgTrip: Average trip length per party in interviews ... this is used
#                 nowhere elsebut was in Excel file (SHOULD IT BE DELETED??)
#   * obsAvgAnglerHours: Average hours fished per angler in interviews
#   * obsAnglersPerInt: Average anglers per interview
#   * obsPercSucc: Proportion of successful anglers in interviews ...
#                  only computed for "All fish"
#   * ttlVehFshry: Total (expanded) vehicles in the fishery
#   * ttlAnglers: Total (expanded) anglers
#   * ttlAnglerHours: Total (expanded) angler-hours
#   * ttlSuccAnglers: Total (expanded) successful (caught fish) anglers ...
#                     only computed for "All fish"
#   * ttlHarvest: Total (expanded) number of harvested fish.
# NOTES:
#   * 
# USE:
#   * In summaries of harvest below.
# EXPORTED: XXX

  # Summarized observations about angling effort and success by MONTH, FISHERY,
  # and DAYTYPE and also across DAYTYPES.
  f <- 
    ints_NOFISH %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(nInts=dplyr::n(),
                     obsAnglers=sum(PERSONS),
                     obsHours=sum(HOURS),
                     obsAnglerHours=sum(PERSONS*HOURS),
                     obsSuccAnglers=sum(SUCCESS)) %>%
    as.data.frame()
  
  # Summarized observed harvest of each SPP (including "All Fish") by MONTH,
  # FISHERY, and DAYTYPE and also across DAYTYPES.
  h <- 
    ints_ORIG %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(obsHarvest=sum(NUM)) %>%
    as.data.frame()
  
# Combine observed harvest and individual effort summaries
join_bys <- c("SURVEY","ROUTE","UNIT","MONTH","FISHERY","DAYTYPE")
expHarv <-  dplyr::full_join(h,f,by=join_bys) %>%
  dplyr::mutate(obsSuccAnglers=ifelse(SPP=="All Fish",obsSuccAnglers,NA),
                obsHrate=obsHarvest/obsAnglerHours,
                obsHrsperfish=ifelse(obsHrate>0,1/obsHrate,NA),
                obsAvgTrip=obsHours/nInts,
                obsAvgAnglerHours=obsAnglerHours/obsAnglers,
                obsAnglersPerInt=obsAnglers/nInts,
                obsPercSucc=obsSuccAnglers/obsAnglers) %>%
  dplyr::select(-nInts) %>%
  as.data.frame() %>%
  ## Join on effort (total vehicles)
  dplyr::full_join(pcSum,by=join_bys) %>%
  dplyr::select(-NINTS) %>%
  ## Expand observed results to total results
  dplyr::mutate(ttlAnglers=ttlVehFshry*obsAnglersPerInt,
                ttlAnglerHours=ttlAnglers*obsAvgAnglerHours,
                ttlSuccAnglers=ttlAnglers*obsPercSucc,
                ttlHarvest=ttlAnglerHours*obsHrate) %>%
  dplyr::arrange(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE) %>%
  as.data.frame()
  


# RESULT: data.frame that ultimately contains expected harvest of only "ALL
#         FISH") by MONTH, FISHERY, and DAYTYPE and including summaries across
#         MONTHs, FISHERYs, or DAYTYPEs.
#   * ttlVehFshry: Total (expanded) vehicles in the fishery
#   * anglersPerInt: Average anglers per interview
#   * ttlAnglers: Total (expanded) anglers
#   * avgAnglerHours: Average hours fished per angler
#   * ttlAnglerHours: Total (expanded) angler-hours
#   * percSucc: Percent successful anglers
#   * ttlSuccAnglers: Total (expanded) successful (caught fish) anglers
#   * harvestRate: Harvest per angler-hour
#   * ttlHarvest: Total (expanded) number of harvested fish.
# NOTES:
#   * The values for all variables where there is not an "All" for MONTH,
#     FISHERY, or DAYTYPE are exactly from expHarv from above. The values for
#     ttlVehFhsry, ttlAnglers, ttlAnglerHours, ttlSuccAnglers, and ttlHarvest
#     when MONTH, FISHERY, or DAYTYPE is "All" are simple sums of these
#     variables in expHarv across MONTH, FISHERY, DAYTYPE, or combinations of
#     these strata. Once these sums were made across the strata, then
#     anglersPerInt, agAnglerHours, percSucc, and harvestRate were computed.
#     Thus, anglersPerInt, agAnglerHours, percSucc, and harvestRate were from
#     the observed interview data when MONTH, FISHERY, and DAYTYPE are not
#     "All" but are from the expanded and summarized values when these strata
#     are "All".
#   * These are for the "All fish" records only. See next for individual species.   
#   * This is the summaries in columns A-K of '*ICE CREEL HARVEST' Excel file
# USE:
#   * In Table 3.
# EXPORTED: XXX

  # Isolate all fish and the total variables
  eh <- expHarv %>%
    filter(SPP=="All Fish") %>%
    select(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE,
           ttlVehFshry,ttlAnglers,ttlAnglerHours,ttlSuccAnglers,ttlHarvest) %>%
    mutate(MONTH=as.character(MONTH))

  # Find levels of MONTHs, FISHERYs, and DAYTYPEs
  mlvls <- unique(eh$MONTH)
  flvls <- unique(eh$FISHERY)
  dlvls <- unique(eh$DAYTYPE)
  
  ## Find all possible "All"s (for FISHERY, MONTH, and DAYTYPE)
  # Sum total variables across FISHERYs
  tmp1 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,DAYTYPE,MONTH) %>%
    iSumHarv() %>%
    mutate(FISHERY="All") %>%
    select(names(eh))
  # Sum total variables across MONTHs
  tmp2 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,FISHERY,DAYTYPE) %>%
    iSumHarv() %>%
    mutate(MONTH="All") %>%
    select(names(eh))
  # Sum total variables across DAYTYPEs
  tmp3 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY) %>%
    iSumHarv() %>%
    mutate(DAYTYPE="All") %>%
    select(names(eh))
  # ... and across FISHERYs and MONTHs
  tmp4 <- tmp1 %>%
    iSumHarv() %>%
    mutate(FISHERY="All",MONTH="All") %>%
    select(names(eh))
  # Sum total variables across FISHERYs and DAYTYPEs
  tmp5 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,MONTH) %>%
    iSumHarv() %>%
    mutate(FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  # Sum total variables across MONTHs and DAYTYPEs
  tmp6 <- tmp2 %>%
    iSumHarv() %>%
    mutate(MONTH="All",DAYTYPE="All") %>%
    select(names(eh))
  # ... and across MONTHs, DAYTYPES, and FISHERYs
  tmp7 <- tmp6 %>%
    iSumHarv() %>%
    mutate(MONTH="All",FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))

# Combine, calculate the "rates" from the totals, rearrange, and return
sumExpHarvAll <- rbind(eh,as.data.frame(tmp1),as.data.frame(tmp2),
      as.data.frame(tmp3),as.data.frame(tmp4),as.data.frame(tmp5),
      as.data.frame(tmp6),as.data.frame(tmp7)) %>%
  mutate(MONTH=factor(MONTH,levels=c(mlvls,"All")),
         FISHERY=factor(FISHERY,levels=c(flvls,"All")),
         DAYTYPE=factor(DAYTYPE,levels=c(dlvls,"All")),
         anglersPerInt=ttlAnglers/ttlVehFshry,
         avgAnglerHours=ttlAnglerHours/ttlAnglers,
         percSucc=ttlSuccAnglers/ttlAnglers*100,
         harvestRate=ttlHarvest/ttlAnglerHours) %>%
  arrange(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE,SPP) %>%
  select(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE,SPP,
         ttlVehFshry,anglersPerInt,ttlAnglers,avgAnglerHours,
         ttlAnglerHours,percSucc,ttlSuccAnglers,harvestRate,ttlHarvest)


# RESULT: data.frame that ultimately contains expected harvest of each species
#         by MONTH, FISHERY, and DAYTYPE and including summaries across
#         MONTHs, FISHERYs, or DAYTYPEs.
#   * harvestRate: Harvest per angler-hour
#   * ttlHarvest: Total (expanded) number of harvested fish.
# NOTES:
#   * The values for all variables where there is not an "All" for MONTH,
#     FISHERY, or DAYTYPE are exactly from expHarv from above. The values for
#     ttlHarvest when MONTH, FISHERY, or DAYTYPE is "All" is a simple sum from
#     expHarv across MONTH, FISHERY, DAYTYPE, or combinations of these strata.
#     Additionally, ttlAnglerHours was summed across the strata. Once these sums
#     were made across the strata, then harvestRate were computed.
#   * These are for each species; see above for summaries for all fish combined.
#     Note that there are fewer summaries here as some items could not be
#     computed by fish species (e.g., percent of successful anglers) and these
#     are the only summaries in the Excel files. 
#   * This is the summaries in columns L-AD of '*ICE CREEL HARVEST' Excel file
# USE:
#   * In Table 4.
# EXPORTED: XXX

  # Isolate just the ttlAnglerHours in summary of overall harvest
  sehALL <- select(sumExpHarvAll,SURVEY:DAYTYPE,ttlAnglerHours)

  # Isolate individual fish species and the total variables in expHarv
  eh <- expHarv %>%
    filter(SPP!="All Fish") %>%
    select(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE,ttlHarvest) %>%
    mutate(MONTH=as.character(MONTH))

  # Find levels of MONTHs, FISHERYs, and DAYTYPEs
  mlvls <- unique(eh$MONTH)
  flvls <- unique(eh$FISHERY)

  ## Find all possible "All"s (for FISHERY, MONTH, and DAYTYPE)
  # Sum total variables across FISHERYs
  tmp1 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,DAYTYPE,MONTH) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(FISHERY="All") %>%
    select(names(eh))
  # Sum total variables across MONTHs
  tmp2 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,FISHERY,DAYTYPE) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(MONTH="All") %>%
    select(names(eh))
  # Sum total variables across DAYTYPEs
  tmp3 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(DAYTYPE="All") %>%
    select(names(eh))
  # ... and across FISHERYs and MONTHs
  tmp4 <- tmp1 %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(FISHERY="All",MONTH="All") %>%
    select(names(eh))
  # Sum total variables across FISHERYs and DAYTYPEs
  tmp5 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,MONTH) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  # Sum total variables across MONTHs and DAYTYPEs
  tmp6 <- tmp2 %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(MONTH="All",DAYTYPE="All") %>%
    select(names(eh))
  # ... and across MONTHs, DAYTYPES, and FISHERYs
  tmp7 <- tmp6 %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(MONTH="All",FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  
# Combine, calculate the "rates" from the totals, rearrange, and return
sumExpHarvSPP <- rbind(eh,as.data.frame(tmp1),as.data.frame(tmp2),
      as.data.frame(tmp3),as.data.frame(tmp4),as.data.frame(tmp5),
      as.data.frame(tmp6),as.data.frame(tmp7)) %>%
  dplyr::full_join(sehALL,by=c("SURVEY","ROUTE","UNIT","MONTH",
                               "FISHERY","DAYTYPE")) %>%
  dplyr::mutate(MONTH=factor(MONTH,levels=c(mlvls,"All")),
                FISHERY=factor(FISHERY,levels=c(flvls,"All")),
                DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All")),
                harvestRate=ttlHarvest/ttlAnglerHours) %>%
  arrange(SURVEY,ROUTE,UNIT,SPP,FISHERY,MONTH,DAYTYPE) %>%
  select(SURVEY,ROUTE,UNIT,SPP,FISHERY,MONTH,DAYTYPE,harvestRate,ttlHarvest)


## Fish Lengths ----

## This is same as 2019ASHLANDICECATCH.XLSX
fish <- 
  readxl::read_excel(file.path(RDIR,"qry_ice_fish_4R.xlsx")) %>%
  dplyr::filter(ROUTE==LOC) %>%
  dplyr::rename_all(.funs=toupper) %>%
  dplyr::mutate(SPP=FSA::capFirst(SPP),
                ORIGIN=ifelse(is.na(CLIP)|CLIP=="Native","Native","Hatchery")) %>%
  dplyr::left_join(dplyr::select(ints_NOFISH,INTERVIEW,UNIT,MONTH,FISHERY),
                   by="INTERVIEW")  %>%
  dplyr::select(SURVEY,ROUTE,UNIT,SPP,FISHERY,MONTH,LENGTH,CLIP,ORIGIN)


## Summarize Lengths
  tmp1 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,ORIGIN) %>%
    iSumLengths() %>%
    arrange(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,ORIGIN) %>%
    as.data.frame()
  
  tmp2 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY) %>%
    iSumLengths() %>%
    dplyr::mutate(ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp3 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,ORIGIN) %>%
    iSumLengths() %>%
    dplyr::mutate(FISHERY="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp4 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH) %>%
    iSumLengths() %>%
    dplyr::mutate(FISHERY="All",ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp5 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,FISHERY,ORIGIN) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp6 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,FISHERY) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All",ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp7 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,ORIGIN) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All",FISHERY="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp8 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All",FISHERY="All",ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()

flvls <- unique(fish$FISHERY)
sumLen <- lenSum <- rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8) %>%
  as.data.frame() %>%
  mutate(ORIGIN=factor(ORIGIN,levels=c("Native","Hatchery","All")),
         FISHERY=factor(FISHERY,levels=c(flvls,"All"))) %>%
  dplyr::arrange(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,ORIGIN)

