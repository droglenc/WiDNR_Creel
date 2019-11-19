#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE (unless you know what you are doing)!!!
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Setup -----------------------------------------------------------------------
### Load helpers file
source(file.path(WDIR,"Helpers","LS_Ice_Helpers.R"))

### Filename prefix
fnpre <- paste0(RDIR,"/",LOC2,"_Ice_",YR,"_")

## Pressure counts -------------------------------------------------------------

# DESCRIPTION: This creates a data.frame that identifies the total number of
#              fishable days (i.e., "good ice") at each SITE (by ROUTE, MONTH,
#              and DAYTYPE) and the proportions of interviews that will be
#              apportioned into the various fisheries (for use when there are
#              no interviews at a site but there was a pressure count at that
#              site). This will be split into two below with just the fishable 
#              days and just the proportion of interviews data.
# RESULT: A data.frame with ...
#   * SURVEY: Year of the ice survey
#   * ROUTE: Route of the ice survey
#   * MONTH: Month of the ice survey
#   * YEAR: Calendar year of the ice survey
#   * DAYTYPE: Type of day (Weekday or Weekend)
#   * SITE: Long site description
#   * TTLDAYS: Total number of fishable days (i.e., "good ice") at that SITE,
#              ROUTE, MONTH, DAYTYPE
#   * < 60 ft - Shallow: Proportion of interviews to allocate to this fishery
#   * > 60 ft - Bobbing: Proportion of interviews to allocate to this fishery
#   * Pleasure: Proportion of interviews to allocate to this fishery
#   * Post LT Open-Water: Proportion of interviews to allocate to this fishery
#   * Tribal: Proportion of interviews to allocate to this fishery
#   * NOP Spearing: Proportion of interviews to allocate to this fishery
fdays <- 
  readxl::read_excel(file.path(RDIR,"data",FDAY_FILE)) %>%
  ## Filter to the route in LOC and current SURVEY year
  dplyr::filter(ROUTE==LOC,SURVEY==YEAR) %>%
  ## Change FDAYS variable to TTLDAYS
  dplyr::rename(TTLDAYS=Fdays) %>%
  ## Change main variable names to upper-case (easier to remember)
  dplyr::rename(MONTH=Month,YEAR=Year) %>%
  ## Convert MONTH number to name and re-level so that DEC is first
  ## Simplify DAYTYPE name and convert to factor
  ## Create combined site name
  dplyr::mutate(MONTH=factor(month.abb[MONTH],levels=month.abb[c(12,1:11)]),
                DAYTYPE=factor(FSA::mapvalues(DAYTYPE,
                               from="Weekend/Holiday",to="Weekend"),
                               levels=c("Weekday","Weekend")),
                SITE=paste0(SITE,"-",FSA::capFirst(SiteDesc))) %>%
  ## Remove variables that are not needed further below
  dplyr::select(-FdaysKey,-SiteDesc,-Comments) %>%
  ## Convert to data.frame (and not tibble)
  as.data.frame()


# DESCRIPTION: This creates a data.frame of the proportion of interviews at a
#              each SITE, by MONTH and DAYTYPE, that would be allocated to the
#              different FISHERYs in the instances where no interviews were
#              recorded at the site but pressure was recorded. This is the fdays
#              data.frame from above rearranged to long format and without the
#              total number of fishable days. Used when dealing with interviews
#              much further below, but needed to create here as fdays data.frame
#              is modified next.
# RESULT: A data.frame with ...
#   * SURVEY, ROUTE, MONTH, YEAR, DAYTYPE, SITE: as described above.
#   * FISHERY: Type of fishery
#   * pIntsInFishery: proportion of interviews at a given SITE that would
#                     be allocated to the different FISHERYs
pints <- 
  select(fdays,-TTLDAYS) %>%
  tidyr::gather(key="FISHERY",value="pIntsInFishery",-(SURVEY:SITE)) %>%
  mutate(FISHERY=factor(FISHERY,levels=lvlsFISHERY))


# DESCRIPTION: This creates a data.frame of fishable days at each SITE by MONTH
#              and DAYTYPE. This is the fdays data.frame from above but without
#              the propotion of interviews in each fishery type variables. This
#              will be used to expand the observed pressure counts to all
#              fishable days at a SITE.
# RESULT: A data.frame with ...
#   * SURVEY, ROUTE, MONTH, YEAR, DAYTYPE, SITE: as described above.
#   * TTLDAYS: Number of fishable (i.e., "good ice") days
fdays <- dplyr::select(fdays,SURVEY:TTLDAYS)



# DESCRIPTION: This creates a data.frame of pressure count data expanded to
#              represent the number of vehicles at each SITE by MONTH and
#              DAYTYPE. This is expanded to represent all fishable days, not
#              just those that were sampled. This is used in Table 1.
# RESULT: A data.frame with ...
#   * SURVEY, ROUTE, MONTH, DAYTYPE, SITE: as defined above.
#   * cntDays: Days that vehicles were counted at that SITE, MONTH, DAYTYPE
#   * cntdVeh: Total number of vehicles counted at that SITE, MONTH, DAYTYPE
#   * TTLDAYS: Total fishable days at that SITE, MONTH, DAYTYPE (from fdays)
#   * ttlVeh: Expanded (by number of fishable days) number of vehicles at that
#             SITE, MONTH, DAYTYPE
# NOTES:
#   * Same (with some issues and slightly different format of variables) as
#     2019 ASHLAND ICE CREEL MONTHLY PRESSURE.XLSX
pressureCount <-
  readxl::read_excel(file.path(RDIR,"data",CNTS_FILE)) %>%
  ## Filter to the route in LOC and survey year
  dplyr::filter(ROUTE==LOC,SURVEY==YEAR) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Convert MONTH number to name and re-level so that DEC is first
  ## Simplify DAYTYPE name and convert to factor
  ## Create combined site name
  dplyr::mutate(MONTH=factor(month.abb[MONTH],levels=month.abb[c(12,1:11)]),
                DAYTYPE=factor(FSA::mapvalues(DAYTYPE,
                                              from="Weekend/Holiday",to="Weekend"),
                               levels=c("Weekday","Weekend")),
                SITE=paste0(SITE,"-",FSA::capFirst(SITEDESC)),
                ## Sum all vehicle counts (treat NAs as 0s so they will sum)
                ALLVEH=NA2zero(MORNVEHR)+NA2zero(EVEVEHR)+
                  NA2zero(MORNVEHNR)+NA2zero(EVEVEHNR)) %>%
  ## Find number of days and avg number of vehicles at each SITE, MONTH, DAYTYPE
  dplyr::group_by(MONTH,DAYTYPE,SITE) %>%
  dplyr::summarize(cntDays=dplyr::n(),
                   cntdVeh=sum(ALLVEH,na.rm=TRUE)) %>%
  
  ## Now, expand observed to estimated pressure count using fishable days
  ## Append (from fdays) number of fishable days for each SITE, MONTH, DAYTYPE
  dplyr::right_join(.,fdays,by=c("MONTH","DAYTYPE","SITE")) %>%
  ## Expand to total number of vehicles for each SITE, MONTH, DAYTYPE
  dplyr::mutate(ttlVeh=(cntdVeh/cntDays)*TTLDAYS) %>%
  ## Rearrange and isolate specific variables
  dplyr::arrange(MONTH,DAYTYPE,SITE) %>%
  dplyr::select(SURVEY,ROUTE,MONTH,DAYTYPE,SITE,
                cntDays,cntdVeh,TTLDAYS,ttlVeh) %>%
  ## Make a data.frame (not a grouped tibble)
  as.data.frame()



## Interviewed Effort ----------------------------------------------------------
# DESCRIPTION: This creates a data.frame of all data recorded for each
#              interview. Note that the starting and ending times have been
#              converted to a number of hours fished.
# RESULT: A data.frame with ...
#   * INTERVIEW: Unique ID for the interview
#   * SURVEY: Year of the ice survey
#   * ROUTE: Route for the interview
#   * SITE: Site description for interview
#   * STATE: State fished
#   * UNIT: Management unit fished
#   * YEAR: Calendar year for the interview
#   * MONTH: Month for the interview
#   * DAY: Day for the interview
#   * DATE: Date for the interview
#   * DAYTYPE: Type of day (Weekday or Weekend)
#   * FISHERY: Type of ice fishery
#   * STATUS: Whether the interview represents a completed trip or not 
#   * PERSONS: Number of persons in the party
#   * SUCCESS: Number of successful anglers in the party
#   * HOURS: Hours fished.
#   * SPECIES: Species harvested (includs an "All fish" category)
#   * NUM: Number of that species harested
intvs_ORIG <- 
  readxl::read_excel(file.path(RDIR,"data",INTS_FILE)) %>%
  ## Filter to the route in LOC and survey year
  dplyr::filter(ROUTE==LOC,SURVEY==YEAR) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Convert MONTH number to name and re-level so that DEC is first
  ## Simplify DAYTYPE name and convert to factor
  ## Create combined site name
  dplyr::mutate(DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%Y"),
                MONTH=factor(month.abb[MONTH],levels=month.abb[c(12,1:11)]),
                DAYTYPE=factor(FSA::mapvalues(DAYTYPE,
                                              from="Weekend/Holiday",to="Weekend"),
                               levels=c("Weekday","Weekend")),
                FISHERY=factor(FISHERY,levels=lvlsFISHERY),
                SITE=paste0(SITE,"-",FSA::capFirst(SITEDESC)),
                ## Capitalize all SPP and call it SPECIES (for consistency)
                SPECIES=FSA::capFirst(SPP),
                ## compute HOURS of effort
                HOURS=((STOPHH+STOPMM/60)-(STARTHH+STARTMM/60))) %>%
  ## Arrange variables (also removes unneeded variables
  dplyr::select(INTERVIEW,SURVEY,ROUTE,SITE,STATE,UNIT,YEAR,MONTH,DAY,DATE,
                DAYTYPE,FISHERY,STATUS,PERSONS,SUCCESS,HOURS,SPECIES,NUM) %>%
  ## Make a data.frame(and not a tibble)
  as.data.frame()


# DESCRIPTION: This creates a data.frame from intvs_ORIG that is just the unique
#              interview data (i.e., no information about the harvested fish).
# RESULT: A data.frame with ...
#   * INTERVIEW, SURVEY, ROUTE, SITE, STATE, UNIT, YEAR, MONTH, DAY, DATE,
#     DAYTYPE, FISHERY, STATUS, PERSONS, SUCCESS, HOURS: as defined above.
intvs_NOFISH <-
  intvs_ORIG %>%
  # remove variables related to the different species of fish
  dplyr::select(-SPECIES,-NUM) %>%
  # remove the rows that were duplcated for the different species
  dplyr::distinct()


# DESCRIPTION: This creates a data.frame of pressure count data expanded to
#              represent the total number of vehicles at each SITE in each
#              FISHERY by MONTH and DAYTYPE. Ultimately this is an intermediate
#              calculation used to find ttlEffort below. This is used in Table 2.
# RESULT: A data.frame with ...
#   * SURVEY, ROUTE, UNIT, MONTH, DAYTYPE, SITE, FISHERY: as defined above.
#   * NINTS: Number of observed interviews at SITE in FISHERY
#   * TTLINTS: Number of observed interview at SITE
#   * pIntsInFishery: Proportion of total interviews at SITE in each FISHERY ...
#                     this includes values calculated from interviews and those
#                     from pints (from above) for sites with pressure but no
#                     interviews.
#   * ttlVehSite: Total vehicles at the SITE
#   * ttlVehSiteFshry: Total vehicles at the SITE in each FISHERY (this used
#                      pIntsInFishery)
# NOTES: This is the bulk of 2019 ASHLAND ICE CREEL EXPANDED PRESSURE.XLSX.
ttlEffortBySite <- expandPressureCount(pressureCount,intvs_NOFISH,pints)


# DESCRIPTION: This creates a data.frame of pressure count data expanded to
#              represent the number of vehicles in each FISHERY by MONTH and
#              DAYTYPE (across all SITEs). This is used in the harvest
#              calculations further below and is expored as "*_ttlEffort.csv".
# RESULT: A data.frame with ...
#   * SURVEY, ROUTE, UNIT, MONTH, DAYTYPE, FISHERY: as defined above.
#   * NINTS: Number of observed interviews in each FISHERY
#   * ttlVehFshry: Total vehicles in each FISHERY
# NOTES:
#   * This is the summaries in 2019 ASHLAND ICE CREEL EXPANDED PRESSURE.XLSX 
#     that are carried forward to 2019 ASHLAND ICE CREEL HARVEST.XLSX.
ttlEffort <- ttlEffortBySite %>%
  dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
  dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                   ttlVehFshry=sum(ttlVehSiteFshry,na.rm=TRUE)) %>%
  as.data.frame()
writeDF(ttlEffort,fnpre)


## Harvest ---------------------------------------------------------------------
# DESCRIPTION: This creates a data.frame that ultimately contains expected
#              harvest and other harvest-related summaries (see below) of each
#              SPECIES (including "All Fish") by MONTH, FISHERY, and DAYTYPE.
#              However, this does not have any summaries across MONTH, FISHERY,
#              or DAYTYPE (see further below for those summaries).
# RESULT: A data.frame with ...
#   * SURVEY, ROUTE, UNIT, SPECIES, MONTH, FISHERY, DAYTYPE: as defined above.
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
#                 nowhere else but was in Excel file (SHOULD IT BE DELETED??)
#   * obsAvgAnglerHours: Average hours fished per angler in interviews
#   * obsAnglersPerInt: Average anglers per interview
#   * obsPercSucc: Proportion of successful anglers in interviews ...
#                  only computed for "All fish"
#   * ttlVehFshry: Total (expanded) vehicles in the fishery
#   * ttlAnglers: Total (expanded) anglers
#   * ttlAnglerHours: Total (expanded) angler-hours
#   * ttlSuccAnglers: Total (expanded) successful (caught fish) anglers ...
#                     only computed for "All fish"
#   * ttlHarvest: Total (expanded) number of harvested fish
expHarv <- expandHarv(intvs_NOFISH,intvs_ORIG,ttlEffort)


# DESCRIPTION: This creates a data.frame that ultimately contains expanded
#              harvest of only "All Fish") by MONTH, FISHERY, and DAYTYPE, and
#              includes summaries across MONTHs, FISHERYs, and DAYTYPEs. See
#              next for summaries of individual species. This is used in Table 3.
# RESULT: A data.frame with ...
#   * SURVEY, ROUTE, UNIT, SPECIES, MONTH, FISHERY, DAYTYPE, SPECIES: as defined above.
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
#   * This is the summaries in columns A-K of '*ICE CREEL HARVEST' Excel file
sumExpHarvAll <- sumExpHarvestAll(expHarv)


# DESCRIPTION: This creates a data.frame that ultimately contains expected
#              harvest of individual species by MONTH, FISHERY, and DAYTYPE, and
#              includes summaries across MONTHs, FISHERYs, and DAYTYPEs. See
#              above for summaries of "All Fish". This is used in Table 4.
# RESULT: A data.frame with ...
#   * SURVEY, ROUTE, UNIT, SPECIES, MONTH, FISHERY, DAYTYPE, SPECIES: as defined above.
#   * harvestRate: Harvest per angler-hour
#   * ttlHarvest: Total (expanded) number of harvested fish.
# NOTES:
#   * The values for all variables where there is not an "All" for MONTH,
#     FISHERY, or DAYTYPE are exactly from expHarv from above. The values for
#     ttlHarvest when MONTH, FISHERY, or DAYTYPE is "All" is a simple sum from
#     expHarv across MONTH, FISHERY, DAYTYPE, or combinations of these strata.
#     Additionally, ttlAnglerHours was summed across the strata. Once these sums
#     were made across the strata, then harvestRate were computed.
#   * There are fewer summaries here as compared to above for "All Fish" as some
#     items could not be computed by fish species (e.g., percent of successful
#     anglers) and these are the only summaries in the Excel files. 
#   * This is the summaries in columns L-AD of '*ICE CREEL HARVEST' Excel file
sumExpHarvSPP <- sumExpHarvestSPP(expHarv,sumExpHarvAll)


# DESCRIPTION: This creates a data.frame that combines sumExpHarvALL and
#              sumExpHarvSPP. All variables are defined above. It is exported
#              as "*_ttlHarvest.csv".
ttlHarvest <- 
  sumExpHarvSPP %>%
  mutate(ttlVehFshry=NA_real_,anglersPerInt=NA_real_,ttlAnglers=NA_real_,
         avgAnglerHours=NA_real_,ttlAnglerHours=NA_real_,percSucc=NA_real_,
         ttlSuccAnglers=NA_real_) %>%
  select(names(sumExpHarvAll)) %>%
  bind_rows(sumExpHarvAll) %>%
  mutate(SPECIES=factor(SPECIES,levels=lvlsSPECIES)) %>%
  arrange(SURVEY,ROUTE,UNIT,SPECIES,FISHERY,MONTH,DAYTYPE)
writeDF(ttlEffort,fnpre)


## Fish Lengths ----

# DESCRIPTION: This creates a data.frame that contains length and clip
#              information for all measured fish. The lengths are summarized
#              further below. This is used in Table 5 and is exported as
#              "*_lengths.csv". 
# RESULT: A data.frame with ...
#   * SURVEY, ROUTE, UNIT, FISHERY, MONTH, DATE, SITE, SPECIES: as defined above.
#   * LENGTH: Total length in inches.
#   * CLIP: Fin clip type
#   * ORIGIN: Whether "Native" or from a "Hatchery"
lengths <- 
  readxl::read_excel(file.path(RDIR,"data",FISH_FILE)) %>%
  ## Filter to the route in LOC and current SURVEY year
  dplyr::filter(ROUTE==LOC,SURVEY==YEAR) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Change SPP to SPECIES and standardize capitalization
  ## Create an ORIGIN ariable that is "Native" if CLIP is missing or CLIP says
  ## "Native", other is "Hatchery"
  dplyr::mutate(SPECIES=FSA::capFirst(SPP),
                ORIGIN=ifelse(is.na(CLIP)|CLIP=="Native","Native","Hatchery")) %>%
  ## Join on the interview specific information
  dplyr::left_join(dplyr::select(intvs_NOFISH,-SURVEY,-ROUTE,-STATUS,
                                 -PERSONS,-SUCCESS,-HOURS),
                   by="INTERVIEW")  %>%
  ## Isolate and rearrange the variables
  dplyr::select(SURVEY,ROUTE,UNIT,FISHERY,MONTH,DATE,SITE,SPECIES,CLIP,ORIGIN,LENGTH) %>%
  ## Sort rows
  dplyr::arrange(SURVEY,ROUTE,UNIT,SPECIES,FISHERY,MONTH,LENGTH) %>%
  ## Make a data.frame (and not a tibble)
  as.data.frame()
writeDF(lengths,fnpre)


# DESCRIPTION: This creates a data.frame that summarizes (see below) total
#              length by MONTH, FISHERY, SPECIES, and ORIGIN.
# RESULT: A data.frame with ...
#   * SURVEY, ROUTE, UNIT, FISHERY, MONTH, SPECIES, ORIGIN: as defined above.
#   * n: Number of fish
#   * mnLen: Mean length
#   * sdLen: SD length
#   * minLen: Minimum length
#   * maxLen: Maximum length
sumLen <- sumLengths(lengths)

