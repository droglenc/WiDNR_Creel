source(file.path(WDIR,"Helpers","LS_ICE_Analysis_Helpers.R"))

## Pressure counts ----
# RESULT: data.frame of fishable days at each SITE by MONTH and DAYTYPE.
#   * ttlDays: Number of fishable (i.e., "good ice") days
# NOTES:
#   * Basically just a read of the "*_fdays" file
# USE: 
#   * To expand the observed pressure counts to all fishable days at site.
# EXPORTED: Not exported.
fdays <- readFishableDays(RDIR,LOC)

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
pressureCount <- readPressureCountData_ICE(RDIR,LOC,fdays)


## Interviewed Effort ----
# RESULT: data.frame (pcSite) of pressure count data expanded to represent the
#         number of vehicles at each SITE in each FISHERY by MONTH and DAYTYPE.
#   * NINTS: Number of observed interviews at SITE in FISHERY
#   * TTLINTS: Number of observed interview at SITE
#   * pIntsInFishery: Proportion of total interviews at SITE in each FISHERY
#   * ttlVehSite: Total vehicles at the SITE
#   * ttlVehSiteFshry: Total vehicles at the SITE in each FISHERY
# NOTES:
#   * This is the bulk of 2019 ASHLAND ICE CREEL EXPANDED PRESSURE.XLSX.
# USE:
#   * Summarized to pcSum below. This is intermediate calculations.
#   * Table 2 (used to compare to "*EPANDED PRESSURE.XLSX" file)
# EXPORTED: not exported

# Read original interview data and also create a data.frame that has just the
# unique interview data (i.e., remove variables related to the different species
# of fish and then remove the rows that were duplcated for the different species)
ints_ORIG <- readInterviewData_ICE(RDIR,LOC)
ints_NOFISH <- ints_ORIG %>%
  dplyr::select(-SPP,-NUM) %>%
  dplyr::distinct()

pcSite <- ints_NOFISH %>%
  # Get proportion of interviews at each SITE (within each MONTH and DAYTYPE)
  # that are within each interviewed FISHERY
  iFindPropIntsInFishery() %>%
  # Find number of vehicles at each SITE within each FISHERY by MONTH, DAYTYPE
  iFindTtlVehiclesInFishery(pressureCount) %>%
  ## OPEN QUESTION -- how were the vehicles at a SITE allocated into FISHERYs
  ##   when those FISHERYs did not appear in an interview ... this is apparently
  ##   handled on an ad hoc basis (accoring to Zunk) ... I have started to address
  ##   this with the iHndlNoIntsButPressure() function
  iHndlNoIntsButPressure()


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
pcSum <- sumPressureCount(pcSite)


## Harvest Overall ----
# RESULT: data.frame that ultimately contains expected harvest of each SPP
#         (including "ALL FISH") by MONTH, FISHERY, and DAYTYPE. However, this
#         does not have any summaries across MONTH, FISHERY, or DAYTYPE (see
#         below for those summaries).
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
expHarv <- expandHarvest(ints_ORIG,ints_NOFISH,pcSum)


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
sumExpHarvAll <- sumExpHarvest_AllFish(expHarv)


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
sumExpHarvSPP <- sumExpHarvest_eachSpp(expHarv,sumExpHarvAll)


## Fish Lengths ----

## This is same as 2019ASHLANDICECATCH.XLSX
fish <- readFish_ICE(RDIR,LOC,ints_NOFISH)

sumLen <- sumLengths_ICE(fish)


