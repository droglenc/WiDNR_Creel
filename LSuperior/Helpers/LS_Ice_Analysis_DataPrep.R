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

# Read original interview data
ints_ORIG <- readInterviewData_ICE(RDIR,LOC)

# Get variables not related to fish and remove rows duplicated for diff species
# This is just unique interview data
ints_NOFISH <- ints_ORIG %>%
  dplyr::select(-SPP,-NUM) %>%
  dplyr::distinct()

# Move the total harvested fish to "ALL FISH" in the SPP variable
# This allows for easier summarization below
ints_ORIG <- iHndlHarvestAllFish(ints_ORIG,ints_NOFISH)

pcSite <- ints_NOFISH %>%
  # Get proportion of interviews at each SITE (within each MONTH and DAYTYPE)
  # that are within each interviewed FISHERY
  iFindPropIntsInFishery() %>%
  # Find number of vehicles at each SITE within each FISHERY by MONTH, DAYTYPE
  iFindTtlVehiclesInFishery(pressureCount) %>%
  ## OPEN QUESTION -- how were the vehicles at a SITE allocated into FISHERYs
  ##   when those FISHERYs did not appear in an interview ... this is apparently
  ##   handled on an ad hoc basis (accoring to Zunk) ... I have started to address
  ##   this with the hndlNoIntsButPressure() function
  iHndlNoIntsButPressure()

# RESULT: data.frame (pcSite) of pressure count data expanded to represent the
#         number of vehicles in each FISHERY by MONTH and DAYTYPE (across SITEs).
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
# Summarized observations about angling effort and success by MONTH, FISHERY,
# and DAYTYPE and also across DAYTYPES.
sumObsEffSucc <- sumObsEffortSuccess(ints_NOFISH)

# Summarized observed harvest of each SPP (including "ALL FISH") by MONTH,
# FISHERY, and DAYTYPE and also across DAYTYPES.
sumObsHarv <- sumObsHarvest(ints_ORIG)



  
hSum <- expandHarvest(sumObsHarv,sumObsEffSucc,pcSum)






## This is same as 2019ASHLANDICECATCH.XLSX
fish <- readxl::read_excel(file.path(RDIR,"qry_ice_fish_4R.xlsx")) %>%
  dplyr::filter(ROUTE==LOC) %>%
  dplyr::rename_all(.funs=toupper) %>%
  dplyr::mutate(SPP=FSA::capFirst(SPP)) %>%
  dplyr::select(-FISHKEY,-COMMENTS)
fish

