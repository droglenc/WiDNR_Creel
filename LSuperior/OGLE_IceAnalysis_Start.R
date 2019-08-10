WDIR <- file.path(here::here(),"LSuperior")
RDIR <- file.path(WDIR,"LS_Analysis_2019")
ROUTE2USE <- "Ashland"

source(file.path(WDIR,"Helpers","LS_ICE_Analysis_Helpers.R"))

## Pressure counts ----
# RESULT: data.frame of fishable days at each SITE by MONTH and DAYTYPE.
#   * ttlDays: Number of fishable (i.e., "good ice") days
# NOTES:
#   * Basically just a read of the "*_fdays" file
# USE: 
#   * To expand the observed pressure counts to all fishable days at site.
#   * Table 1.
# EXPORTED: Not exported.
fdays <- readFishableDays(RDIR,ROUTE2USE)

table1(fdays)

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
# EXPORTED: 
pressureCount <- readPressureCountData_ICE(RDIR,ROUTE2USE,fdays)

table2(pressureCount)

## Interviewed Effort ----
# Read interview data
ints_ORIG <-
  readxl::read_excel(file.path(RDIR,"qry_ice_interviews_4R.xlsx")) %>%
  dplyr::filter(ROUTE==ROUTE2USE) %>%
  dplyr::rename_all(.funs=toupper) %>%
  dplyr::mutate(MONTH=iHndlMonth(MONTH),
                SITE=iHndlSiteDesc(SITE,SITEDESC),
                SPP=FSA::capFirst(SPP),
                HOURS=((STOPHH+STOPMM/60)-(STARTHH+STARTMM/60))) %>%
  dplyr::select(-INTERVIEWKEY,-SITEDESC,-(STARTHH:STOPMM),-RES)

## Get variables not related to fish and remove rows duplicated for diff species
ints_NOFISH <- 
  ints_ORIG %>%
  dplyr::select(-SPP,-NUM) %>%
  dplyr::distinct()

## Move the total harvested fish to "ALL FISH" in the SPP variable
ints_ORIG_MOD <- iHndlHarvestAllFish(ints_ORIG,ints_NOFISH)

## This is the bulk of what is in 2019 ASHLAND ICE CREEL EXPANDED PRESSURE.XLSX
pcSite <- 
  ints_NOFISH %>%
  # Get proportion of interviews at each SITE (within each MONTH and DAYTYPE)
  # that are within each interviewed FISHERY
  iFindPropIntsInFishery() %>%
  # Find number of vehicles at each SITE within each FISHERY by MONTH, DAYTYPE
  iFindTtlVehiclesInFishery(pressureCount) %>%
  ## OPEN QUESTION -- how were the vehicles at a SITE allocated into FISHERYs
  ##   when those FISHERYs did not appear in an interview ... this is apparently
  ##   handled on an ad hoc basis (accoring to Zunk) ... I have starte to address
  ##   this with the hndlNoIntsButPressure() function
  iHndlNoIntsButPressure()

## Total interviews and pressure across sites by MONTH, DAYTYPE, FISHERY
## This is part of 2019 ASHLAND ICE CREEL EXPANDED PRESSURE.XLSX that gets
## carried forward to 2019 ASHLAND ICE CREEL HARVEST.XLSX
pcSum <- iSumPressureCount(pcSite)


## Harvest Overall ----
obsEff1 <- 
  ints_NOFISH %>%
  dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
  dplyr::summarize(nInts=dplyr::n(),
                   obsAnglers=sum(PERSONS),
                   obsHours=sum(HOURS),
                   obsAnglerHours=sum(PERSONS*HOURS),
                   obsSuccAnglers=sum(SUCCESS))

obsEff2 <-
  obsEff1 %>%
  dplyr::summarize(nInts=sum(nInts),
                   obsAnglers=sum(obsAnglers),
                   obsSuccAnglers=sum(obsSuccAnglers),
                   obsHours=sum(obsHours),
                   obsAnglerHours=sum(obsAnglerHours)) %>%
  dplyr::mutate(DAYTYPE="ALL") %>%
  select(names(obsEff1))

obsEff <- rbind(obsEff1,obsEff2)
  
obsHarv1 <-
  ints_ORIG_MOD %>%
  dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,SPP,DAYTYPE) %>%
  dplyr::summarize(obsHarvest=sum(NUM))

obsHarv2 <-
  obsHarv1 %>%
  dplyr::summarize(obsHarvest=sum(obsHarvest)) %>%
  dplyr::mutate(DAYTYPE="All") %>%
  dplyr::select(names(obsHarv1))

obsHarv <- rbind(obsHarv1,obsHarv2)

hSum <-
  dplyr::full_join(obsHarv,obsEff,
                   by=c("SURVEY","ROUTE","UNIT","MONTH","FISHERY","DAYTYPE")) %>%
  dplyr::mutate(obsSuccAnglers=ifelse(SPP=="ALL FISH",obsSuccAnglers,NA),
                obsHrate=obsHarvest/obsAnglerHours,
                obsHrsperfish=ifelse(obsHrate>0,1/obsHrate,NA),
                obsAvgTrip=obsHours/nInts,
                obsAvgAnglerHours=obsAnglerHours/obsAnglers,
                obsAnglersPerInt=obsAnglers/nInts,
                obsPercSucc=obsSuccAnglers/obsAnglers) %>%
  dplyr::select(-nInts) %>%
  as.data.frame() %>%
  ## Join on effort (total vehicles)
  dplyr::full_join(pcSum,by=c("SURVEY","ROUTE","UNIT","MONTH","FISHERY","DAYTYPE")) %>%
  dplyr::select(-NINTS) %>%
  dplyr::mutate(ttlAnglers=ttlVehFshry*obsAnglersPerInt,
                ttlAnglerHours=ttlAnglers*obsAvgAnglerHours,
                ttlSuccAnglers=ttlAnglers*obsPercSucc,
                ttlHarvest=ttlAnglerHours*obsHrate)
  



## This is same as 2019ASHLANDICECATCH.XLSX
fish <- readxl::read_excel(file.path(RDIR,"qry_ice_fish_4R.xlsx")) %>%
  dplyr::filter(ROUTE==ROUTE2USE) %>%
  dplyr::rename_all(.funs=toupper) %>%
  dplyr::mutate(SPP=FSA::capFirst(SPP)) %>%
  dplyr::select(-FISHKEY,-COMMENTS)
fish

