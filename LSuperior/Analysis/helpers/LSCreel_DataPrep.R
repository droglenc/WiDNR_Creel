#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
#
#  DIRECTIONS:
#   * Don't run this file ... use LSCreel_TableMaker.R
#   * This, along with LSCreel_helpers, contains the "guts" of the program
#
#  NOTES:
#   * Counts (for Lake Superior) are average number of parties present during
#     the wait time, not total effort seen during the wait time.
#   * Only official holidays are New Years, Memorial Day, July Fourth, and Labor
#     Day (Thanksgiving and Christmas are not included).
#   * FINCLIP=99 means length field has number of fish harvested.
#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
# DO NOT CHANGE ANYTHING BENEATH HERE
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Setup ----
### Converts SDATE and FDATE to useful objects
SDATE <- as.Date(SDATE,"%m/%d/%Y")
FDATE <- as.Date(FDATE,"%m/%d/%Y")

### Prepare working and results directory
suppressWarnings(dir.create(RDIR))

### Load helper files
source(paste0(WDIR,"helpers/LSCreel_helpers.R"))

### Filename prefix
fnpre <- fnPrefix(RDIR,LOC,SDATE)

## Create expansion factors ----
### Make data.frame of dates from starting to ending date (entered above)
###    include the year, month, and what type of day it is (Weekend or Weekday).
###    Holidays are coded as weekends. 
### Counts the number of weekend and weekday days in each month and includes a
###   variable that is the fishing day length.
### This is used later to expand sample to population. Also used for Table 1.
#!!!!!! This matches Iyob's 'calendar1' after his line 97
calSum <- data.frame(DATE=seq(SDATE,FDATE,1)) %>%
  mutate(YEAR=year(DATE),
         MONTH=droplevels(month(DATE,label=TRUE)),
         DAYTYPE=iMvDaytype(wday(DATE,label=TRUE),MONTH,mday(DATE))
  ) %>%
  group_by(YEAR,MONTH,DAYTYPE) %>%
  summarize(DAYS=n()) %>%
  mutate(DAYLEN=iMvDaylen(MONTH)) %>%
  as.data.frame()


## Interviewed fishing effort ----
### Read and prepare interviews file
###   Remove days with no effort between SDATE and FDATE (HOURS will be NA)
###   Remove unneeded variables
###   Drop unused levels
#!!!!!! This largely matches Iyob's 'ints' after his line 129 ... this includes
#!!!!!! YEAR variables; DATE is a different format; and I dropped the
#!!!!!! CLIPXX, LENXX, and SPECXX variables that had no data.
ints_ORIG <- readInterviewData(WDIR,LOC,SDATE,FDATE,type=FTYPE,
                               dropCLS=TRUE,dropHM=TRUE) %>%
  filter(!is.na(HOURS)) %>%
  select(-FISH,-RES,-SUCCESS) %>%
  droplevels()

### A simplified data.frame that does not include any fish data
###   HOURS: fishing effort by the party.
###   PERSONS: number of individuals in the party.
ints_NOFISH <- select(ints_ORIG,INTID:PERSONS)

### Number of interviews (NINTS) and interviewed fishing effort (HOURS)
###   across sites within strata (STATE, DAYTYPE, FISHERY, MONTH).
### This is used for Table 2.
intvdEffortStates <- ints_NOFISH %>%
  group_by(YEAR,STATE,DAYTYPE,FISHERY,MONTH,.drop=FALSE) %>%
  summarize(NINTS=n(),HOURS=sum(HOURS)) %>%
  as.data.frame()

### Summarized interviewed effort by WATERS, DAYTYPE, FISHERY, and MONTH ...
### similar to above but by WATERS rather than STATE and more summaries ...
###   NINTS= Number of interviews
###   HOURS= Total interviewed effort (hours) of ALL parties
###   VHOURS= Square of HOURS (in SAS this is uncorrected sum-of-squares)
###   MTRIP= Mean interviewed effort (hours) by COMPLETED parties
###   PROP= Proportion of total interviewed effort for month-daytype that is in
###         a given waters-fishery. Should sum to 1 within each month-daytype
###         Check with: group_by(effort,MONTH,DAYTYPE) %>% summarize(sum(PROP))
###   PARTY= Party size (person's per party)
#!!!!!! This matches Iyob's 'effort' after his line 183
intvdEffortWaters <- sumInterviewedEffort(ints_NOFISH)

## Pressure counts ----
### Read count pressure data and compute the following:
###   WAIT= Total amount of time that clerk was at the SITE during that shift.
###   COUNT= Total count of boats at the landing during the shift (note that
###          this has been adjusted for boats that came back during the shift
###          and boats that left after the shift started. Thus, there are 
###          fractions in these counts).
###   ** Both WAIT & COUNT have been combined for multiple visits during shift
#!!!!!! This largely matches Iyob's 'counts1' after his line 198. However,
#!!!!!!  this has one fewer records because one of the 27-Sep had a bad STARTHH.
#!!!!!!  Finally, Iyob's code did not restrict to within the survey period or
#!!!!!!  convert missing counts to zeroes, but the SAS code did, and I did here.
pressureCount <- readPressureCountData(WDIR,LOC,SDATE,FDATE,
                                       type=FTYPE,dropHM=TRUE)

### Expand the daily count pressure data to be a summary for each MONTH and
### DAYTYPE with the following variables:
###   NCOUNTS= Number of days clerk estimated pressure counts
###   DAYS= Number of days in the month
###   COUNT= Total pressure count (number of boats)
###   VCOUNT= Variance of pressure count (SD^2)
### This is used for Table 3.
#!!!!!! This matches Iyob's 'counts' after his line 215 (except no SDCOUNT as
#!!!!!! this is not used elsewhere and is just sqrt of VCOUNT).
pressureCount <- expandPressureCounts(pressureCount,calSum)

## Combining Effort and Counts ----
### This is used for Table 4
#!!!!!! This largely matches Iyob's 'effort' after his line 247
#!!!!!! Note that Iyob rounded his numerics to three decimal places
ttlEffort <- sumEffort(intvdEffortWaters,pressureCount)
writeDF(ttlEffort,fnpre)


## Calculate Harvest ----
### Isolated and rearranged fish information from interviews
### Removed non-fishing records
#!!!!!! This largely matches Iyob's 'ints2' after his line 320, though I have
#!!!!!!   several additional variables here (select to get closer to Iyob's).
ints_FISH <- rearrangeFishInfo(ints_ORIG) %>%
  filter(FISHERY!="NON-FISHING") %>%
  select(INTID,YEAR,WATERS,DAYTYPE,FISHERY,MONTH,DATE,STATE,SITE,HOURS,
         SPECIES,CLIP,CLIPPED,LEN)

### Summarize harvest by strata and species
#!!!!!! This largely matches Iyob's 'ints_new' after his line 347
intvdHarv <- sumObsHarvest(ints_FISH)

### Combine effort with harvest data (for NON-FISHING records)
### Remove non-fishing records from effort data, select specific variables
#!!!!!! This largely matches Iyob's 'ints_effort' after his line 351
ttlEffort2 <- filter(ttlEffort,FISHERY!="NON-FISHING") %>%
  select(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,
         NINTS,HOURS,VHOURS,INDHRS,PHOURS,VPHOURS)

### Summarize harvest and effort together
###   NINTS= Number of interviews
###   HARVEST= Total estimated harvest
###   VHARVEST= Variance of total estimated harvest
###   INDHRS= Hours of fishing effort for all individuals
### This is used for Table 5.
#!!!!!! This contains Iyob's 'ints_effort' after his line 375
ttlHarvest <- sumHarvestEffort(intvdHarv,ttlEffort2)
writeDF(ttlHarvest,fnpre)



## Length summaries ----
## This is used to make Table 9.
#!!!!!! This matches Iyob's 'lengthall' after his line 420
lengths <- select(ints_FISH,YEAR,STATE,FISHERY,MONTH,DATE,SITE,
                  SPECIES,CLIP,CLIPPED,LEN)
writeDF(lengths,fnpre)

### Lengths summarized by species, month, and finclipped (or not)
#!!!!!! This matches the data needed for TABLE 6
sumLengthSMF <- sumLengths(lengths,CLIPPED)

### Lengths summarized by species, month, and fin-clip (only for those clipped)
#!!!!!! This matches the data needed for TABLE 7 (except that I included only
#!!!!!! those species that had at least one observed fin-clip).
specClipped <- unique(filter(lengths,CLIPPED=="FINCLIP")$SPECIES)
sumLengthSMC <- sumLengths(filter(lengths,SPECIES %in% specClipped),CLIP)

