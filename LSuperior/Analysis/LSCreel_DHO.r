#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
#
# PROGRAM TO ANALYZE "BUS ROUTE" TYPE LAKE SUPERIOR CREEL
#   SINGLE ROUTE  -   INTEGRATED EFFORT AT LANDING COUNT
#
#   VERSION 1         JULY, 2016  (Iyob T)
#   VERSION 2         XXXX, 201X  (Derek O)
#
#  DIRECTIONS:
#   * Fill in initials for filename below at LOC
#   * Fill in effective state and final dates for creel below at SDATE & FDATE
#
#  NOTES:
#   * Counts (for Lake Superior) are average number of parties present during
#     the wait time, not total effort seen during the wait time.
#   * Only official holidays are New Years, Memorial Day, July Fourth, and Labor
#     Day (Thanksgiving and Christmas are not included).
#   * FINCLIP=99 means length field has number of fish harvested.
#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

## User-Specified Information ----
LOC <- "sup"  # must be one of "ash","byf","cpw","lsb","rdc","sax","sup", "wsh"
SDATE <- "05/21/2014" # must use two digits mon/day/year format
FDATE <- "09/30/2014"
MAKE_TABLES <- TRUE


## Setup ----
setwd(paste0(here::here(),"/LSuperior/Analysis"))
source("helpers/LSCreel_helpers.R")
if (MAKE_TABLES) source("helpers/LSCreel_tables.R")
### Converts SDATE and FDATE to useful objects
SDATE <- as.Date(SDATE,"%m/%d/%Y")
FDATE <- as.Date(FDATE,"%m/%d/%Y")


## Expansion factors (Table 1) ----
### Make data.frame of dates from starting to ending date (entered above)
###    include the year, month, and what type of day it is (Weekend or Weekday).
###    Holidays are coded as weekends. 
#!!!!!! This matches Iyob's 'calendar' after his line 91.
### Counts the number of weekend and weekday days in each month and includes a
###   variable that is the fishing day length.
#!!!!!! This matches Iyob's 'calendar1' after his line 97
calSum <- data.frame(DATE=seq(SDATE,FDATE,1)) %>%
  mutate(YEAR=year(DATE),
         MONTH=month(DATE,label=TRUE,abbr=FALSE),
         MONTH=droplevels(MONTH),
         DAYTYPE=iMvDaytype(wday(DATE,label=TRUE,abbr=TRUE),MONTH,mday(DATE))
  ) %>%
  group_by(YEAR,MONTH,DAYTYPE) %>%
  summarize(DAYS=n()) %>%
  mutate(DAYLEN=iMvDaylen(MONTH)) %>%
  as.data.frame()

## Make Table 1
#!!!!!! This matches Iyob's Table 1' after his line 115
if (MAKE_TABLES) table1(calSum,LOC,SDATE,FDATE)


## Interview Data ----
### Read and prepare interviews file
###   Remove days with no effort between SDATE and FDATE (HOURS will be NA)
###   Remove unneeded variables
###   Drop unused levels
### HOURS is fishing effort by the party.
#!!!!!! This largely matches Iyob's 'ints' after his line 129 ... this includes
#!!!!!! YEAR and WDAY variables; DATE is a different format; and I dropped the
#!!!!!! CLIPXX, LENXX, and SPECXX variables that had no data.
ints <- readInterviewData(LOC,SDATE,FDATE,dropCLS=TRUE,dropHM=TRUE) %>%
  filter(!is.na(HOURS)) %>%
  select(-FISH,-RES,-SUCCESS) %>%
  droplevels()

## Create a simplified data.frame that does not include any fish data
intsXX <-select(ints,WATERS,STATE,SITE,DATE,YEAR,MONTH,DAY,WDAY,DAYTYPE,
                FISHERY,STATUS,PERSONS,HOURS)

## This summarizes (# of interviews and hours across sites within strata)
tmp1 <- group_by(intsXX,YEAR,WATERS,STATE,DAYTYPE,FISHERY,MONTH,.drop=FALSE) %>%
  summarize(NINTS=n(),HOURS=sum(HOURS))

### Table 2 -- Number of interviews and interviewed fishing effort by strata
#!!!!!! This matches Iyob's Table 2 after his line 149
if (MAKE_TABLES) table2(ints,LOC,SDATE,FDATE)



## Summarize Fishing Effort ----
### Summarized interviewed effort by MONTH, DAYTYPE, WATERS, FISHERY
###   N= Number of interviews
###   HOURS= Total interviewed effort (hours) of ALL parties
###   VHOURS= Square of HOURS (in SAS this is uncorrected sum-of-squares)
###   MTRIP= Mean interviewed effort (hours) by COMPLETED parties
###   PROP= Proportion of total interviewed effort for month-daytype that is in
###         a given waters-fishery. Should sum to 1 within each month-daytype
###         Check with: group_by(effort,MONTH,DAYTYPE) %>% summarize(sum(PROP))
###   PARTY= Party size (person's per party)
#!!!!!! This matches Iyob's 'effort' after his line 183
effort <- sumEffort(intsXX)
head(effort)

## Count Data ----
### Read count pressure data and compute the following:
###   WAIT= Total amount of time that clerk was at the SITE during that shift.
###   COUNT= Total count of boats at the landing during the shift (note that
###          this has been adjusted for boats that came back during the shift
###          and boats that left after the shift started. Thus, there are 
###          fractions in these counts).
###   ** Both WAIT & COUNT have been combined for multiple visits during shift
#!!!!!! This largely matches Iyob's 'counts1' after his line 198.  However,
#!!!!!!  this has one fewer records because one of the 27-Sep had a bad STARTHH.
#!!!!!!  Finally, Iyob's code did not restrict to within the survey period or
#!!!!!!  convert missing counts to zeroes, but the SAS code did, and I did here.
counts <- readPressureCountData(LOC,SDATE,FDATE,dropHM=TRUE)
head(counts)

### Expand the daily count pressure data to be a summary for each MONTH and
### DAYTYPE with the following variables:
###   N= Number of days clerk estimated pressure counts
###   DAYS= Number of days in the month
###   COUNT= Total pressure count
###   SDCOUNT= SD of pressure count
###   VCOUNT= Variance of pressure count
#!!!!!! This matches Iyob's 'counts' after his line 215
counts <- expandPressureCounts(counts,calSum)
counts

### Make TABLE 3 here

## Combining Effort and Counts ----
### Combine effort and counts into one data.frame with new calculations
###   PHOURS= Party hours
###   TRIPS= Total trips
###   INDHRS= Total individual hours
#!!!!!! This largely matches Iyob's 'effort' after his line 247
#!!!!!! Note that Iyob rounded his numerics to three decimal places
effort <- merge(effort,counts,by=c("MONTH","DAYTYPE")) %>%
  mutate(PHOURS=COUNT*PROP,
         VPHOURS=VCOUNT*(PROP^2),
         TRIPS=PHOURS/MTRIP,
         VTRIPS=VPHOURS/(MTRIP^2),
         INDHRS=PHOURS*PARTY,
         VINDHRS=VPHOURS*(PARTY^2)) %>%
  arrange(MONTH,DAYTYPE,WATERS,FISHERY)

# show results with numerics round to two decimals
showRoundedNumerics(effort)


### Table 4 here


## Calculate Harvest ----
### Isolated and rearranged fish information from interviews
### Removed non-fishing records
#!!!!!! This largely matches Iyob's 'ints2' after his line 320, though I have
#!!!!!!   several additional variables here (select to get closer to Iyob's).
ints2 <- rearrangeFishInfo(ints) %>%
  filter(FISHERY!="NON-FISHING") %>%
  select(INTID,DATE,YEAR,MONTH,DAYTYPE,WATERS,STATE,FISHERY,SITE,HOURS,
         SPECCODE,SPEC,CLIPCODE,CLIP,CLIPPED,LEN)

### Summarize harvest by strata and species
#!!!!!! This largely matches Iyob's 'ints_new' after his line 347
harv <- sumHarvest(ints2)

### Combine effort with harvest data (for NON-FISHING records)
### Remove non-fishing records from effort data, select specific variables
#!!!!!! This largely matches Iyob's 'ints_effort' after his line 351
effort2 <- filter(effort,FISHERY!="NON-FISHING") %>%
  select(WATERS,FISHERY,MONTH,DAYTYPE,NINTS,HOURS,VHOURS,INDHRS,PHOURS,VPHOURS)

### Summarize harvest and effort together
#!!!!!! This matches Iyob's 'ints_effort' after his line 375
harv_eff <- sumHarvestEffort(harv,effort2)
rm(effort2)

# show results with numerics round to three decimals
showRoundedNumerics(harv_eff)

### Table 5 here


## Working with lengths ----
#!!!!!! This matches Iyob's 'lengthall' after his line 420
lengths <- select(ints2,FISHERY,DATE,YEAR,MONTH,STATE,SITE,
                  SPEC,CLIP,CLIPCODE,CLIPPED,LEN)

### Lengths summarized by species, month, and finclipped (or not)
#!!!!!! This matches the data needed for TABLE 6
sumLenSMF <- sumLengths(lengths,CLIPPED)

### Lengths summarized by species, month, and fin-clip (only for those clipped)
#!!!!!! This matches the data needed for TABLE 7 (except that I included only
#!!!!!! those species that had at least one observed fin-clip).
( specClipped <- unique(filter(lengths,CLIPPED=="FINCLIP")$SPEC) )
sumLenSMC <- sumLengths(filter(lengths,SPEC==specClipped),CLIP)

