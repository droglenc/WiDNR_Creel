#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE
#
# DO NOT RUN THIS FILE AS A STAND-ALONE SCRIPT. THIS IS CALLED WHEN THE
#   LS_ANALYZER_TEMPLATE.Rmd FILE IS RENDERED FROM LS_ANALYZER.R FILE.
#
# THIS CODE, ALONG WITH LS_ANALSYIS_HELPERS.R IS THE "GUTS" OF THE ANALYSIS.
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!



## Setup -----------------------------------------------------------------------
### Load helper files
source(file.path(WDIR,"Helpers","LS_Analysis_Helpers.R"))

### Converts SDATE and FDATE to useful objects
SDATE <- as.Date(SDATE,"%m/%d/%Y")
FDATE <- as.Date(FDATE,"%m/%d/%Y")

### Filename prefix
fnpre <- fnPrefix(RDIR,LOC,SDATE)



## Create expansion factors ----------------------------------------------------
# RESULT: A data.frame with
#   * YEAR: Analysis year
#   * MONTH: Month
#   * DAYTYPE: Type of day (Weekday, Weekend ... Holidays are coded as Weekends)
#   * DAYS: Possible days of that day type per month
#   * DAYLEN: Total possible shift length (sampling period) for each month. Note
#             that these values are from the information file.
# NOTES:
#   * None.
# USE: Used to expand sampling info to population. Also basis for Table 1.
# EXPORTED: Not exported to a file.
calSum <- data.frame(DATE=seq(SDATE,FDATE,1)) %>%
  dplyr::mutate(YEAR=year(DATE),
                MONTH=droplevels(lubridate::month(DATE,label=TRUE)),
                DAYTYPE=iMvDaytype(lubridate::wday(DATE,label=TRUE),
                                   MONTH,
                                   lubridate::mday(DATE))
  ) %>%
  dplyr::group_by(YEAR,MONTH,DAYTYPE) %>%
  dplyr::summarize(DAYS=n()) %>%
  dplyr::mutate(DAYLEN=iMvDaylen(MONTH,DAY_LENGTH)) %>%
  as.data.frame()



## Interviewed fishing effort --------------------------------------------------
# RESULT: A data.frame of raw interview data.
# NOTES:
#   * CLIPXX, LENXX, and SPECIESXX variables that did not have data were removed
#   * Days that had no effort between SDATE and FDATE were removed (these rows
#     would have NA for the HOURS variable). This also removes days with bad
#     SDATE and FDATE values (e.g., SDATE>FDATE)
#   * The SUCCESS (whether interviewee caught fish), RES (residency), and 
#     FISH (number of fish caught) variables were removed (not used further)
# USE: Separated into two smaller data.frames below (more details there)
# EXPORTED: Not exported to a file.
ints_ORIG <- readInterviewData(INTS_FILE,RDIR,LOC,SDATE,FDATE)

# RESULT: A data.frame of individual interview data, from ints_ORIG but with
#         only interviee and no fish data.
#   * YEAR: Year of interview
#   * MONTH: Month of interview 
#   * WATERS: Waters where interviewee fished (WI or non-WI)
#   * MUNIT: Management unit where interviewee fished (e.g., WI-1, WI-2)
#   * STATE: State where inteviewee fished (e.g., WI, WI/MN)
#   * FISHERY: Type of fishery (e.g., Cold-Open, Warm-Open)
#   * DAYTPE: Type of day (Weekday or Weekend ... holidays are weekends)
#   * HOURS: Hours of fishing effort reported by the interviewee
#   * PERSONS: Number of individuals in the fishing party
# NOTES:
#   * This is observed data, not yet expanded to all days in month/year.
# USE: Ultimately sent to make Table 2 and expanded to entire population below.
# EXPORTED: Not exported to a file.
ints_NOFISH <- ints_ORIG %>%
  dplyr::select(YEAR,WATERS,MUNIT,STATE,FISHERY,DAYTYPE,MONTH,HOURS,PERSONS,STATUS)

# RESULT: A data.frame that summarizes the number of OBSERVED interviews and
#         reported hours of fishing effort by "strata" (!!WATERS!!, DAYTYPE,
#         FISHERY, MONTH) ... across sites and individual interviews.
#   * YEAR, WATERS, MUNIT, DAYTYPE, FISHERY, MONTH: as defined above
#   * NINTS: Total number of interviews in the stata
#   * HOURS: Total interviewed effort (hours) of ALL parties in the strata
#   * USSHOURS: Uncorrected sum-of-squares (square of hours) ... this will be
#               used to calculate the variance in sumHarvestEffort() below
#   * MTRIP: Mean interviewed effort (hours) by COMPLETED parties
#   * PROP: Proportion of total interviewed effort for month-daytype that is in
#           a given waters-fishery. Should sum to 1 within each month-daytype.
#           Check with: 
#             group_by(intvdEffortWaters,MONTH,DAYTYPE) %>% summarize(sum(PROP))
#   * PARTY: Mean party size (person's per party)
# NOTES:
#   * This is still observed results, not yet expanded to the population.
#   * This is "corrected" for split state fishing (e.g., half of hourly effort
#     in WI/MI state is apportioned to WI and MI.)
# USE: Used later for computing total effort (expanded to population)
# EXPORTED: Not exported to a file
intvdEffortWaters <- sumInterviewedEffort(ints_NOFISH)



## Pressure counts -------------------------------------------------------------
# RESULT: data.frame of interviewer's time and observed vehicle count at a site
#   * YEAR: Year of analysis
#   * MONTH: Month of observation
#   * DAY: Day of observation
#   * DAYTYPE: Type of day (Weekday or Weekend ... holidays are weekends)
#   * SITE: Specific site code
#   * WAIT: Total amount of time that clerk was at the site during that shift
#   * COUNT: Total count of boats at the landing during the shift (note that
#            this has been adjusted for boats that came back during the shift
#            and boats that left after the shift started. Thus, there are 
#            fractions in these counts).
# NOTES:
#   * These are observed values
#   * Both WAIT & COUNT have been combined for multiple visits during shift
# USE: Expanded to population counts below.
# EXPORTED: Not exported to a file.
pressureCount <- readPressureCountData(CNTS_FILE,RDIR,LOC,SDATE,FDATE)

# RESULT: data.frame of pressure count data expanded to represent the entire
#         population and then summarized by MONTH and DAYTYPE.
#   * YEAR: Year of analysis
#   * MONTH: Month of interview (now contains an "All" month)
#   * DAYTYPE: Type of day (now contains an "All" day type)
#   * NCOUNT: Number of days in the month/daytype clerk did pressure count
#   * DAYS: Number of days in the month/daytype
#   * COUNT: Total pressure count (number of boats) in the month/daytype
#   * SDCOUNT: SD of pressure count
#   * VCOUNT: Variance of pressure count (SD^2)
# NOTES: 
# USE: Used for Table 3. Used below for total effort.
# EXPORTED: Not exported to a file.
pressureCount <- expandPressureCounts(pressureCount,calSum)



## Combining Effort and Counts -------------------------------------------------
# RESULT: A data.frame that summarizes (see below) total fishing effort expended
#         by strata (WATERS, FISHERY, MONTH, DAYTYPE)
#   * PHOURS: Total party hours of fishing
#   * SDPHOURS: Standard deviation of above
#   * PARTY: Mean party size (person's per party)
#   * INDHRS: Total individual/person hours of fishing
#   * SDINDHRS: Standard deviation of above
#   * MTRIP: Mean interviewed effort (hours) by COMPLETED parties
#   * TRIPS: Total number of fishing trips
#   * SDTRIPS: Standard deviation of above
#
# The following are intermediate calculations returned for completeness
#   * NINTS: Number of actual interviews
#   * HOURS: Total interviewed effort (hours) of ALL parties
#   * USSHOURS: Uncorrected SS of HOURS (explained above)
#   * VPHOURS: Variance (standard deviation^2) of further above
#   * VINDHRS: Variance (standard deviation^2) of further above
#   * VTRIPS: Variance (standard deviation^2) of further above
# NOTES:
#   * This is expanded to the entire population (not just observations)
# USE: 
#   * Table 4 and Figures 1 & 2.
#   * To expand catch to harvest further below.
# EXPORTED: Exported as "LOCATION_YEAR_ttlEffort.csv".
ttlEffort <- sumEffort(intvdEffortWaters,pressureCount) %>% 
  dplyr::mutate(ROUTE=LOC) %>%
  dplyr::select(YEAR,ROUTE,WATERS:VTRIPS)
writeDF(ttlEffort,fnpre)



## Calculate Harvest -----------------------------------------------------------
# RESULT: data.frame from ints_ORIG of mostly just fish-related data
#   * INTID: A unique ID for each interview
#   * DATE: Data of interview ... maintained for Table 9
#   * YEAR: Year of interview
#   * WATERS: Waters fished (WI or non-WI)
#   * STATE: State fished (e.g., WI, WI/MN)
#   * SITE: Specific site code (of landing) ... maintained for Table 9
#   * DAYTYPE: Type of day (Weekday or Weekend ... holidays are weekends)
#   * FISHERY: Type of fishery (e.g., cold-open, warm-open)
#   * MONTH: Month of interview
#   * HOURS: Hours that party fished
#   * SPECIES: Species name of fish harvested
#   * CLIP: Fin-clip type (code + name)
#   * CLIPPED: Whether the fish was clipped or not (CLIP or NO CLIP)
#   * LEN: Measured length of the fish (inches, nearest tenth)
# NOTES:
#   * Removed non-fishing records
#   * At times the clerk only recorded the number of fish. In these cases the
#     number was expanded to separate rows for each fish but no LEN, CLIP, or
#     CLIPPED was recorded. These instances were recorded as CLIPCODE1=99 in
#     the original interviews data file.
# USE: To compute harvest and summarize length data further below.
# EXPORTED: Not exported to a file.
ints_FISH <- rearrangeFishInfo(ints_ORIG) %>%
  dplyr::filter(FISHERY!="NON-FISHING") %>%
  dplyr::select(INTID,DATE,YEAR,WATERS,MUNIT,STATE,SITE,DAYTYPE,FISHERY,MONTH,
                HOURS,SPECIES,CLIP,CLIPPED,LEN) %>%
  droplevels()

# RESULT: data.frame that summarizes (see below) harvest observed from
#         interviews by strata (WATERS, DAYTYPE, FISHERY, MONTH, SPECIES)
#   * YEAR, WATERS, DAYTYPE, FISHERY, MONTH: as defined above
#   * HARVEST: Observed number of fish harvested (by strata)
#   * USSHARVEST: Uncorrected sum-of-squares of HARVEST (i.e., square of
#                 HARVEST) ... will be used to compute the variance of HARVEST
#                 in sumHarvestEffort
#   * UCOVAR: Start of a covariance calculation
# NOTES:
#   * This is observed data, not yet expanded to all days in month/year.
#   * This was "corrected" for split state fishing (e.g., half of harvest
#     in WI/MI state is apportioned to WI and MI.)
# USE: To compute total harvest below
# EXPORTED: Not exported to a file.
intvdHarv <- sumObsHarvest(ints_FISH)

# RESULT: data.frame that is a subset of records and variables from ttlEffort
#   * All variables defined above.
# NOTES:
#   * Removed "NON-FISHING" records
#   * Selected specific variables as needed below
#   * This is largely an intermediate calculation only for the next step below
# USE: To compute total harvest below
# EXPORTED: Not exported to a file.
ttlEffort2 <- ttlEffort %>%
  dplyr::filter(FISHERY!="NON-FISHING") %>%
  dplyr::select(YEAR,WATERS,MUNIT,DAYTYPE,FISHERY,MONTH,
                NINTS,HOURS,USSHOURS,INDHRS,PHOURS,VPHOURS)

# RESULT:
#   * YEAR, WATERS, MUNIT DAYTYPE, FISHERY, MONTH: as above, may incl. "All"
#   * ROUTE: Route abbreviation
#   * SPECIES: Species of fish
#   * INDHRS: Total number of individual hours of fishing effort
#   * HARVEST: Total estimated harvest
#   * SDHARVEST: Standard deviation of total estimated harvest
#   * VHARVEST: Variance of total estimated harvest (SD^2; for completeness)
#   * HRATE: Harvest rate per individual
# NOTES:
#   * None.
# USE: For Table 5 and Figure 3-5.
# EXPORTED: Exported to "LOCATION_YEAR_ttlHarvest.csv"
ttlHarvest <- sumHarvestEffort(intvdHarv,ttlEffort2) %>%
  dplyr::mutate(ROUTE=LOC) %>%
  dplyr::select(YEAR,ROUTE,WATERS:HRATE)
writeDF(ttlHarvest,fnpre)



## Length summaries ------------------------------------------------------------
# RESULT: re-arranged data.frame from ints_FISH for export
#   * All but WT variable defined previously.
#   * Added weight (WT) variable in grams.
# NOTES:
#   * None.
# USE: For Tables 6-9 and Figure 6.
# EXPORTED: Exported to "LOCATION_YEAR_lengths.csv"
lengths <- ints_FISH %>%
  dplyr::mutate(ROUTE=LOC) %>%
  dplyr::select(YEAR,ROUTE,WATERS,MUNIT,STATE,FISHERY,MONTH,
                DATE,SITE,SPECIES,CLIP,CLIPPED,LEN) %>%
  addWeights(RDIR,YEAR)
writeDF(lengths,fnpre)

