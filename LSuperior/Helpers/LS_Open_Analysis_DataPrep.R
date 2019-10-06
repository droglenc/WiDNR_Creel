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
source(file.path(WDIR,"Helpers","LS_Open_Analysis_Helpers.R"))

### Converts SDATE and FDATE to useful objects
SDATE <- as.Date(SDATE,"%m/%d/%Y")
FDATE <- as.Date(FDATE,"%m/%d/%Y")

### Filename prefix
fnpre <- paste0(RDIR,"/",LOC,"_",YR,"_")


## Pressure counts -------------------------------------------------------------
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
  dplyr::mutate(YEAR=YR,
                MONTH=droplevels(lubridate::month(DATE,label=TRUE)),
                WDAY=lubridate::wday(DATE,label=TRUE),
                MDAY=lubridate::mday(DATE),
                DAYTYPE=FSA::mapvalues(WDAY,
                                       from=c("Mon","Tue","Wed","Thu","Fri",
                                              "Sat","Sun"),
                                       to=c("Weekday","Weekday","Weekday",
                                            "Weekday","Weekday",
                                            "Weekend","Weekend"),warn=FALSE),
                ## Handle Hoidays (find them, make them a weekend)
                DAYTYPE=dplyr::case_when(
                  MONTH=="Jan" & MDAY==1 ~ "Weekend",                # New Years Day
                  MONTH=="May" & MDAY>=25 & WDAY=="Mon" ~ "Weekend", # Memorial Day
                  MONTH=="Jul" & MDAY==4 ~ "Weekend",                # 4th of July
                  MONTH=="Sep" & MDAY<=7 & WDAY=="Mon" ~ "Weekend",  # Labor Day
                  TRUE ~ as.character(DAYTYPE)),
                DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend"))) %>%
  ## Count the number of each DAYTYPE in each MONTH
  dplyr::group_by(YEAR,MONTH,DAYTYPE) %>%
  dplyr::summarize(DAYS=n()) %>%
  ## Add on the day length for each month
  dplyr::mutate(DAYLEN=DAY_LENGTH[as.character(MONTH)]) %>%
  as.data.frame()


# RESULT: data.frame of interviewer's time and observed vehicle count at a site
#   * YEAR: Year of analysis
#   * MONTH: Month of observation
#   * DAY: Day of observation
#   * DAYTYPE: Type of day (Weekday or Weekend ... holidays are weekends)
#   * SITE: Specific site code
#   * WAIT: Total amount of time that clerk was at the site during that shift
#   * COUNT: Total amount of time that boats present at the landing during the
#            shift that were fishing. This is the average count of boats at the
#            landing during each stop (adjusted for boats that came back or left
#            during the stop; thus, there are fractions in these counts) times
#            each amount of time the clerk was at the stop summed across 
#            multiple stops at the site.
# NOTES:
#   * Counts (for Superior) from the original file are average number of parties
#     present during the wait time, not total effort seen during the wait time.
#   * These are observed values
#   * Both WAIT & COUNT have been combined for multiple visits during shift
# USE: Expanded to population counts below.
# EXPORTED: Not exported to a file.
pressureCount <- readxl::read_excel(file.path(RDIR,CNTS_FILE)) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Filter to chosen route
  dplyr::filter(ROUTE==LOC,YEAR==YR) %>%
  dplyr::mutate(
    DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%Y"),
    MONTH=lubridate::month(DATE,label=TRUE,abbr=TRUE),
    MONTH=droplevels(factor(month.abb[MONTH],levels=month.abb,ordered=TRUE)),
    WDAY=lubridate::wday(DATE,label=TRUE,abbr=TRUE),
    DAYTYPE=FSA::mapvalues(DAYTYPE,from="Weekend/Holiday",to="Weekend"),
    DAYTYPE=droplevels(factor(DAYTYPE,levels=c("Weekday","Weekend"))),
    SITE=paste0(SITE,"-",FSA::capFirst(SITEDESC)),
    # Calculate the "WAIT" time (hours at the site)
    WAIT=iHndlHours(STARTHH,STARTMM,STOPHH,STOPMM,DATE,SDATE,FDATE),
    # Convert average counts (the original TOTAL variable) to "total effort"
    # during shift (by muliplying by the WAIT time) so that multiple shifts on
    # each day can be combined (from original SAS code).
    COUNT=TOTAL*WAIT
  ) %>% 
  # Remove records with "bad" wait times
  dplyr::filter(!is.na(WAIT)) %>%
  # selec only variables that will be used later
  dplyr::select(YEAR,MONTH,DAY,WDAY,DAYTYPE,SITE,WAIT,COUNT) %>%
  # Combine observations of WAIT and COUNT from multiple visits to the same
  #   SITE within the same day
  dplyr::group_by(YEAR,MONTH,DAY,WDAY,DAYTYPE,SITE) %>%
  dplyr::summarize(WAIT=sum(WAIT),COUNT=sum(COUNT)) %>%
  as.data.frame()

# RESULT: data.frame of pressure count data expanded to represent the entire
#         population and then summarized by MONTH and DAYTYPE.
#   * YEAR: Year of analysis
#   * MONTH: Month of interview (now contains an "All" month)
#   * DAYTYPE: Type of day (now contains an "All" day type)
#   * NCOUNT: Number of days in the month/daytype clerk did pressure count
#   * DAYS: Number of days in the month/daytype
#   * TCOUNT: Total pressure count in the month/daytype
#   * SDCOUNT: SD of pressure count
#   * VCOUNT: Variance of pressure count (SD^2)
# NOTES: 
# USE: Used for Table 3. Used below for total effort.
# EXPORTED: Not exported to a file.
pressureCount <- expandPressureCounts(pressureCount,calSum)



## Interviewed fishing effort --------------------------------------------------
# RESULT: A data.frame of raw interview data.
# NOTES:
#   * Days that had no effort between SDATE and FDATE were removed (these rows
#     would have NA for the HOURS variable). This also removes days with bad
#     SDATE and FDATE values (e.g., SDATE>FDATE)
#   * The SUCCESS (whether interviewee caught fish), RES (residency), and 
#     FISH (number of fish caught) variables were removed (not used further)
# USE: Filtered into a smaller data.frames below (more details there)
#      Joined with the fish data.
# EXPORTED: Not exported to a file.
intvs_ORIG <- readxl::read_excel(file.path(RDIR,INTS_FILE)) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Filter to chosen route
  dplyr::filter(ROUTE==LOC,YEAR==YR) %>%
  ## Rename a few variables
  dplyr::rename(SPECIES=SPP,HARVEST=NUM) %>%
  dplyr::mutate(
    ## Handle dates (find weekends/days) & times (incl. hrs of effort)
    DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%Y"),
    MONTH=lubridate::month(DATE,label=TRUE,abbr=TRUE),
    MONTH=droplevels(factor(month.abb[MONTH],levels=month.abb,ordered=TRUE)),
    WDAY=lubridate::wday(DATE,label=TRUE,abbr=TRUE),
    ## Convert to factors
    DAYTYPE=FSA::mapvalues(DAYTYPE,from="Weekend/Holiday",to="Weekend"),
    DAYTYPE=droplevels(factor(DAYTYPE,levels=c("Weekday","Weekend"))),
    FISHERY=droplevels(factor(FISHERY,levels=lvlsFISHERY)),
    MUNIT=droplevels(factor(UNIT,levels=c("WI-1","WI-2","MI","MN"))),
    SPECIES=FSA::capFirst(SPECIES),
    SPECIES=droplevels(factor(SPECIES,levels=lvlsSPECIES)),
    ## Create longer site description
    SITE=paste0(SITE,"-",FSA::capFirst(SITEDESC)),
    ## Find hrs of effort
    HOURS=iHndlHours(STARTHH,STARTMM,STOPHH,STOPMM,DATE,SDATE,FDATE)) %>%
  ## Rearrange vars (& drop SUCCESS, RES, STARTHH, STARTMM, STOPHH, STOPMM,NUM)
  dplyr::select(INTERVIEW,DATE,YEAR,MUNIT,STATE,FISHERY,DAYTYPE,MONTH,DAY,
                SITE,STATUS,HOURS,PERSONS,SPECIES,HARVEST) %>%
  ## Drop "bad" HOURS records
  dplyr::filter(!is.na(HOURS)) %>%
  as.data.frame()

# RESULT: A subset of intvs_ORIG without any fish variables.
#   * YEAR: Year of interview
#   * MUNIT: Management unit where interviewee fished (e.g., WI-1, WI-2)
#   * STATE: State where inteviewee fished (e.g., WI, WI/MN)
#   * FISHERY: Type of fishery (e.g., Cold-Open, Warm-Open)
#   * DAYTPE: Type of day (Weekday or Weekend ... holidays are weekends)
#   * MONTH: Month of interview 
#   * HOURS: Hours of fishing effort reported by the interviewee
#   * PERSONS: Number of individuals in the fishing party
#   * STATUS: Whether the interview represents a completed trip or not
# NOTES:
#   * This is observed data, not yet expanded to all days in month/year.
# USE: Ultimately sent to make Table 2 and expanded to entire population below.
# EXPORTED: Not exported to a file.
intvs <- intvs_ORIG %>%
  ## Reduce to records of All Fish only to remove duplicated variables
  dplyr::filter(SPECIES=="All Fish") %>%
  ## Restrict to variables of interest
  dplyr::select(YEAR,MUNIT,STATE,FISHERY,DAYTYPE,MONTH,HOURS,PERSONS,STATUS)

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
intvdEffortWaters <- sumInterviewedEffort(intvs)




## Combining Counts and Effort -------------------------------------------------
# RESULT: A data.frame that summarizes (see below) total fishing effort expended
#         by strata (MUNIT, ROUTE, FISHERY, DAYTYPE, MONTH)
#   * WATERS: Wisconsin or not Wisconsin waters
#   * PHOURS: Total party hours of fishing
#   * SDPHOURS: Standard deviation of above
#   * PARTY: Mean party size (person's per party)
#   * INDHRS: Total individual/person hours of fishing
#   * SDINDHRS: Standard deviation of above
#   * MTRIP: Mean interviewed effort (hours) by COMPLETED parties
#   * TRIPS: Total number of fishing trips
#   * SDTRIPS: Standard deviation of above
#
# The following are intermediate calculations returned for completeness and use
#  in sumHarvestEffort() below.
#   * NINTS: Number of actual interviews
#   * HOURS: Total interviewed effort (hours) of ALL parties
#   * USSHOURS: Uncorrected SS of HOURS (explained above)
#   * VPHOURS: Variance of PHOURS
#   * VINDHRS: Variance of INDHRS
#   * VTRIPS: Variance of TRIPS
# NOTES:
#   * This is expanded to the entire population (not just observations)
# USE: 
#   * Table 4 and Figures 1 & 2.
#   * To expand catch to harvest further below.
# EXPORTED: Exported as "LOCATION_YEAR_ttlEffort.csv".
ttlEffort <- sumEffort(intvdEffortWaters,pressureCount) %>% 
  dplyr::mutate(ROUTE=LOC,
                WATERS=iMvWaters(MUNIT),
                WATERS=factor(WATERS,levels=c("Wisconsin","Non-Wisconsin"))) %>%
  dplyr::select(YEAR,ROUTE,WATERS,MUNIT:VTRIPS)
writeDF(ttlEffort,fnpre)



## Calculate Harvest -----------------------------------------------------------
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
intvdHarv <- sumObsHarvest(intvs_ORIG)


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
  dplyr::select(YEAR,MUNIT,DAYTYPE,FISHERY,MONTH,
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
  dplyr::mutate(ROUTE=LOC,
                WATERS=iMvWaters(MUNIT),
                WATERS=factor(WATERS,levels=c("Wisconsin","Non-Wisconsin"))) %>%
  dplyr::select(YEAR,ROUTE,WATERS,MUNIT:HRATE)
writeDF(ttlHarvest,fnpre)



## Length summaries ------------------------------------------------------------
# RESULT: A data.frame of raw individual fish data.
# USE: Joined with interview data below.
# EXPORTED: Not exported to a file.
fish <- readxl::read_excel(file.path(RDIR,FISH_FILE)) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Filter to chosen route and survey years
  dplyr::filter(ROUTE==LOC,SURVEY==YR) %>%
  dplyr::rename(SPECIES=SPP) %>%
  dplyr::select(INTERVIEW,SPECIES,LENGTH,CLIP) %>%
  dplyr::mutate(SPECIES=FSA::capFirst(SPECIES),
                SPECIES=droplevels(factor(SPECIES,levels=lvlsSPECIES)))

# RESULT: re-arranged data.frame from ints_FISH for export
#   * All but WT variable defined previously.
#   * Added weight (WT) variable in grams.
# NOTES:
#   * None.
# USE: For Tables 6-9 and Figure 6.
# EXPORTED: Exported to "LOCATION_YEAR_lengths.csv"
intvs_FISH <- intvs_ORIG %>%
  filter(SPECIES=="All Fish") %>%
  select(INTERVIEW,YEAR,MUNIT,FISHERY,MONTH,DATE,SITE)

lengths <- dplyr::right_join(intvs_FISH,fish,by="INTERVIEW") %>%
  dplyr::mutate(ROUTE=LOC,
                WATERS=iMvWaters(MUNIT),
                WATERS=droplevels(factor(WATERS,
                                  levels=c("Wisconsin","Non-Wisconsin"))),
                CLIP=droplevels(factor(CLIP,levels=lvlsCLIP)),
                CLIPPED=ifelse(CLIP=="Native","No Clip","Clip")) %>%
  dplyr::select(YEAR,ROUTE,WATERS,MUNIT,FISHERY,MONTH,
                DATE,SITE,SPECIES,CLIP,CLIPPED,LENGTH) %>%
  addWeights(RDIR,YEAR)
writeDF(lengths,fnpre)

