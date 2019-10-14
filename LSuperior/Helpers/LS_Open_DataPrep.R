#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE
#
# DO NOT RUN THIS FILE AS A STAND-ALONE SCRIPT. THIS IS CALLED WHEN THE
#   LS_Open_Analsyis_Template.Rmd FILE IS RENDERED FROM LS_Open_Analysis.R.
#
# THIS CODE, ALONG WITH LS_Open_Helpers.R, IS THE "GUTS" OF THE ANALYSIS.
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!


## Setup -----------------------------------------------------------------------
### Load helpers file
source(file.path(WDIR,"Helpers","LS_Open_Helpers.R"))

### Converts SDATE and FDATE to useful objects
SDATE <- as.Date(SDATE,"%m/%d/%Y")
FDATE <- as.Date(FDATE,"%m/%d/%Y")

### Filename prefix
fnpre <- paste0(RDIR,"/",LOC,"_Open_",YR,"_")


## Pressure counts -------------------------------------------------------------
# DESCRIPTION: This creates a data.frame that identifies the total number of
#              each type of day (weekday and weekend) and the fishing day
#              length for each month that the route was run. The starting date,
#              ending date, and day lengths are from the route's information 
#              file read previously. These values are ultimately used to expand
#              the catch and effort observations (from the sample of days) to
#              entire population (all available days). This also forms Table 1.
# RESULT: A data.frame with ...
#   * YEAR: Analysis year
#   * MONTH: Month
#   * DAYTYPE: Type of day (Weekday, Weekend ... Holidays are coded as Weekends)
#   * DAYS: Possible days of that day type per month
#   * DAYLEN: Total possible shift length (sampling period) for each month.
calSum <- 
  ## Make a sequence of every day between SDATE and FDATE (from info file)
  data.frame(DATE=seq(SDATE,FDATE,1)) %>%
  dplyr::mutate(
    ## Make year, month, weekeday, day number within month, daytype variables
    YEAR=YR,
    MONTH=droplevels(lubridate::month(DATE,label=TRUE)),
    WDAY=lubridate::wday(DATE,label=TRUE),
    MDAY=lubridate::mday(DATE),
    DAYTYPE=FSA::mapvalues(WDAY,from=c("Mon","Tue","Wed","Thu","Fri",
                                       "Sat","Sun"),
                           to=c("Weekday","Weekday","Weekday","Weekday","Weekday",
                                "Weekend","Weekend"),warn=FALSE),
    ## Handle hoidays during open-water seasons (find them, make them a weekend)
    DAYTYPE=dplyr::case_when(
      MONTH=="Jan" & MDAY==1 ~ "Weekend",                # New Years Day
      MONTH=="May" & MDAY>=25 & WDAY=="Mon" ~ "Weekend", # Memorial Day
      MONTH=="Jul" & MDAY==4 ~ "Weekend",                # 4th of July
      MONTH=="Sep" & MDAY<=7 & WDAY=="Mon" ~ "Weekend",  # Labor Day
      TRUE ~ as.character(DAYTYPE)),
    ## Make daytype a factor (and control the order of levels)
    DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend"))) %>%
  ## Count the number of each DAYTYPE in each MONTH
  dplyr::group_by(YEAR,MONTH,DAYTYPE) %>%
  dplyr::summarize(DAYS=n()) %>%
  ## Add on the day length for each month (from info file)
  dplyr::mutate(DAYLEN=DAY_LENGTH[as.character(MONTH)]) %>%
  ## Make a regular data.frame (rather than tibble)
  as.data.frame()


# DESCRIPTION: This creates a data.frame from the original counts file for the
#              route. It converts the original start and ending times for the
#              clerk into a "wait" time and combines multiple visits to the same
#              site (the first site is often visited again as the last site).
#              It then computes the total amount of time that boats present at
#              the site during the shift were fishing. This is the average count
#              of boats at the landing during each stop (adjusted for boats that
#              came back or left during the stop; thus, there are fractions in
#              these "counts") times the amount of time the clerk was at the
#              site. These are observed values from the sampling of days that
#              will be expanded to all days further below.
# RESULT: A data.frame with ...
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
pressureCount <- 
  readxl::read_excel(file.path(RDIR,"data",CNTS_FILE)) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Filter to chosen route and year
  dplyr::filter(ROUTE==LOC,YEAR==YR) %>%
  dplyr::mutate(
    ## Add date, month, and daytype variables
    DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%Y"),
    MONTH=lubridate::month(DATE,label=TRUE,abbr=TRUE),
    MONTH=droplevels(factor(month.abb[MONTH],levels=month.abb,ordered=TRUE)),
    DAYTYPE=FSA::mapvalues(DAYTYPE,from="Weekend/Holiday",to="Weekend"),
    DAYTYPE=droplevels(factor(DAYTYPE,levels=c("Weekday","Weekend"))),
    ## Create new site variable from old site and site description variables
    SITE=paste0(SITE,"-",FSA::capFirst(SITEDESC)),
    ## Calculate "WAIT" time (hours at the site)
    WAIT=hndlHours(STARTHH,STARTMM,STOPHH,STOPMM,DATE,SDATE,FDATE),
  ) %>% 
  ## Remove records with "bad" wait times (see hndlHours description)
  dplyr::filter(!is.na(WAIT)) %>%
  ## Combine WAIT and COUNT from multiple visits to the same SITE in same day
  dplyr::group_by(YEAR,MONTH,DAY,DAYTYPE,SITE) %>%
  dplyr::summarize(TOTAL=sum(TOTAL),WAIT=sum(WAIT)) %>%
  ## Convert avg counts (original TOTAL) to "total hours" during shift
  dplyr::mutate(COUNT=TOTAL*WAIT) %>%
  ## Remove TOTAL variable that is no longer needed
  dplyr::select(-TOTAL) %>%
  ## Convert to regular data.frame (rather than tibble)
  as.data.frame()


# DESCRIPTION: This creates a data.frame of pressure counts expanded to all
#              days and times (rather than just sampled days and times). The
#              pressure counts are also summarized by month (across daytypes)
#              and daytypes (across months). This data.frame is used below to
#              find total fishing effort. This is used in Table 3.
# RESULT: A data.frame with ...
#   * YEAR: Year of analysis
#   * DAYTYPE: Type of day (now contains an "All" day type)
#   * MONTH: Month of interview (now contains an "All" month)
#   * NCOUNT: Number of days the clerk did pressure count
#   * DAYS: Total number of days in the month/daytype
#   * TCOUNT: Total pressure count
#   * SDCOUNT: SD of total pressure count
#   * VCOUNT: Variance of total pressure count (SD^2)
pressureCount <- expandPressureCounts(pressureCount,calSum)



## Interviewed fishing effort --------------------------------------------------
# DESCRIPTION: This creates a data.frame from the original interviess file.
#              Several of the original variables are dropped, other variables
#              are modified somewhat (to factors, re-arranged, etc.), and
#              number of hours of effort is calculated. This data.frame is
#              filtered into smaller data.frames further below. Meanings of the
#              variables are described for those smaller data.frames.
# NOTES:
#   * Days that had no effort between SDATE and FDATE were removed (these rows
#     would have NA for the HOURS variable). This also removes days with bad
#     SDATE and FDATE values (e.g., SDATE>FDATE).
#   * The SUCCESS (whether interviewee caught fish), RES (residency), and 
#     FISH (number of fish caught) variables were removed (not used further).
#     This is noted here just because it is different than the SAS code.
intvs_ORIG <- 
  readxl::read_excel(file.path(RDIR,"data",INTS_FILE)) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Filter to chosen route and year
  dplyr::filter(ROUTE==LOC,YEAR==YR) %>%
  ## Rename a few variables (to better match SAS code)
  dplyr::rename(SPECIES=SPP,HARVEST=NUM) %>%
  dplyr::mutate(
    ## Add date, month, and daytype variables
    DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%Y"),
    MONTH=lubridate::month(DATE,label=TRUE,abbr=TRUE),
    MONTH=droplevels(factor(month.abb[MONTH],levels=month.abb,ordered=TRUE)),
    DAYTYPE=FSA::mapvalues(DAYTYPE,from="Weekend/Holiday",to="Weekend"),
    DAYTYPE=droplevels(factor(DAYTYPE,levels=c("Weekday","Weekend"))),
    ## Convert fishery variable to factor (drop unused)
    FISHERY=droplevels(factor(FISHERY,levels=lvlsFISHERY)),
    ## Fill in management unit with state abbreviation for non-WI units
    ## and make management unit a factor, with specific order of levels
    MUNIT=dplyr::case_when(STATE=="Minnesota" ~ "MN",
                           STATE=="Michigan" ~ "MI",
                           TRUE ~ UNIT),
    MUNIT=droplevels(factor(MUNIT,levels=c("WI-1","WI-2","MI","MN"))),
    ## Make species names consistent, turn to factors, drop unused levels
    SPECIES=FSA::capFirst(SPECIES),
    SPECIES=droplevels(factor(SPECIES,levels=lvlsSPECIES)),
    ## Create longer site description
    SITE=paste0(SITE,"-",FSA::capFirst(SITEDESC)),
    ## Find hrs of effort
    HOURS=hndlHours(STARTHH,STARTMM,STOPHH,STOPMM,DATE,SDATE,FDATE)) %>%
  ## Rearrange vars (& drop several)
  dplyr::select(INTERVIEW,DATE,YEAR,STATE,MUNIT,FISHERY,DAYTYPE,MONTH,DAY,
                SITE,STATUS,HOURS,PERSONS,SPECIES,HARVEST) %>%
  ## Drop "bad" HOURS records
  dplyr::filter(!is.na(HOURS)) %>%
  ## Convert to regular data.frame (rather than tibble)
  as.data.frame()


# DESCRIPTION: This creates a data.frame that is a subset of intvs_ORIG that
#              removes all variables related to the fish (i.e., species and
#              number harvested), so that it contains only information specific
#              to the recorded fishing effort. This is observed data only for
#              the days sampled. It is expanded to the entire population of days
#              further below. This is also used to make Table 2.
# RESULT: A data.frame with ...
#   * YEAR: Year of interview
#   * STATE: State where inteviewee fished (e.g., WI, WI/MN)
#   * MUNIT: Management unit where interviewee fished (e.g., WI-1, WI-2)
#   * FISHERY: Type of fishery
#   * DAYTPE: Type of day (Weekday or Weekend ... holidays are weekends)
#   * MONTH: Month of interview 
#   * HOURS: Hours of fishing effort reported by the interviewee
#   * PERSONS: Number of individuals in the fishing party
#   * STATUS: Whether the interview represents a completed trip or not
intvs <- 
  intvs_ORIG %>%
  ## Reduce to records of All Fish only to remove variables that were duplicated
  ## for "All fish" and specific species
  dplyr::filter(SPECIES=="All Fish") %>%
  ## Restrict to variables of interest (i.e., remove "fish" variables)
  dplyr::select(YEAR,STATE,MUNIT,FISHERY,DAYTYPE,MONTH,HOURS,PERSONS,STATUS)


# DESCRIPTION: This creates a data.frame that summarizes the number of OBSERVED
#              interviews and reported hours of fishing effort by "strata"
#              (!!WATERS!!, DAYTYPE, FISHERY, MONTH) ... across sites and
#              individual interviews. These results include "corrections" for
#              split state fishing (e.g., half of hourly effort in WI/MI "state"
#              is apportioned to WI and MI). This is still observed results
#              which have not yet been expanded to the population of days. This
#              is used later for computing total effort (i.e., expanded to the
#              population of days).
# RESULT: A data.frame with ...
#   * YEAR, MUNIT, FISHERY, DAYTYPE, MONTH: as defined above
#   * NINTS: Total number of interviews in the strata
#   * HOURS: Total interviewed effort (hours) of ALL parties in the strata
#   * USSHOURS: Uncorrected sum-of-squares (square of hours) ... this will be
#               used to calculate the variance in sumHarvestEffort() below
#   * MTRIP: Mean interviewed effort (hours) by COMPLETED parties
#   * PROP: Proportion of total interviewed effort for month-daytype that is in
#           a given waters-fishery. Should sum to 1 within each month-daytype.
#           Check with: 
#             group_by(intvdEffortWaters,MONTH,DAYTYPE) %>% summarize(sum(PROP))
#   * PARTY: Mean party size (person's per party)
intvdEffortWaters <- sumInterviewedEffort(intvs)



## Combining Counts and Effort -------------------------------------------------
# DESCRIPTION: This creates a data.frame that summarizes (see below) total 
#              (expanded) fishing effort by strata (MUNIT, ROUTE, FISHERY,
#              DAYTYPE, MONTH). This is expanded to the entire population of
#              days (not just observations from the sample of days) and will be
#              used to expand catch to harvest further below. This is also used
#              for Table 4 and Figures 1 & 2 and is exported as "*_ttlEffort.csv"
# RESULT: A data.frame with ...
#   * YEAR, ROUTE, WATERS, MUNIT, FISHERY, DAYTYPE, MONTH: defined above.
#   * PHOURS: Total party hours of fishing
#   * SDPHOURS: Standard deviation of above
#   * PARTY: Mean party size (person's per party)
#   * INDHRS: Total individual/person hours of fishing
#   * SDINDHRS: Standard deviation of above
#   * MTRIP: Mean interviewed effort (hours) by COMPLETED parties
#   * TRIPS: Total number of fishing trips
#   * SDTRIPS: Standard deviation of above
#
#   The following are intermediate calculations returned for completeness and
#   used in sumHarvestEffort() below.
#   * NINTS: Number of actual interviews
#   * HOURS: Total interviewed effort (hours) of ALL parties
#   * USSHOURS: Uncorrected SS of HOURS (explained above)
#   * VPHOURS: Variance of PHOURS
#   * VINDHRS: Variance of INDHRS
#   * VTRIPS: Variance of TRIPS
ttlEffort <- 
  sumEffort(intvdEffortWaters,pressureCount) %>% 
  dplyr::mutate(ROUTE=LOC,
                WATERS=ifelse(MUNIT %in% c("MN","MI"),"Non-Wisconsin","Wisconsin"),
                WATERS=factor(WATERS,levels=c("Wisconsin","Non-Wisconsin"))) %>%
  dplyr::select(YEAR,ROUTE,WATERS,MUNIT:VTRIPS)
writeDF(ttlEffort,fnpre)



## Calculate Harvest -----------------------------------------------------------
# DESCRIPTION: This creates a data.frame that is filtered from intvs_ORIG to
#              remove the effort related variables to focus on the fish harvest
#              related variables and then summarized by strata (see below).
#              The original harvest data were "corrected" for split state
#              fishing (e.g., half of harvest in WI/MI state is apportioned to
#              WI and MI). This is still observed data that has not yet been
#              expanded to all days in month/year. This will be used to compute
#              total harvest further below.
# RESULT: A data.frame with ...
#   * YEAR, MUNIT, FISHERY, DAYTYPE, MONTH: as defined above
#   * SPECIES: Species harvest (note that "All fish" is for all fish harvested)
#   * HARVEST: Observed number of fish harvested (by strata)
#   * USSHARVEST: Uncorrected sum-of-squares of HARVEST (i.e., square of
#                 HARVEST) ... will be used to compute the variance of HARVEST
#                 in sumHarvestEffort
#   * UCOVAR: Start of a covariance calculation
intvdHarv <- sumObsHarvest(intvs_ORIG)


# DESCRIPTION: This creates a data.frame that is a subset of records and
#              variables from ttlEffort (created above). Of note it removes
#              records where the interview was related to "non-fishing". This is
#              an intermediate step to provide a data.frame used to compute
#              total harvest below.
ttlEffort2 <- 
  ttlEffort %>%
  dplyr::filter(FISHERY!="NON-FISHING") %>%
  dplyr::select(YEAR,MUNIT,DAYTYPE,FISHERY,MONTH,
                NINTS,HOURS,USSHOURS,INDHRS,PHOURS,VPHOURS)


# DESCRIPTION: This creates a data.frame that summarizes (see below) total
#              (expanded) harvest by strata. This is used in Table 5 and 
#              Figure2 3-5 and exported to "*_ttlHarvest.csv".
# RESULT: A data.frame with ...
#   * YEAR, ROUTE, WATERS, MUNIT, FISHERY,  DAYTYPE, MONTH, SPECIES: as defined
#     above, but may include "All"
#   * INDHRS: Total number of individual hours of fishing effort
#   * HARVEST: Total estimated harvest
#   * SDHARVEST: Standard deviation of total estimated harvest
#   * VHARVEST: Variance of total estimated harvest (SD^2; for completeness)
#   * HRATE: Harvest rate per individual
ttlHarvest <- 
  sumHarvestEffort(intvdHarv,ttlEffort2) %>%
  dplyr::mutate(ROUTE=LOC,
                WATERS=ifelse(MUNIT %in% c("MN","MI"),"Non-Wisconsin","Wisconsin"),
                WATERS=factor(WATERS,levels=c("Wisconsin","Non-Wisconsin"))) %>%
  dplyr::select(YEAR,ROUTE,WATERS,MUNIT:HRATE)
writeDF(ttlHarvest,fnpre)



## Length summaries ------------------------------------------------------------
# DESCRIPTION: This creates a data.frame from the raw individual fish data. It
#              is joined with interview data further below (see details there).
fish <- 
  readxl::read_excel(file.path(RDIR,"data",FISH_FILE)) %>%
  ## Change all variable names to upper-case (easier to remember)
  dplyr::rename_all(.funs=toupper) %>%
  ## Filter to chosen route and survey years
  dplyr::filter(ROUTE==LOC,SURVEY==YR) %>%
  ## Change species name and names for ease
  dplyr::rename(SPECIES=SPP) %>%
  dplyr::mutate(SPECIES=FSA::capFirst(SPECIES),
                SPECIES=droplevels(factor(SPECIES,levels=lvlsSPECIES))) %>%
  dplyr::select(INTERVIEW,SPECIES,LENGTH,CLIP)


# DESCRIPTION: This creates a data.frame that is a subset of just pertinent
#              fields from intvs_ORIG. This will be joined with the fish
#              data.frame so that lengths can be summarized by some of the
#              interview fields (see below).
intvs_FISH <- intvs_ORIG %>%
  filter(SPECIES=="All Fish") %>%
  select(INTERVIEW,YEAR,MUNIT,FISHERY,MONTH,DATE,SITE)


# DESCRIPTION: This creates a data.frame that contains all catch information
#              recorded about individual fish (of primary use here is the 
#              length and weight). Used in Tables 6-9 and Figure 6 and 
#              exported to "*_lengths.csv",
# RESULT: A data.frame with ...
#   * YEAR, ROUTE, WATERS, MUNIT, FISHERY, MONTH, DATE, SITE: described above.
#   * SPECIES: Species of fish harvested
#   * CLIP: Type of fin-clip ("Native" if not clipped)
#   * CLIPPED: Whether fin-clipped or not.
#   * LENGTH: Total length (in.)
#   * WEIGHT: Weight (g) predicted from length and weight-length regression
#             equations given in external file.
lengths <- 
  dplyr::right_join(intvs_FISH,fish,by="INTERVIEW") %>%
  dplyr::mutate(
    ROUTE=LOC,
    WATERS=ifelse(MUNIT %in% c("MN","MI"),"Non-Wisconsin","Wisconsin"),
    WATERS=droplevels(factor(WATERS,levels=c("Wisconsin","Non-Wisconsin"))),
    CLIP=droplevels(factor(CLIP,levels=lvlsCLIP)),
    CLIPPED=ifelse(CLIP=="Native","No Clip","Clip")) %>%
  dplyr::select(YEAR,ROUTE,WATERS,MUNIT,FISHERY,MONTH,
                DATE,SITE,SPECIES,CLIP,CLIPPED,LENGTH) %>%
  addWeights(RDIR,YEAR)
writeDF(lengths,fnpre)

