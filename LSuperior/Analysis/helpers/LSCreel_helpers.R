#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
# DO NOT CHANGE ANYTHING BENEATH HERE
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Load Packages ----
rqrd <- c("FSA","lubridate","tables","tidyr","dplyr","magrittr","huxtable",
          "kableExtra","captioner","ggplot2","patchwork","sugrrants","grid",
          "haven","knitr","here","readxl","tibble")
for (i in seq_along(rqrd)) suppressPackageStartupMessages(library(rqrd[i],
                                                          character.only=TRUE))

## Main Helpers ----

## Read and prepare the interview data file
readInterviewData <- function(wdir,LOC,SDATE,FDATE,type,
                              dropCLS=TRUE,dropHM=TRUE) {
  ## Add interview ID number
  ## Convert some codes to words
  ## Handle dates (find weekends and weekdays) & times (incl. hours of effort)
  ## NOTE: USING DATABASE VERSION OF DAYTYPE EVEN THOUGH IT HAS ERRORS SO THAT
  ##       I CAN TEST AGAINST SAS OUPTUT
  if (type=="CSV") d <- read.csv(paste0(wdir,"data/",LOC,"ints.csv"))
  else d <- haven::read_sas(paste0(wdir,"data/",LOC,"ints.sas7bdat"))
  d <- d %>%
    dplyr::mutate(INTID=1:n(),
                  STATE=iMvStates(STATE),
                  WATERS=iMvWaters(STATE),
                  FISHERY=iMvFishery(FISHERY),
                  STATUS=iMvStatus(STATUS),
                  RES=iMvResidency(RES),
                  DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%y"),
                  YEAR=lubridate::year(DATE),
                  MONTH=lubridate::month(DATE,label=TRUE,abbr=TRUE),
                  WDAY=lubridate::wday(DATE,label=TRUE,abbr=TRUE),
                  DAYTYPE=factor(FSA::mapvalues(DAYTYPE,from=1:2,
                                                to=c("Weekday","Weekend"))),
                  DAYTYPE2=iMvDaytype(WDAY,MONTH,DAY),
                  HOURS=iHndlHours(STARTHH,STARTMM,STOPHH,STOPMM,
                                   DATE,SDATE,FDATE))
  ## Drop hours and minutes variables if asked to
  if (dropHM) d <- dplyr::select(d,-(STARTMM:STOPHH))
  ## Drop CLIP, LEN, and SPEC variables that have no data, if asked to
  if (dropCLS) {
    allNA <- sapply(d,function(x) {
      if (is.character(x)) all(is.na(x) | x=="")
      else all(is.na(x))
    })
    d <- d[,!allNA]
  }
  ## Rearrange variables
  d %<>% dplyr::select(INTID,DATE,YEAR,WATERS,STATE,DAYTYPE,FISHERY,
                       MONTH,DAY,SITE,STATUS,HOURS,PERSONS,RES,FISH,SUCCESS,
                       contains("SPEC"),contains("CLIP"),contains("LEN"))
  ## Return data.frame
  as.data.frame(d)
}


##
sumInterviewedEffort <- function(dints) {
  ### Separate into only one and split states
  ### All hours for split states are cut in half
  f1 <- dplyr::filter(dints,!STATE %in% c("WI/MN","WI/MI"))
  f2 <- dplyr::filter(dints,STATE %in% c("WI/MN","WI/MI")) %>%
    dplyr::mutate(HOURS=0.5*HOURS)
  ### Duplicated f2 to get other half of HOURS, label as NON-WISCONSIN
  f3 <- dplyr::mutate(f2,WATERS="Non-WI")

  ### Combine to get all interviews corrected for location
  ### Compute people-hours of fishing effort (in INDHRS)
  ### Compute hours for only completed trips (in CHOURS)
  f <- rbind(f1,f2,f3) %>%
    dplyr::mutate(INDHRS=PERSONS*HOURS,
                  CHOURS=ifelse(STATUS=="Complete",HOURS,NA))
  
  ### Summarize interviewed effort data by WATERS, DAYTPE, FISHERY, MONTH  
  fsum <- f %>%
    dplyr::group_by(YEAR,WATERS,DAYTYPE,FISHERY,MONTH) %>%
    dplyr::summarize(NINTS=n(),
                     MTRIP=mean(CHOURS,na.rm=TRUE),
                     VHOURS=sum(HOURS^2,na.rm=TRUE),
                     HOURS=sum(HOURS,na.rm=TRUE),
                     INDHRS=sum(INDHRS,na.rm=TRUE),
                     CHOURS=sum(CHOURS,na.rm=TRUE)) %>%
    dplyr::select(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,
                  NINTS,HOURS,INDHRS,CHOURS,MTRIP,VHOURS) %>%
    as.data.frame()
  
  ### Summarize total interviewed hours by MONTH and DAYTYPE
  ### *** This is nints in SAS/IYOB code
  fsum2 <- dints %>%
    dplyr::group_by(MONTH,DAYTYPE) %>%
    dplyr::summarize(THOURS=sum(HOURS,na.rm=TRUE)) %>%
    as.data.frame()
  
  ### Combine the summarized effort with fsum2
  f <- merge(fsum,fsum2,by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(PROP=HOURS/THOURS,
                  PARTY=INDHRS/HOURS) %>%
    dplyr::select(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,
                  NINTS,HOURS,VHOURS,MTRIP,PROP,PARTY) %>%
    dplyr::arrange(WATERS,DAYTYPE,FISHERY,MONTH)
  ### Return data.frame
  as.data.frame(f)
}


## Read and prepare the interview data file
readPressureCountData <- function(wdir,LOC,SDATE,FDATE,type,dropHM=TRUE) {
  ###   Find various varsions of dates (note that DATE had to be handled
  ###     differently than above b/c four rather than two digits used here).
  ###   Convert missing COUNTs to zeroes
  ###   Calculate the "WAIT" time (hours at the site)
  ###   Convert average counts (the original COUNT variable) to "total effort"
  ###     during shift (by muliplying by the WAIT time) so that multiple shifts
  ###     on each day can be combined (from original SAS code).
  if (type=="CSV") d <- read.csv(paste0(wdir,"data/",LOC,"cnts.csv"))
  else d <- haven::read_sas(paste0(wdir,"data/",LOC,"cnts.sas7bdat"))
  d <- d %>%
    dplyr::mutate(DATE=as.Date(paste(MONTH,DAY,YEAR,sep="/"),"%m/%d/%Y"),
                  YEAR=lubridate::year(DATE),
                  MONTH=lubridate::month(DATE,label=TRUE,abbr=TRUE),
                  WDAY=lubridate::wday(DATE,label=TRUE,abbr=TRUE),
                  DAYTYPE=iMvDaytype(WDAY,MONTH,DAY),
                  COUNT=iConvNA20(COUNT),
                  WAIT=iHndlHours(STARTHH,STARTMM,STOPHH,STOPMM,
                                  DATE,SDATE,FDATE),
                  COUNT=COUNT*WAIT
    )
  ###   Drop hours and minutes variables if asked to do so
  if (dropHM) d <- dplyr::select(d,-(STARTMM:STOPHH))
  ###   Remove records with "bad" wait times
  ###   Narrow variable list down
  ###   Combine observations of WAIT and COUNT from multiple visits to the same
  ###     SITE within the same day
  d %<>% dplyr::filter(!is.na(WAIT)) %>%
    dplyr::select(DATE,YEAR,MONTH,DAY,WDAY,DAYTYPE,SITE,WAIT,COUNT) %>%
    dplyr::group_by(DATE,YEAR,MONTH,DAY,WDAY,DAYTYPE,SITE) %>%
    dplyr::summarize(WAIT=sum(WAIT),COUNT=sum(COUNT))
  ### Return data.frame
  as.data.frame(d)
}


##
expandPressureCounts <- function(dcnts,cal) {
  dcnts %<>%
    ### Expands observed pressure counts to represent the entire day 
    dplyr::mutate(COUNT=COUNT*iMvDaylen(MONTH)/WAIT) %>%
    ### Computes daily pressure counts (across sites within days)
    dplyr::group_by(DATE,YEAR,MONTH,DAY,WDAY,DAYTYPE) %>%
    dplyr::summarize(WAIT=sum(WAIT),
                     COUNT=sum(COUNT)) %>%
    dplyr::ungroup() %>%
    ### Summarizes daily pressure counts by month and daytype
    dplyr::group_by(YEAR,MONTH,DAYTYPE) %>%
    dplyr::summarize(NCOUNT=n(),
                     VCOUNT=var(COUNT),
                     COUNT=mean(COUNT,na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    ### Expand by number of days in the month (in cal)
    merge(cal[,c("MONTH","DAYTYPE","DAYS")],by=c("MONTH","DAYTYPE")) %>%
    dplyr::mutate(COUNT=COUNT*DAYS,
                  VCOUNT=VCOUNT/NCOUNT*(DAYS^2)) %>%
    dplyr::select(YEAR,DAYTYPE,MONTH,NCOUNT,DAYS,COUNT,VCOUNT) %>%
    dplyr::arrange(YEAR,DAYTYPE,MONTH)
  ## Return data.frame
  as.data.frame(dcnts)
}


## Rearrange fish information from interviews
rearrangeFishInfo <- function(dints) {
  ## Get the main information about the interview
  mainInts <- dplyr::select(dints,INTID:HOURS)
  
  ## Isolate species, clips, and lengths and make long format
  ## Change missing SPECCODEs to actual NAs
  specInts <- dplyr::select(dints,INTID,contains("SPEC")) %>%
    tidyr::gather(tmp,SPECCODE,-INTID) %>%
    dplyr::select(-tmp) %>%
    dplyr::mutate(SPECCODE=ifelse(SPECCODE=="",NA,SPECCODE))
  
  ## Isolate clips, make long, change code to word, add clipped variable
  clipInts <- dplyr::select(dints,INTID,contains("CLIP")) %>%
    tidyr::gather(tmp,CLIPCODE,-INTID) %>%
    dplyr::select(-tmp)
  
  ## Isolate lengths, make long
  lenInts <- dplyr::select(dints,INTID,contains("LEN")) %>%
    tidyr::gather(tmp,LEN,-INTID) %>%
    dplyr::select(-tmp)
  
  ## Put species, clips, and lengths back together
  ## Reduce to only those where a species, clip, or length was recorded (this
  ##   allows for one to be missing ... most commonly a length)
  sclInts <- cbind(specInts,
                   clipInts[,"CLIPCODE",drop=FALSE],
                   lenInts[,"LEN",drop=FALSE]) %>%
    dplyr::filter(!(is.na(SPECCODE) & is.na(CLIPCODE) & is.na(LEN)))
  
  ## Expand records that were only counts of fish (when CLIPCODE==99 or when
  ## CLIPCODE='099' ... extra work is needed here because the CLIPCODE can be
  ## either numeric or character depending on original file type)
  if (is.character(sclInts$CLIPCODE)) {
    if (any(sclInts$CLIPCODE=='099',na.rm=TRUE)) {
      ### isolate records to be expanded
      tmp <- dplyr::filter(sclInts,CLIPCODE=='099')
      ### determine how many of each row to repeat, repeat those rows, 
      ###   replace CLIPCODE with '040' (for not examined), replace LEN with NA
      reprows <- rep(seq_len(nrow(tmp)),tmp$LEN)
      tmp2 <- tmp[reprows,] %>%
        dplyr::mutate(CLIPCODE='040',LEN=NA)
      ### combine originals without records that needed expanding, with expanded
      sclInts <- rbind(dplyr::filter(sclInts,CLIPCODE!='099'),tmp2)
    }
  } else {
    if (any(sclInts$CLIPCODE==99,na.rm=TRUE)) {
      ### isolate records to be expanded
      tmp <- dplyr::filter(sclInts,CLIPCODE==99)
      ### determine how many of each row to repeat, repeat those rows, 
      ###   replace CLIPCODE with 40 (for not examined), replace LEN with NA
      reprows <- rep(seq_len(nrow(tmp)),tmp$LEN)
      tmp2 <- tmp[reprows,] %>%
        dplyr::mutate(CLIPCODE=40,LEN=NA)
      ### combine originals without records that needed expanding, with expanded
      sclInts <- rbind(dplyr::filter(sclInts,CLIPCODE!=99),tmp2)
    }
  }
  
  ## Add words from codes
  sclInts %<>% dplyr::mutate(SPECIES=iMvSpecies(SPECCODE),
                             CLIP=iMvFinclips(CLIPCODE),
                             CLIPPED=iFinclipped(CLIP)) %>%
    dplyr::select(INTID,SPECCODE,SPECIES,CLIPCODE,CLIP,CLIPPED,LEN)
  
  ## Join back on the main interview information
  tmp <- dplyr::right_join(mainInts,sclInts,by="INTID") %>%
    dplyr::arrange(INTID)
  
  ## Return data.frame
  as.data.frame(tmp)
}

## Summarize observed harvest by strata and species
##   HARVEST= Total _observed_ harvest in interviews
##   VHARVEST= Square of HARVEST (in SAS this is uncorrected sum-of-squares)
##   COVAR= Start of a covariance calculation
sumObsHarvest <- function(d) {
  ## Compute harvest for each interview
  harv <- d %>%
    dplyr::group_by(INTID,YEAR,WATERS,STATE,DAYTYPE,
                    FISHERY,MONTH,DATE,HOURS,SPECIES) %>%
    dplyr::summarize(HARVEST=n()) %>%
    dplyr::ungroup()

  ### Separate into only one and split states
  ### All hours and harvest for split states are cut in half
  h1 <- dplyr::filter(harv,!STATE %in% c("WI/MN","WI/MI"))
  h2 <- dplyr::filter(harv,STATE %in% c("WI/MN","WI/MI")) %>%
    dplyr::mutate(HOURS=0.5*HOURS,HARVEST=0.5*HARVEST)
  ### Duplicated h2 to get other half of HOURS/HARVEST, label as Non-WI
  h3 <- dplyr::mutate(h2,WATERS="Non-WI")
  ### Combine to get all interviews corrected for location
  ### Add COVAR variable
  harv <- rbind(h1,h2,h3) %>%
    dplyr::mutate(COVAR=HARVEST*HOURS)
  ### Summarize harvest by strata and species
  harv %<>% dplyr::group_by(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,SPECIES) %>%
    dplyr::summarize(VHARVEST=sum(HARVEST^2),
                     HARVEST=sum(HARVEST),
                     COVAR=sum(COVAR)) %>%
    as.data.frame() %>%
    dplyr::select(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,SPECIES,
                  HARVEST,VHARVEST,COVAR)
  ### Return data.frame
  as.data.frame(harv)
}

## Summarize Harvest and Effort
##   Merge harvest and effort data.frames
##   Replace variables with 0 if NINTS is =0 or NA
##   Calculate the appropriate variances and covariances
##   Note that:
##     NINTS= Number of interviews
##     HARVEST= Total estimated harvest
##     VHARVEST= Variance of total estimated harvest
##     INDHRS= Hours of fishing effort for all individuals
##   Further note that the following are intermediate values & are not returned:
##     HRATE, VHRATE, MHOURS, MHARV
##   Reduce variables and sort
sumHarvestEffort <- function(h,f) {
  hf <- merge(h,f,by=c("YEAR","WATERS","DAYTYPE","FISHERY","MONTH"),all=TRUE) %>%
    dplyr::mutate(HOURS=ifelse(is.na(NINTS) | NINTS==0,NA,HOURS),
                  VHOURS=ifelse(is.na(NINTS) | NINTS==0,NA,VHOURS),
                  VHOURS=(VHOURS-(HOURS^2)/NINTS)/(NINTS-1),
                  HARVEST=ifelse(is.na(NINTS) | NINTS==0,NA,HARVEST),
                  VHARVEST=ifelse(is.na(NINTS) | NINTS==0,NA,VHARVEST),
                  VHARVEST=(VHARVEST-(HARVEST^2)/NINTS)/(NINTS-1),
                  COVAR=ifelse(is.na(NINTS) | NINTS==0,NA,COVAR),
                  COVAR=(COVAR-HARVEST*HOURS/NINTS)/(NINTS-1),
                  MHOURS=HOURS/NINTS,
                  MHARV=HARVEST/NINTS,
                  HRATE=HARVEST/HOURS,
                  VHRATE=ifelse(MHARV==0 & NINTS>1,0,
                                (VHARVEST/(MHARV^2))+(VHOURS/(MHOURS^2))-
                                  2*COVAR/MHARV/MHOURS),
                  VHRATE=(HRATE^2)*VHRATE/NINTS,
                  HARVEST=PHOURS*HRATE,
                  VHARVEST=(PHOURS^2)*VHRATE+(HRATE^2)*VPHOURS+VHRATE*VPHOURS) %>%
    dplyr::select(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,SPECIES,
                  NINTS,HARVEST,VHARVEST,INDHRS) %>%
    dplyr::arrange(YEAR,WATERS,DAYTYPE,FISHERY,MONTH,SPECIES)
  ## Return data.frame
  as.data.frame(hf)
}

## Summarize lengths by species, month, and var
## Note that this is generalized so that var can be either CLIPPED or CLIP
sumLengths <- function(d,var) {
  tmp <- rlang::quo_name(rlang::enquo(var))
  ## summarize by YEAR, SPECIES, MONTH, and "clip"
  lenSum1 <- dplyr::group_by(d,YEAR,SPECIES,MONTH,!!rlang::enquo(var)) %>%
    iSumLen()
  ## summarize by YEAR, SPECIES, and MONTH (across all "clip"s)
  lenSum2 <- dplyr::group_by(d,YEAR,SPECIES,MONTH) %>% iSumLen() %>%
    dplyr::mutate(TMP="All")
  names(lenSum2)[length(names(lenSum2))] <- tmp
  lenSum2 <- dplyr::select(lenSum2,names(lenSum1))
  ## summarize by YEAR, SPECIES, and "clip" (across all MONTHs)
  lenSum3 <- dplyr::group_by(d,YEAR,SPECIES,!!enquo(var)) %>% iSumLen() %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(lenSum1))
  ## summarize by YEAR and SPECIES (across all MONTHs and "clip"s)
  lenSum4 <- dplyr::group_by(d,YEAR,SPECIES) %>% iSumLen() %>%
    dplyr::mutate(MONTH="All",TMP="All")
  names(lenSum4)[length(names(lenSum4))] <- tmp
  lenSum4 <- dplyr::select(lenSum4,names(lenSum1))
  ## put them all together
  lenSum <- rbind(lenSum1,lenSum2,lenSum3,lenSum4) %>%
    mutate(SPECIES=iMvSpecies(SPECIES),
           MONTH=iOrderMonths(MONTH,addAll=TRUE))
  if (tmp=="CLIPPED") {
    lenSum %<>%
      dplyr::mutate(CLIPPED=factor(CLIPPED,levels=c("No Clip","Clip","All"))) %>%
      dplyr::arrange(YEAR,SPECIES,MONTH,CLIPPED)
  } else {
    lenSum %<>% dplyr::arrange(YEAR,SPECIES,MONTH,CLIP)
  }
  ## Remove rows that are duplicates of a previous row for just the summary vars
  lenSum %<>% filter(FSA::repeatedRows2Keep(.,
                      cols2use=c("n","mnLen","seLen","varLen","minLen","maxLen")))
  ## return data.frame
  as.data.frame(lenSum)
}




## Tables ----
tableCaptions <- function() {
  tables <- captioner::captioner(prefix="Table")
  tables(name="Table1",
         caption=paste("Number of days by day type and assumed fishing day",
                       "length (h) for each month during the sampling period.",
                       "These values were used to expand sampled observations",
                       "to entire population."))
  tables(name="Table2",
         caption=paste("Number of interviews (N) and hours of interviewed effort",
                       "(Hrs) by state, day type, type of fishery, and month."))
  tables(name="Table3",
         caption=paste("Number of day sampled and total party-hours of pressure",
                       "month and day type (includes non-fishing effort)."))
  tables(name="Table4",
         caption=paste("Total Party-hours, persons per party, total individual-hours,",
                       "mean trip length, and total number of trips by waters,",
                       "fishery type, month, and day type. 'All' results are",
                       "shown only for those species found in multiple months."))
  tables(name="Table5",
         caption=paste("Total harvest and harvest rates based on fishery-specific",
                       "angling hours by waters, fishery, species, month and",
                       "day type. 'All' results are shown only for those",
                       "species found in multiple months."))
  tables(name="Table6",
         caption=paste("Summary statistics of all measured fish by species,",
                       "month, and whether clipped or not. 'All' results are",
                       "shown only in those months where both clipped and",
                       "unclipped fish were observed."))
  tables(name="Table7",
         caption=paste0("Summary statistics of measured fish by species, month,",
                        " and whether clipped or not. Only species for which some",
                        " fin-clipped fish were recorded are shown (otherwise see ",
                        tables('Table6',display='cite'),"). 'All' results are",
                        " shown only in those months where both clipped and",
                        " unclipped fish were observed."))
  tables(name="Table8",
         caption=paste("Number of fish examined for fin clips by species,",
                       "month, and clip. 'All' results are shown only for those",
                       "species where both clipped and unclipped fish were",
                       "observed."))
  tables(name="Table9",
         caption="Detailed listing of all measured fish.")
  tables
}
table1 <- function(fnpre) {
  ## Prepare data.frame for huxtable
  ##   Read saved file
  ##   Remove the year and creat columns of weekdays and weekends
  ##   Add a TOTAL days column and convert months to a character type (which is
  ##     needed to add the total row later)
  ##   Rearrange and rename some columns
  ##   Add on a total row with a TOTAL label
  calTbl1 <- read.csv(paste0(fnpre,"calSum.csv")) %>%
    dplyr::select(-YEAR) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH)) %>%
    tidyr::spread(DAYTYPE,DAYS) %>%
    dplyr::mutate(All=Weekday+Weekend,
                  MONTH=as.character(MONTH)) %>%
    dplyr::select(MONTH,Weekday,Weekend,All,DAYLEN) %>%
    dplyr::rename(`Length`=DAYLEN) %>%
    dplyr::bind_rows(dplyr::summarize_at(.,vars(Weekday:All),'sum')) %>%
    dplyr::mutate(MONTH=replace(MONTH,is.na(MONTH),"All"))
  names(calTbl1)[which(names(calTbl1)=="All")] <- "All Days"
  ## Make the huxtable
  calTbl2 <- as_hux(calTbl1,add_colnames=TRUE) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    # right align all but leftmost column
    set_align(row=everywhere,col=-1,value="right") %>%
    # Top label covers columns 2-4, centered, line underneath it
    rbind(c("","DAY TYPE","","","Day"),.) %>%
    merge_cells(row=1,col=2:4) %>% 
    set_align(row=1,col=everywhere,value="center") %>%
    set_bottom_border(row=1,col=2,value=1) %>%
    # Extra space above last (TOTAL) row
    set_top_padding(row=final(1),col=everywhere,value=10) %>%
    # Sets table & column widths
    set_width(0.4) %>%
    set_col_width(col=c(.2,.2,.2,.2,.2)) %>%
    iFinishTable(labelsRow=2,labelsCol=1)
  calTbl2
}


table2 <- function(fnrpre) {
  ## Prepare data.frame for huxtable
  ##   Read saved file
  ##   Properly order the MONTHs, FISHERYs, and STATEs
  ##   Drop levels that are not needed
  tmp <- read.csv(paste0(fnpre,"intvdEffortSimple.csv")) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH),
                  FISHERY=iMvFishery(FISHERY),
                  STATE=iMvStates(STATE)) %>%
    droplevels()
  
  ## Get a list of months, including "All"
  mos <- c(levels(tmp$MONTH),"All")
  
  ## Summary table of number and hours of interviews by state, day type, fishery
  ##   and month.
  tmp <- tabular((STATE+1)*(DAYTYPE+1)*(FISHERY+1) ~
                   (MONTH+1)*(NINTS+HOURS)*sum*Format(digits=14),data=tmp)
  tmpTbl1 <- as.matrix(tmp)
  tmpTbl1 <- data.frame(tmpTbl1[-(1:4),],stringsAsFactors=FALSE)
  ## Created better variable names
  names(tmpTbl1) <- c("STATE","DAYTYPE","FISHERY",
                       paste(rep(mos,each=2),
                             rep(c("NINTS","HOURS"),length(mos)),sep="_"))
  ## Converted results back to numeric
  tmpTbl1[,-(1:3)] <- lapply(tmpTbl1[,-(1:3)],as.numeric)
  ## Converted zeroes to NAs (for dashes in huxtable)
  tmpTbl1[sapply(tmpTbl1,is.numeric)] <-
    lapply(tmpTbl1[sapply(tmpTbl1,is.numeric)],round,digits=9)
  tmpTbl1[,-(1:3)][tmpTbl1[,-(1:3)]<0.000000001] <- NA

  ## Rows with "All" (except last) get extra space below
  allRows <- which(tmpTbl1$FISHERY=="All")
  allRows <- allRows[-length(allRows)]
  ## Rows that have a state name (except first) get extra space above
  breakRows <- which(tmpTbl1$STATE!="")
  breakRows <- breakRows[-1]

  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_left_padding(row=everywhere,col=ends_with("NINTS"),value=15) %>%
    set_bottom_padding(row=allRows,col=everywhere,value=10) %>%
    set_top_padding(row=breakRows,col=everywhere,value=15) %>%
    # Convert NAs to dashes
    set_na_string(row=everywhere,col=-(1:3),value="--") %>%
    # No decimals on NINTS or HOURS
    set_number_format(row=everywhere,col=ends_with("NINTS"),value=0) %>%
    set_number_format(row=everywhere,col=ends_with("HOURS"),value=0) %>%
    # Extra label at the top
    rbind(c("STATE","DAY TYPE","FISHERY",rep(c("N","Hrs"),length(mos))),.) %>%
    rbind(c("","","",c(rbind(mos,""))),.) %>%
    # Right align values
    set_align(row=-1,col=-(1:3),value="right") %>%
    # Sets table width
    set_width(0.99) %>%
    iFinishTable(labelsRow=2,labelsCol=3)
  
  ## Creates month labels over N and Sum ... generic for different #s of months
  for (i in 1:length(mos)) {
    tmp <- 2*(i+1)
    tmpTbl2 <- merge_cells(tmpTbl2,1,tmp:(tmp+1)) %>%
#      set_bottom_border(1,tmp,1) %>%
      set_align(1,tmp,"center")
  }
  tmpTbl2
}

table3 <- function(fnpre,which=c("ALT","ORIG")) {
  which <- match.arg(which)
  if (which=="ALT") table3ALT(fnpre)
  else table3ORIG(fnpre)
}

table3ALT <- function(fnpre) {
  ## Prepare data.frame for huxtable
  ##   Read saved file
  ##   Get proper order of months and drop unused levels
  tmp <- read.csv(paste0(fnpre,"pcount.csv")) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH)) %>%
    droplevels()
  
  ## Table of days sampled and total (SD) party hours by month and daytype.
  ##   Removed first two rows (labels will be added back in huxtable),
  ##   renamed columns
  tmpTbl1 <- tabular((MONTH+1)~(DAYTYPE+1)*(NCOUNT+COUNT)*sum*Format(digits=14)+
                       (DAYTYPE+1)*(VCOUNT)*sumsqrt*Format(digits=14),data=tmp)
  tmpTbl1 <- as.matrix(tmpTbl1)
  tmpTbl1 <- tmpTbl1[,c(1,2,3,8,4,5,9,6,7,10)]
  colnames(tmpTbl1) <- c("MONTH",
                         paste("Weekday",c("NCOUNT","COUNT","SDCOUNT"),sep="."),
                         paste("Weekend",c("NCOUNT","COUNT","SDCOUNT"),sep="."),
                         paste("All",c("NCOUNT","COUNT","SDCOUNT"),sep="."))
  tmpTbl1 <- as.data.frame(tmpTbl1[-(1:4),],stringsAsFactors=FALSE)
  
  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_top_padding(row=final(1),col=everywhere,value=10) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_left_padding(row=everywhere,col=ends_with("NCOUNT"),value=15) %>%
    # No decimals on NCOUNT and COUNT
    set_number_format(row=everywhere,col=ends_with("NCOUNT"),value=0) %>%
    set_number_format(row=everywhere,col=ends_with("COUNT"),value=0) %>%
    # One decimal on SD
    set_number_format(row=everywhere,col=ends_with("SDCOUNT"),value=1) %>%
    rbind(c("MONTH",rep(c("Sampled","Total","SD"),3)),.) %>%
    rbind(c("",rep(c("Days","Party Hours",""),3)),.) %>%
    rbind(c("","Weekday","","","Weekend","","","All Days","",""),.) %>%
    merge_cells(row=1,col=2:4) %>%
    merge_cells(row=1,col=5:7) %>%
    merge_cells(row=1,col=8:10) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    merge_cells(row=2,col=3:4) %>%
    merge_cells(row=2,col=6:7) %>%
    merge_cells(row=2,col=9:10) %>%
    set_bottom_border(row=2,col=c(3,6,9),value=1) %>%
    set_align(row=2,col=everywhere,value="center") %>%
    set_align(row=3,col=c(4,5,7,8,9,10),value="center") %>%
    # Right align values for all but first three rows and first column
    set_align(row=-(1:3),col=-1,value="right") %>%
    set_width(0.5) %>%
    iFinishTable(labelsRow=3,labelsCol=1)
  tmpTbl2
}

## This one more closely matches SAS look
table3ORIG <- function(fnpre) {
  ## Prepare data.frame for huxtable
  ##   Read saved file
  ##   Get proper order of months and drop unused levels
  tmp <- read.csv(paste0(fnpre,"pcount.csv")) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH)) %>%
    droplevels()
  
  ## Table of days sampled and total (SD) party hours by month and daytype.
  ##   Removed first two rows (labels will be added back in huxtable),
  ##   renamed columns
  tmpTbl1 <- as.matrix(tabular((MONTH+1)*(DAYTYPE+1)~(NCOUNT+COUNT)*sum+
                                 (VCOUNT)*sumsqrt,data=tmp))
  tmpTbl1 <- as.data.frame(tmpTbl1[-c(1,2),],stringsAsFactors=FALSE)
  names(tmpTbl1) <- c("MONTH","DAYTYPE","NCOUNT","COUNT","SDCOUNT")
  
  ## Find which rows to shade as a sum
  ## Rows with "All" (except last) get extra space below
  allRows <- which(tmpTbl1$DAYTYPE=="All")
  allRows <- allRows[-length(allRows)]
  
  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_bottom_padding(row=allRows,col=everywhere,15) %>%
    # No decimals on NCOUNT and COUNT
    set_number_format(row=everywhere,col=c("NCOUNT","COUNT"),value=0) %>%
    # One decimal on SD
    set_number_format(row=everywhere,col=c("SDCOUNT"),value=1) %>%
    rbind(c("MONTH","DAY TYPE","Sampled","Total","St. Dev."),.) %>%
    rbind(c("","","Days","Party Hours",""),.) %>%
    merge_cells(row=1,col=4:5) %>%
    set_bottom_border(row=1,col=4,value=1) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    # Right align values for all but first row and first two columns
    set_align(row=-1,col=-(1:2),value="right") %>%
    set_width(0.25) %>%
    iFinishTable(labelsRow=2,labelsCol=2)
  tmpTbl2
}

table4 <- function(fnpre) {
  ## Prepare data.frame for huxtable
  ##   Read saved file
  ##   Make proper orders of MONTHs, WATERS, and FISHERYs
  ##   drop unused levels
  tmp <- read.csv(paste0(fnpre,"ttlEffort.csv")) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH),
                  WATERS=factor(WATERS,levels=c("WI","NON-WI")),
                  FISHERY=iMvFishery(FISHERY)) %>%
    droplevels()
  
  ## Table of days sampled and total (SD) party hours by month and daytype.
  ##   Must convert "All" words to "TOTAL", removed the first two rows (labels
  ##   that will be added back with hustable), renamed columns
  tmpTbl1 <- tabular((WATERS)*(FISHERY)*(MONTH+1)*(DAYTYPE+1)*DropEmpty(which="row")~
                       (NINTS+HOURS+TRIPS+INDHRS+PHOURS)*sum*Format(digits=14)+
                       (VHOURS+VTRIPS+VINDHRS+VPHOURS)*sumsqrt*Format(digits=14),
                     data=tmp)
  tmpTbl1 <- as.matrix(tmpTbl1)
  colnames(tmpTbl1) <- c("WATERS","FISHERY","MONTH","DAYTYPE",
                         tmpTbl1[1,5:13])
  tmpTbl1 <- tmpTbl1[-(1:2),]
  tmpTbl1 <- as.data.frame(tmpTbl1,stringsAsFactors=FALSE)
  tmpTbl1[,5:13] <- lapply(tmpTbl1[,5:13],as.numeric)
  tmpTbl1 <- tmpTbl1 %>%
    mutate(PARTY=INDHRS/PHOURS,
           MTRIP=PHOURS/TRIPS) %>%
    rename(SDHOURS=VHOURS,SDTRIPS=VTRIPS,SDINDHRS=VINDHRS,SDPHOURS=VPHOURS) %>%
    select(WATERS:DAYTYPE,PHOURS,SDPHOURS,PARTY,INDHRS,SDINDHRS,MTRIP,TRIPS,SDTRIPS)
  tmpTbl1[is.na(tmpTbl1)] <- NA
  tmpTbl1[,-(1:4)][tmpTbl1[,-(1:4)]<0.00000001] <- NA
  tmpTbl1 %<>% dplyr::filter(FSA::repeatedRows2Keep(.,
                              cols2ignore=c("WATERS","FISHERY","MONTH","DAYTYPE")))
  
  ## Rows with a MONTH (except first) get extra space above
  breakRows1 <- which(tmpTbl1$MONTH!="")
  breakRows1 <- breakRows1[-1]
  ## Rows that have a fishery name (except first) get extra space above
  breakRows2 <- which(tmpTbl1$FISHERY!="")
  breakRows2 <- breakRows2[-1]
  
  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=breakRows1,col=everywhere,value=10) %>%
    set_top_padding(row=breakRows2,col=everywhere,value=15) %>%
    # Change all NAs to dashes
    set_na_string(row=everywhere,col=-(1:4),value="--") %>%
    # Round these to columns
    set_number_format(row=everywhere,col=c("PHOURS","INDHRS","TRIPS"),value=0) %>%
    set_number_format(row=everywhere,col=c("SDPHOURS","SDINDHRS","SDTRIPS"),
                      value=1) %>%
    set_number_format(row=everywhere,col=c("PARTY","MTRIP"),value=2) %>%
    # Creating column headers
    rbind(c("","","","","","","Persons","","","Mean","",""),
          c("","","","","Party Hours","","per","Ind. Hours","","Trip",
            "Trips",""),
          c("WATERS","FISHERY","MONTH","DAY TYPE","Total","SD","Party",
            "Total","SD","Length","Total","SD"),
          .) %>%
    merge_cells(row=2,col=5:6) %>%
    set_bottom_border(row=2,col=5,value=1) %>%
    merge_cells(row=2,col=8:9) %>%
    set_bottom_border(row=2,col=8,value=1) %>%
    merge_cells(row=2,col=11:12) %>%
    set_bottom_border(row=2,col=11,value=1) %>%
    set_align(row=2,col=everywhere,value="center") %>%
    set_align(row=2,col=c(7,10),value="right") %>%
    set_align(row=-(1:2),col=-(1:4),value="right") %>%
    set_width(0.7) %>%
    iFinishTable(labelsRow=3,labelsCol=4)
  tmpTbl2
}

table5 <- function(fnpre) {
  ## Prepare data.frame for huxtable
  ##   Read saved file
  ##   Make proper order of MONTHs, WATERS, FISHERYs, and SPECIES
  ##   Drop unused levels
  tmp <- read.csv(paste0(fnpre,"ttlHarvest.csv")) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH),
                  WATERS=factor(WATERS,levels=c("WI","NON-WI")),
                  FISHERY=iMvFishery(FISHERY),
                  SPECIES=iMvSpecies(SPECIES)) %>%
    droplevels()

  ## Table of days sampled and total (SD) party hours by month and daytype.
  ##   Must convert "All" words to "TOTAL", removed the first two rows (labels
  ##   that will be added back with huxtable), renamed columns
  tmpTbl1 <- tabular((WATERS)*(FISHERY+1)*(SPECIES)*(MONTH+1)*DropEmpty(which="row")~
                       (DAYTYPE+1)*(HARVEST+INDHRS)*sum*Format(digits=14)+
                       (DAYTYPE+1)*(VHARVEST)*sumsqrt*Format(digits=14),
                     data=tmp)
  tmpTbl1 <- as.matrix(tmpTbl1)
  colnames(tmpTbl1) <- c("WATERS","FISHERY","SPECIES","MONTH",
                         paste0("WEEKDAY",tmpTbl1[3,5:6]),
                         paste0("WEEKEND",tmpTbl1[3,7:8]),
                         paste0("TOTAL",tmpTbl1[3,9:10]),
                         paste0("WEEKDAY",tmpTbl1[3,11]),
                         paste0("WEEKEND",tmpTbl1[3,12]),
                         paste0("TOTAL",tmpTbl1[3,13]))
  tmpTbl1 <- tmpTbl1[-(1:4),]
  tmpTbl1 <- as.data.frame(tmpTbl1,stringsAsFactors=FALSE)
  tmpTbl1[,5:13] <- lapply(tmpTbl1[,5:13],as.numeric)
  tmpTbl1 <- tmpTbl1 %>%
    mutate(WEEKDAYHRATE=WEEKDAYHARVEST/WEEKDAYINDHRS,
           WEEKENDHRATE=WEEKENDHARVEST/WEEKENDINDHRS,
           TOTALHRATE=TOTALHARVEST/TOTALINDHRS) %>%
    rename(WEEKDAYSDHARVEST=WEEKDAYVHARVEST,
           WEEKENDSDHARVEST=WEEKENDVHARVEST,
           TOTALSDHARVEST=TOTALVHARVEST,) %>%
    select(WATERS:MONTH,WEEKDAYHARVEST,WEEKDAYSDHARVEST,WEEKDAYHRATE,
           WEEKENDHARVEST,WEEKENDSDHARVEST,WEEKENDHRATE,
           TOTALHARVEST,TOTALSDHARVEST,TOTALHRATE)
  # Hack needed to deal with NaN results
  tmpTbl1[is.na(tmpTbl1)] <- NA
  # Set zero harvests and its SD to NA (for huxtable)
  zeroHarvs <- tmpTbl1$WEEKDAYHARVEST==0
  tmpTbl1$WEEKDAYHARVEST[zeroHarvs] <- NA
  tmpTbl1$WEEKDAYSDHARVEST[zeroHarvs] <- NA
  zeroHarvs <- tmpTbl1$WEEKENDHARVEST==0
  tmpTbl1$WEEKENDHARVEST[zeroHarvs] <- NA
  tmpTbl1$WEEKENDSDHARVEST[zeroHarvs] <- NA
  # Remove rows that are repeats of the row above it (for the numeric variables)
  tmpTbl1 %<>% dplyr::filter(FSA::repeatedRows2Keep(.,
                             cols2ignore=c("WATERS","FISHERY","SPECIES","MONTH")))
  
  ## Rows with a SPECIES name (except first) get extra space above
  breakRows1 <- which(tmpTbl1$SPECIES!="")
  breakRows1 <- breakRows1[-1]
  ## Rows that have a fishery name (except first) get extra space above
  breakRows2 <- which(tmpTbl1$FISHERY!="")
  breakRows2 <- breakRows2[-1]

  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=breakRows1,col=everywhere,value=10) %>%
    set_top_padding(row=breakRows2,col=everywhere,value=25) %>%
    # Convert NAs to dashes
    set_na_string(row=everywhere,col=-(1:4),value="--") %>%
    # Set decimals
    set_number_format(row=everywhere,
                      col=c("WEEKDAYHARVEST","WEEKENDHARVEST","TOTALHARVEST"),
                      value=0) %>%
    set_number_format(row=everywhere,
                      col=c("WEEKDAYSDHARVEST","WEEKENDSDHARVEST","TOTALSDHARVEST"),
                      value=1) %>%
    set_number_format(row=everywhere,
                      col=c("WEEKDAYHRATE","WEEKENDHRATE","TOTALHRATE"),
                      value=4) %>%
    # Create nice column headers
    rbind(c("","","","","Weekday","","","Weekend","","","ALL DAYS","",""),
          c("","","","","Harvest","","Harvest/","Harvest","","Harvest/",
            "Harvest","","Harvest/"),
          c("WATERS","FISHERY","SPECIES","MONTH","Number","SD","Angler-Hr",
            "Number","SD","Angler-Hr","Number","SD","Angler-Hr"),
          .) %>%
    merge_cells(row=1,col=5:7) %>%
    merge_cells(row=1,col=8:10) %>%
    merge_cells(row=1,col=11:13) %>%
    merge_cells(row=2,col=5:6) %>%
    merge_cells(row=2,col=8:9) %>%
    merge_cells(row=2,col=11:12) %>%
    set_bottom_border(row=1:2,col=c(5,8,11),value=1) %>%
    set_align(row=1:2,col=everywhere,value="center") %>%
    set_align(row=-(1:2),col=-(1:2),value="right") %>%
    set_width(0.7) %>%
    iFinishTable(labelsRow=3,labelsCol=4)
  tmpTbl2
}


table6 <- function(fnpre) {
  ## Prepare data.frame for huxtable
  ##   Read saved file
  ##   Remove fish for which a length was not recorded
  ##   Summarize lengths by whether clipped or not
  ##   Set proper order of MONTH, SPECIES, and CLIPPED
  ##   Arrange for table
  ##   Remove duplicate SPECIES and MONTH labels
  ##   Remove the YEAR variable
  tmp <- read.csv(paste0(fnpre,"lengths.csv")) %>%
    filter(!is.na(LEN)) %>%
    sumLengths(CLIPPED) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH,addAll=TRUE),
                  SPECIES=iMvSpecies(SPECIES),
                  CLIPPED=factor(CLIPPED,
                                 levels=c("No Clip","Clip","All"))) %>%
    arrange(SPECIES,MONTH,CLIPPED) %>%
    droplevels() %>%
    dplyr::mutate(SPECIES=ifelse(iRepeatsPrevItem(SPECIES),"",
                                 levels(SPECIES)[SPECIES]),
                  MONTH=ifelse(iRepeatsPrevItem(MONTH),"",levels(MONTH)[MONTH])
    ) %>%
    select(-YEAR)

  ## Add some more space above where SPECIES & MONTY name is (except for first)
  breakRows1 <- which(tmp$MONTH!="")
  breakRows1 <- breakRows1[-1]
  breakRows2 <- which(tmp$SPECIES!="")
  breakRows2 <- breakRows2[-1]
  
  ## Make the huxtable
  tmpTbl2 <- as_hux(tmp) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=breakRows1,col=everywhere,10) %>%
    set_top_padding(row=breakRows2,col=everywhere,25) %>%
    # Change NAs to dashes
    set_na_string(row=everywhere,col=-(1:4),value="--") %>%
    # Round n to 0; mean, SE, and VAR to 2; and Min and Max to 1 decimal
    set_number_format(row=everywhere,col="n",0) %>%
    set_number_format(row=everywhere,col=c("mnLen","seLen","varLen"),2) %>%
    set_number_format(row=everywhere,col=c("minLen","maxLen"),1) %>%
    # Add column labels
    rbind(c("","","","Length (in.)","","","","",""),
          c("SPECIES","MONTH","Clipped?","N","Mean","SE","Var","Min","Max"),.) %>%
    # Top label should extend across 4-9 columns with line underneath
    merge_cells(row=1,col=4:9) %>%
    set_bottom_border(row=1,col=4,1) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    set_width(0.5) %>%
    iFinishTable(labelsRow=2,labelsCol=3)
  tmpTbl2
}


table7 <- function(fnpre) {
  ## Prepare data.frame for huxtable
  ##   Read saved file
  ##   Remove fish for which a length was not recorded
  tmp <- read.csv(paste0(fnpre,"lengths.csv")) %>%
    filter(!is.na(LEN))
  ## Find only those species for which a fin-slip was recorded
  specClipped <- as.character(unique(dplyr::filter(lengths,CLIPPED=="Clip")$SPECIES))
  ##   Summarize lengths by whether clipped or not
  ##   Set proper order of MONTH, SPECIES, and CLIPPED
  ##   Arrange for table
  ##   Remove duplicate SPECIES and MONTH labels
  ##   Remove the YEAR variable
  tmp <- sumLengths(filter(tmp,SPECIES %in% specClipped),CLIP) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH,addAll=TRUE),
                  SPECIES=iMvSpecies(SPECIES),
                  CLIP=iMvFinclips(CLIP)) %>%
    arrange(SPECIES,MONTH,CLIP) %>%
    droplevels() %>%
    dplyr::mutate(CLIP=ifelse(is.na(CLIP),"All",as.character(CLIP)),
                  SPECIES=ifelse(iRepeatsPrevItem(SPECIES),"",
                                 levels(SPECIES)[SPECIES]),
                  MONTH=ifelse(iRepeatsPrevItem(MONTH),"",levels(MONTH)[MONTH])
    ) %>%
    select(-YEAR)

  ## Which rows to add some more space above
  breakRows1 <- which(tmp$MONTH!="")
  breakRows1 <- breakRows1[-1]
  breakRows2 <- which(tmp$SPECIES!="")
  breakRows2 <- breakRows2[-1]

  ## Make the huxtable
  tmpTbl2 <- as_hux(tmp) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=breakRows1,col=everywhere,value=10) %>%
    set_top_padding(row=breakRows2,col=everywhere,value=25) %>%
    # Change NAs to dashes
    set_na_string(row=everywhere,col=-(1:4),value="--") %>%
    # Round n to 0; mean, SE, and VAR to 2; and Min and Max to 1 decimal
    set_number_format(row=everywhere,col="n",value=0) %>%
    set_number_format(row=everywhere,col=c("mnLen","seLen","varLen"),value=2) %>%
    set_number_format(row=everywhere,col=c("minLen","maxLen"),1) %>%
    # Add column labels
    rbind(c("","","","Length (in.)","","","","",""),
          c("SPECIES","MONTH","Clip","N","Mean","SE","Var","Min","Max"),.) %>%
    # Top label should extend across 4-9 columns with line underneath
    merge_cells(row=1,col=4:9) %>%
    set_bottom_border(row=1,col=4,value=1) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    set_width(0.4) %>%
    iFinishTable(labelsRow=2,labelsCol=3)
  tmpTbl2
}


table8 <- function(fnpre) {
  ## Prepare data.frame for huxtable
  ##   Read saved file
  ##   Remove fish that were not checked for clips
  ##   Make proper order of MONTHs, SPECIES and CLIPs
  ##   drop unused levels
  tmp <- read.csv(paste0(fnpre,"lengths.csv")) %>%
    filter(!is.na(CLIP)) %>%
    dplyr::mutate(MONTH=iOrderMonths(MONTH),
                  SPECIES=iMvSpecies(SPECIES),
                  CLIP=iMvFinclips(CLIP)) %>%
    droplevels()
  
  ## Make table
  tmpTbl1 <- tabular((SPECIES)*(CLIP+1)*DropEmpty(which="row")~
                       (MONTH+1)*(LEN)*(n=1),data=tmp)
  tmpTbl1 <- as.matrix(tmpTbl1)
  colnames(tmpTbl1) <- c(tmpTbl1[4,1:2],tmpTbl1[2,3:ncol(tmpTbl1)])
  tmpTbl1 <- as.data.frame(tmpTbl1[-(1:4),])
  # Remove rows that are repeats of the row above it (for the numeric variables)
  tmpTbl1 %<>% dplyr::filter(FSA::repeatedRows2Keep(.,
                              cols2ignore=c("SPECIES","CLIP")))
  
  ## Find rows that need more space
  breakRows <- which(tmpTbl1$SPECIES!="")
  breakRows <- breakRows[-1]
  ## Find number of months
  mos <- ncol(tmpTbl1)-3

  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=breakRows,col=everywhere,value=15) %>%
    # Add column labels
    rbind(c("","","MONTH",rep("",mos)),
          names(tmpTbl1),.) %>%
    # Top label should extend across MONTHs columns with line underneath
    merge_cells(row=1,col=3:(mos+2+1)) %>%
    set_bottom_border(row=1,col=3,value=1) %>%
    set_align(row=everywhere,col=3:(mos+2+1),value="right") %>%
    set_align(row=1,col=everywhere,value="center") %>%
    set_width(0.35) %>%
    iFinishTable(labelsRow=2,labelsCol=2)
  tmpTbl2
}


table9 <- function(fnpre) {
  ## Prepare data.frame for huxtable
  ##   Read saved file
  tmp <- read.csv(paste0(fnpre,"lengths.csv")) %>%
    dplyr::mutate(FISHERY=iMvFishery(FISHERY),
                  SPECIES=iMvSpecies(SPECIES),
                  CLIP=iMvFinclips(CLIP),
                  STATE=factor(STATE,levels=c("WI","WI/MN","WI/MI","MN","MI"))) %>%
    dplyr::select(SPECIES,STATE,SITE,FISHERY,DATE,LEN,CLIP) %>%
    dplyr::arrange(SPECIES,DATE,LEN) %>%
    droplevels() %>%
    dplyr::mutate(SPECIES=ifelse(duplicated(SPECIES),"",levels(SPECIES)[SPECIES]))
  ## Find rows for extra space above
  breakRows <- which(tmp$SPECIES!="")
  breakRows <- breakRows[-1]
  ## Make huxtable
  tmpTbl2 <- as_hux(tmp) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_top_padding(row=breakRows,col=everywhere,15) %>%
    # Put extra spacing between columns
    set_left_padding(row=everywhere,col=everywhere,15) %>%
    # Round lengths to one decimal (what they were recorded in)
    set_number_format(row=everywhere,col="LEN",1) %>%
    # Add column labels
    rbind(c("SPECIES","STATE","SITE","FISHERY","DATE","LENGTH (in)","Clip"),.) %>%
    set_width(0.5) %>%
    iFinishTable(labelsRow=1,labelsCol=1)
  tmpTbl2
}


## Figures ----
figureCaptions <- function() {
  figures <- captioner::captioner(prefix="Figure")
  figures(name="Figure9",
          caption=paste("Density plot of total length by month for the three",
                        "most commonly measured fish. Sample size is for",
                        "combined clipped and not clipped fish."))
  figures
}

figure9 <- function(fnpre,topN=3) {
  # Turn warnings off (turned back on below)
  options(warn=-1)
  # Read and prepare lengths data file
  tmp <- read.csv(paste0(fnpre,"lengths.csv")) %>%
    dplyr::mutate(SPECIES=iMvSpecies(SPECIES),
                  MONTH=iOrderMonths(MONTH),
                  CLIPPED=factor(CLIPPED,levels=c("No Clip","Clip")))
  # Sample size by species and month
  SUM <- dplyr::group_by(tmp,SPECIES,MONTH) %>%
    dplyr::summarize(n=dplyr::n())
  # Find topN species by sample size
  TOPN <- dplyr::summarize(SUM,n=sum(n)) %>%
    dplyr::top_n(topN,n)
  # Reduce summaries to the topN species
  #   add dummy CLIPPED variable for plotting
  SUM %<>% filter(SPECIES %in% TOPN$SPECIES) %>%
    mutate(CLIPPED=NA)
  # Reduce original data.frame to the topN species
  tmp %<>% filter(SPECIES %in% TOPN$SPECIES)
  # Make the plot
  p <- ggplot(data=tmp,aes(x=LEN,fill=CLIPPED)) +
    geom_histogram(alpha=0.6) +
    xlab("Length (Inches)") +
    ylab("Relative Frequency") +
    #  scale_fill_brewer(palette="Dark2") +
    facet_grid(MONTH~SPECIES,scales='free') +
    geom_text(data=SUM,aes(x=Inf,y=Inf,label=paste0("n=",n)),
              size=5,vjust=1.1,hjust=1.1) +
    theme_bw(base_size=18) +
    theme(
      axis.text.y.left = element_blank(),
      legend.position="bottom",
      legend.title=element_blank(),
      legend.text=element_text(size=rel(0.75)),
      legend.spacing.x=unit(3,"mm")
    )
  options(warn=0)
  p
}


## Internals for Mains ----
## Make filename prefix
fnPrefix <- function(rdir,LOC,SDATE) {
  paste0(rdir,"/",lubridate::year(SDATE),"_",iMvLoc(LOC),"_")
}

## Convenience function for making a file of the data.frame in x
writeDF <- function(x,fnpre) {
  x1 <- deparse(substitute(x))
  write.csv(x,file=paste0(fnpre,x1,".csv"),row.names=FALSE,quote=FALSE)
}


## Creates day lengths from month names
iMvDaylen <- function(x) {
  mos <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  lens <- c(0,0,0,0,16,16,16,16,16,0,0,0)
  x <- plyr::mapvalues(x,from=mos,to=lens,warn=FALSE)
  FSA::fact2num(x)
}

## Convert DOW to weekend or weekdays ... with holidays as weekends
iMvDaytype <- function(wd,mon,md) {
  wd2 <- plyr::mapvalues(wd,from=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                         to=c("Weekday","Weekday","Weekday","Weekday","Weekday",
                              "Weekend","Weekend"),warn=FALSE)
  wd2 <- iHndlHolidays(mon,md,wd,wd2)
  wd2 <- factor(wd2,levels=c("Weekday","Weekend"))
  wd2  
}

## Make New Years, Memorial Day, July 4th, and Labor Day as WEEKENDS
iHndlHolidays <- function(mon,md,wd,dt) {
  dplyr::case_when(
    mon=="Jan" & md==1 ~ "Weekend",              # New Years Day
    mon=="May" & md>=25 & wd=="Mon" ~ "Weekend", # Memorial Day
    mon=="Jul" & md==4 ~ "Weekend",              # 4th of July
    mon=="Sep" & md<=7 & wd=="Mon" ~ "Weekend",  # Labor Day
    TRUE ~ as.character(dt)
  )
}


## Convert state codes to words
iMvStates <- function(x) {
  tmp <- c("WI","MN","MI","WI/MN","WI/MI")
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=1:5,to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x
}

## Create "waters" variable to identify if the fished area was in WI or not
iMvWaters <- function(x) {
  x <- dplyr::case_when(
    x=="MN" ~ "Non-WI",
    x=="MI" ~ "Non-WI",
    TRUE ~ "WI"
  )
  factor(x,levels=c("WI","Non-WI"))
}

## Convert "fishery" codes to words
##   left "old" in just in case return to legacy names
iMvFishery <- function(x) {
  old <- c("COLDWATER-OPEN","WARMWATER-OPEN","ICE-STREAM MOUTH","ICE-WARMWATER",
           "ICE-BOBBING","BAD RIVER","NON-FISHING","SHORE","TRIBAL","COMBINED")
  tmp <- c("Cold-Open","Warm-Open","Ice-Mouth","Ice-Warm","Ice-Bob",
           "Bad R.","Non-Fishing","Shore","Tribal","Combined")
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=1:10,to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x
}

## Convert status codes to words
iMvStatus <- function(x) {
  tmp <- c("Complete","Incomplete")
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=1:2,to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x
}

## Convert residency codes to words
##   left "old" in just in case return to legacy names
iMvResidency <- function(x) {
  old <- c("Resident","Non-Resident","Resident/Non-Resident")
  tmp <- c("Res","Non-Res","Res/Non-Res")
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=1:3,to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp)
  x
}

## Compute hours of effort, put NAs if before start or after end of survey
##   period or if stop time is before start time.
iHndlHours <- function(STARTHH,STARTMM,STOPHH,STOPMM,DATE,SDATE,FDATE) {
  START <- STARTHH*60+STARTMM
  STOP <- STOPHH*60+STOPMM
  dplyr::case_when(
    DATE < SDATE ~ NA_real_,   # Date before start date
    DATE > FDATE ~ NA_real_,   # Date after end date
    STOP < START ~ NA_real_,   # Stopped before started
    TRUE ~ (STOP-START)/60     # OK ... calc hours of effort
  )
}



## Convert fish species codes to words ... note possibly three sets of codes
iMvSpecies <- function(x) {
  if (is.numeric(x)) code <- c(2,3,4,5,6,7,11,13,33,34,35,36,43,45,46,47,48,49,
                               51,52,60,62,66,67,70,71,76,78,98,99)
  else {
    tmp <- substr(x,1,1)
    if (any(tmp %in% c("W","M","I","B","R","X")))
      code <- c('W09','W00','W02','W14','I22','W06','R01','M12','I04','I24',
                'I23','I28','L02','I21','I19','W04','I16','I14','I12','I20',
                'J01','B01','W12','W11','I05','I18','X15','X22','098','099')
    else code <- c('002','003','004','005','006','007','011','013','033','034',
                   '035','036','043','045','046','047','048','049','051','052',
                   '060','062','066','067','070','071','076','078','098','099')
  }
  nms <- c('BLUEGILL','SUNFISH SPP.','CRAPPIE SPP.','BLACK CRAPPIE',
           'BROOK TROUT','PUMPKINSEED','BURBOT','CARP','LAKE HERRING',
           'SISCOWET','LAKE TROUT','SPLAKE','NORTHERN PIKE','BROWN TROUT',
           'RAINBOW TROUT','ROCK BASS','CHINOOK','COHO','PINK SALMON',
           'ATLANTIC SALMON','SMELT','STURGEON','LARGEMOUTH BASS',
           'SMALLMOUTH BASS','LAKE WHITEFISH','ROUND WHITEFISH','YELLOW PERCH',
           'WALLEYE','CATFISH','NA')
  lvls <- c('LAKE TROUT','SISCOWET','ATLANTIC SALMON','BROOK TROUT',
            'BROWN TROUT','COHO','CHINOOK','PINK SALMON','RAINBOW TROUT',
            'SPLAKE','BURBOT','LAKE HERRING','LAKE WHITEFISH','ROUND WHITEFISH',
            'SMELT','STURGEON','WALLEYE','YELLOW PERCH','NORTHERN PIKE',
            'BLUEGILL','PUMPKINSEED','SUNFISH SPP.','BLACK CRAPPIE',
            'CRAPPIE SPP.','LARGEMOUTH BASS','ROCK BASS','SMALLMOUTH BASS',
            'CATFISH','CARP')
  nms <- FSA::capFirst(nms)
  nms[length(nms)] <- NA
  lvls <- FSA::capFirst(lvls)
  if (!any(x %in% nms)) x <- plyr::mapvalues(x,from=code,to=nms,warn=FALSE)
  x <- factor(x,levels=lvls)
  x
}

## Convert fin-clip codes to words
##   99 is mapped to 40 as this happens after 99 is expanded to multiple lengths
iMvFinclips <- function(x) {
  tmp <- c('00: NONE','01: AD','02: AD+LV','03: AD+RV','04: AD+LP','05: AD+RP',
           '06: LV','07: RV','08: LP','09: RP','10: LV+RV','11: RP+LV',
           '12: AD+LV+RV','13: D','14: HATCHERY','15: LP+RV','16: D+RV',
           '17: D+RP','18: AD+RM','19: LP+RM','20: LP+LV','21: D+AD',
           '22: D+LV+RV','23: D+LP','24: D+LV','40: NOT EXAMINED',
           '40: NOT EXAMINED',NA)
  if (is.numeric(x)) x <- plyr::mapvalues(x,from=c(0:24,40,99,NA),
                                          to=tmp,warn=FALSE)
  x <- factor(x,levels=tmp[!duplicated(tmp)])
  x  
}

## Adds whether a fish was clipped or not
iFinclipped <- function(x) {
  tmp <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c('00: NONE','40: NOT EXAMINED') ~ "No Clip",
    TRUE ~ "Clip"
  )
  factor(tmp,levels=c("Clip","No Clip"))
}





## Create long name from abbreviated location name
iMvLoc <- function(x) {
  codes <- c("ash","byf","cpw","lsb","rdc","sax","sup","wsh")
  names <- c("Ashland","Bayfield","Corny-Port Wing","Little Sand Bay",
             "Red Cliff","Saxon","Superior","Washburn")
  plyr::mapvalues(x,from=codes,to=names,warn=FALSE)
}

## Converts months to an ordered factor
iOrderMonths <- function(x,addAll=FALSE) {
  if (addAll) ordered(x,levels=c(format(ISOdate(2004,1:12,1),"%b"),"All"))
  else ordered(x,levels=format(ISOdate(2004,1:12,1),"%b"))
}


iSumLen <- function(dgb) {
  dplyr::summarize(dgb,n=n(),mnLen=mean(LEN,na.rm=TRUE),
                   seLen=FSA::se(LEN,na.rm=TRUE),varLen=var(LEN,na.rm=TRUE),
                   minLen=min(LEN,na.rm=TRUE),maxLen=max(LEN,na.rm=TRUE)) %>%
    as.data.frame()
}


## Converts missing values to zeroes
iConvNA20 <- function(x) {
  dplyr::case_when(
    is.na(x) ~ 0,
    TRUE ~ x
  )
}


## Add final characteristics to all tables
iFinishTable <- function(h,labelsRow,labelsCol) {
  h <- h %>%
    # Line above top and below bottom of table
    set_top_border(row=1,col=everywhere,2) %>%
    set_bottom_border(row=final(),col=everywhere,2) %>%
    # Line below variable names
    set_bottom_border(row=labelsRow,col=everywhere,1) %>%
    # Bold rows and columns of labels
    set_bold(row=1:labelsRow,col=everywhere,TRUE) %>%
    set_bold(row=everywhere,col=1:labelsCol,TRUE)
  h
}

## write table out to file
iWriteTable <- function(h,fnpre,TABLE) {
  huxtable::quick_html(h,file=paste0(fnpre,"Table",TABLE,".html"))
}

## Determines if previous item in a vector is repeated in current item
iRepeatsPrevItem <- function(x) {
  if (is.character(x)) x <- factor(x)
  if (is.factor(x)) x <- as.numeric(x)
  c(FALSE,diff(x)==0)
}

## Function to take the square root of a sum ... used for computing the SD
##   from summed variances in the tables.
sumsqrt <- function(x) sqrt(sum(x,na.rm=TRUE))
