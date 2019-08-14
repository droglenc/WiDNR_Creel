#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Load Packages ---------------------------------------------------------------
rqrd <- c("FSA","tidyr","dplyr","huxtable","captioner","knitr","here","readxl")
for (i in seq_along(rqrd))
  suppressPackageStartupMessages(library(rqrd[i],character.only=TRUE))



## Main Helpers ----------------------------------------------------------------
# Read total number of fishable days per month at each site
readFishableDays <- function(RDIR,LOC) {
  readxl::read_excel(file.path(RDIR,"qry_ice_fdays_4R.xlsx")) %>%
    dplyr::filter(ROUTE==LOC) %>%
    dplyr::rename_all(.funs=toupper) %>%
    dplyr::rename(ttlDays=FDAYS) %>%
    dplyr::mutate(MONTH=iHndlMonth(MONTH),
                  DAYTYPE=iHndlDaytype(DAYTYPE),
                  SITE=iHndlSiteDesc(SITE,SITEDESC)) %>%
    dplyr::select(-FDAYSKEY,-SITEDESC,-COMMENTS) %>%
    as.data.frame()
}

#
readPressureCountData_ICE <- function(RDIR,LOC,FD) {
  # Read observed counts and finds total number of vehicles at each site and day
  cnts <- readxl::read_excel(file.path(RDIR,"qry_ice_counts_4R.xlsx")) %>%
    dplyr::filter(ROUTE==LOC) %>%
    dplyr::rename_all(.funs=toupper) %>%
    dplyr::mutate(MONTH=iHndlMonth(MONTH),
                  DAYTYPE=iHndlDaytype(DAYTYPE),
                  SITE=iHndlSiteDesc(SITE,SITEDESC),
                  ## Sum all vehicle counts (treat NAs as 0s so they will sum)
                  ALLVEH=iNA2zero(MORNVEHR)+iNA2zero(EVEVEHR)+
                    iNA2zero(MORNVEHNR)+iNA2zero(EVEVEHNR)) %>%
    dplyr::select(-COUNTKEY,-COUNT,-MORNVEHR,-EVEVEHR,-MORNVEHNR,-EVEVEHNR,
                  -SITEDESC,-COMMENTS)
  # Expand observed to estimated pressure count using fishable days
  PC <- cnts %>%
    ## Number of days and average number of vehicles at each SITE, MONTH, DAYTYPE
    dplyr::group_by(MONTH,DAYTYPE,SITE) %>%
    dplyr::summarize(cntDays=dplyr::n(),
                     cntdVeh=sum(ALLVEH,na.rm=TRUE)) %>%
    ## Append on number of fishable days for each SITE, MONTH, DAYTYPE
    dplyr::right_join(FD,by=c("MONTH","DAYTYPE","SITE")) %>%
    ## Expand to total number of vehicles for each SITE, MONTH, DAYTYPE
    dplyr::mutate(ttlVeh=(cntdVeh/cntDays)*ttlDays) %>%
    ## Rearrange to better match XLS results
    dplyr::arrange(MONTH,DAYTYPE,SITE) %>%
    dplyr::select(SURVEY,ROUTE,MONTH,DAYTYPE,SITE,
                  cntDays,cntdVeh,ttlDays,ttlVeh)
  as.data.frame(PC)
}

# Summarize pressure counts by MONTH, FISHERY, and DAYTYPE, across SITEs
sumPressureCount <- function(d) {
  d %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     ttlVehFshry=sum(ttlVehSiteFshry,na.rm=TRUE)) %>%
    as.data.frame()
}

#
readInterviewData_ICE <- function(RDIR,LOC) {
  readxl::read_excel(file.path(RDIR,"qry_ice_interviews_4R.xlsx")) %>%
    dplyr::filter(ROUTE==LOC) %>%
    dplyr::rename_all(.funs=toupper) %>%
    dplyr::mutate(MONTH=iHndlMonth(MONTH),
                  DAYTYPE=iHndlDaytype(DAYTYPE),
                  SITE=iHndlSiteDesc(SITE,SITEDESC),
                  SPP=FSA::capFirst(SPP),
                  HOURS=((STOPHH+STOPMM/60)-(STARTHH+STARTMM/60))) %>%
    dplyr::select(-INTERVIEWKEY,-SITEDESC,-(STARTHH:STOPMM),-RES)
}

#
expandHarvest <- function(ints,nofish,pc) {
  # Summarized observations about angling effort and success by MONTH, FISHERY,
  # and DAYTYPE and also across DAYTYPES.
  f <- nofish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(nInts=dplyr::n(),
                     obsAnglers=sum(PERSONS),
                     obsHours=sum(HOURS),
                     obsAnglerHours=sum(PERSONS*HOURS),
                     obsSuccAnglers=sum(SUCCESS)) %>%
    as.data.frame()

  # Summarized observed harvest of each SPP (including "All Fish") by MONTH,
  # FISHERY, and DAYTYPE and also across DAYTYPES.
  h <- ints %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(obsHarvest=sum(NUM)) %>%
    as.data.frame()

  # Combine observed harvest and individual effort summaries
  join_bys <- c("SURVEY","ROUTE","UNIT","MONTH","FISHERY","DAYTYPE")
  dplyr::full_join(h,f,by=join_bys) %>%
    dplyr::mutate(obsSuccAnglers=ifelse(SPP=="All Fish",obsSuccAnglers,NA),
                  obsHrate=obsHarvest/obsAnglerHours,
                  obsHrsperfish=ifelse(obsHrate>0,1/obsHrate,NA),
                  obsAvgTrip=obsHours/nInts,
                  obsAvgAnglerHours=obsAnglerHours/obsAnglers,
                  obsAnglersPerInt=obsAnglers/nInts,
                  obsPercSucc=obsSuccAnglers/obsAnglers) %>%
    dplyr::select(-nInts) %>%
    as.data.frame() %>%
    ## Join on effort (total vehicles)
    dplyr::full_join(pc,by=join_bys) %>%
    dplyr::select(-NINTS) %>%
    dplyr::mutate(ttlAnglers=ttlVehFshry*obsAnglersPerInt,
                  ttlAnglerHours=ttlAnglers*obsAvgAnglerHours,
                  ttlSuccAnglers=ttlAnglers*obsPercSucc,
                  ttlHarvest=ttlAnglerHours*obsHrate) %>%
    dplyr::arrange(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE) %>%
    as.data.frame()
}

#
sumExpHarvest_AllFish <- function(eh) {
  ## Make a helper function
  iSumHarv <- function(d) {
      summarize(d,
                ttlVehFshry=sum(ttlVehFshry),
                ttlAnglers=sum(ttlAnglers),
                ttlAnglerHours=sum(ttlAnglerHours),
                ttlSuccAnglers=sum(ttlSuccAnglers),
                ttlHarvest=sum(ttlHarvest))
  }

  # Isolate all fish and the total variables
  eh <- eh %>%
    filter(SPP=="All Fish") %>%
    select(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE,
           ttlVehFshry,ttlAnglers,ttlAnglerHours,ttlSuccAnglers,ttlHarvest) %>%
    mutate(MONTH=as.character(MONTH))
  # Find levels of MONTHs, FISHERYs, and DAYTYPEs
  mlvls <- unique(eh$MONTH)
  flvls <- unique(eh$FISHERY)
  dlvls <- unique(eh$DAYTYPE)
  ## Find all possible "All"s (for FISHERY, MONTH, and DAYTYPE)
  # Sum total variables across FISHERYs
  tmp1 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,DAYTYPE,MONTH) %>%
    iSumHarv() %>%
    mutate(FISHERY="All") %>%
    select(names(eh))
  # Sum total variables across MONTHs
  tmp2 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,FISHERY,DAYTYPE) %>%
    iSumHarv() %>%
    mutate(MONTH="All") %>%
    select(names(eh))
  # Sum total variables across DAYTYPEs
  tmp3 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY) %>%
    iSumHarv() %>%
    mutate(DAYTYPE="All") %>%
    select(names(eh))
  # ... and across FISHERYs and MONTHs
  tmp4 <- tmp1 %>%
    iSumHarv() %>%
    mutate(FISHERY="All",MONTH="All") %>%
    select(names(eh))
  # Sum total variables across FISHERYs and DAYTYPEs
  tmp5 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,MONTH) %>%
    iSumHarv() %>%
    mutate(FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  # Sum total variables across MONTHs and DAYTYPEs
  tmp6 <- tmp2 %>%
    iSumHarv() %>%
    mutate(MONTH="All",DAYTYPE="All") %>%
    select(names(eh))
  # ... and across MONTHs, DAYTYPES, and FISHERYs
  tmp7 <- tmp6 %>%
    iSumHarv() %>%
    mutate(MONTH="All",FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  # Combine, calculate the "rates" from the totals, rearrange, and return
  rbind(eh,as.data.frame(tmp1),as.data.frame(tmp2),
        as.data.frame(tmp3),as.data.frame(tmp4),as.data.frame(tmp5),
        as.data.frame(tmp6),as.data.frame(tmp7)) %>%
    mutate(MONTH=factor(MONTH,levels=c(mlvls,"All")),
           FISHERY=factor(FISHERY,levels=c(flvls,"All")),
           DAYTYPE=factor(DAYTYPE,levels=c(dlvls,"All")),
           anglersPerInt=ttlAnglers/ttlVehFshry,
           avgAnglerHours=ttlAnglerHours/ttlAnglers,
           percSucc=ttlSuccAnglers/ttlAnglers*100,
           harvestRate=ttlHarvest/ttlAnglerHours) %>%
    arrange(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE,SPP) %>%
    select(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE,SPP,
           ttlVehFshry,anglersPerInt,ttlAnglers,avgAnglerHours,
           ttlAnglerHours,percSucc,ttlSuccAnglers,harvestRate,ttlHarvest)
}

sumExpHarvest_eachSpp <- function(eh,sehALL) {
  # Isolate just the ttlAnglerHours in sehALL
  sehALL <- select(sehALL,SURVEY:DAYTYPE,ttlAnglerHours)
  
  # Isolate all fish and the total variables in eh
  eh <- eh %>%
    filter(SPP!="All Fish") %>%
    select(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE,ttlHarvest) %>%
    mutate(MONTH=as.character(MONTH))

  # Find levels of MONTHs, FISHERYs, and DAYTYPEs
  mlvls <- unique(eh$MONTH)
  flvls <- unique(eh$FISHERY)

  ## Find all possible "All"s (for FISHERY, MONTH, and DAYTYPE)
  # Sum total variables across FISHERYs
  tmp1 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,DAYTYPE,MONTH) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(FISHERY="All") %>%
    select(names(eh))
  # Sum total variables across MONTHs
  tmp2 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,FISHERY,DAYTYPE) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(MONTH="All") %>%
    select(names(eh))
  # Sum total variables across DAYTYPEs
  tmp3 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(DAYTYPE="All") %>%
    select(names(eh))
  # ... and across FISHERYs and MONTHs
  tmp4 <- tmp1 %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(FISHERY="All",MONTH="All") %>%
    select(names(eh))
  # Sum total variables across FISHERYs and DAYTYPEs
  tmp5 <- eh %>%
    group_by(SURVEY,ROUTE,UNIT,SPP,MONTH) %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  # Sum total variables across MONTHs and DAYTYPEs
  tmp6 <- tmp2 %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(MONTH="All",DAYTYPE="All") %>%
    select(names(eh))
  # ... and across MONTHs, DAYTYPES, and FISHERYs
  tmp7 <- tmp6 %>%
    summarize(ttlHarvest=sum(ttlHarvest)) %>%
    mutate(MONTH="All",FISHERY="All",DAYTYPE="All") %>%
    select(names(eh))
  # Combine, calculate the "rates" from the totals, rearrange, and return
  rbind(eh,as.data.frame(tmp1),as.data.frame(tmp2),
        as.data.frame(tmp3),as.data.frame(tmp4),as.data.frame(tmp5),
        as.data.frame(tmp6),as.data.frame(tmp7)) %>%
    dplyr::full_join(sehALL,by=c("SURVEY","ROUTE","UNIT","MONTH",
                                 "FISHERY","DAYTYPE")) %>%
    dplyr::mutate(MONTH=factor(MONTH,levels=c(mlvls,"All")),
           FISHERY=factor(FISHERY,levels=c(flvls,"All")),
           DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All")),
           harvestRate=ttlHarvest/ttlAnglerHours) %>%
    arrange(SURVEY,ROUTE,UNIT,SPP,FISHERY,MONTH,DAYTYPE) %>%
    select(SURVEY,ROUTE,UNIT,SPP,FISHERY,MONTH,DAYTYPE,harvestRate,ttlHarvest)
}

readFish_ICE <- function(RDIR,LOC,nofish) {
  readxl::read_excel(file.path(RDIR,"qry_ice_fish_4R.xlsx")) %>%
    dplyr::filter(ROUTE==LOC) %>%
    dplyr::rename_all(.funs=toupper) %>%
    dplyr::mutate(SPP=FSA::capFirst(SPP),
                  ORIGIN=ifelse(is.na(CLIP)|CLIP=="Native","Native","Hatchery")) %>%
    dplyr::left_join(dplyr::select(nofish,INTERVIEW,UNIT,MONTH,FISHERY),
                     by="INTERVIEW")  %>%
    dplyr::select(SURVEY,ROUTE,UNIT,SPP,FISHERY,MONTH,LENGTH,CLIP,ORIGIN)
}

sumLengths_ICE <- function(fish) {
  ## Create a helper file for repeated summaries
  iSumLengths <- function(d) {
    tmp <- dplyr::summarize(d,
                            n=dplyr::n(),
                            mnLen=mean(LENGTH,na.rm=TRUE),
                            sdLen=sd(LENGTH,na.rm=TRUE),
                            minLen=min(LENGTH,na.rm=TRUE),
                            maxLen=max(LENGTH,na.rm=TRUE)) %>%
      as.data.frame()
    # Convert NaNs to NAs (helps in huxtable)
    tmp$sdLen[is.nan(tmp$sdLen)] <- NA
    tmp
  }
  
  tmp1 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,ORIGIN) %>%
    iSumLengths() %>%
    arrange(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,ORIGIN) %>%
    as.data.frame()
  
  tmp2 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY) %>%
    iSumLengths() %>%
    dplyr::mutate(ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp3 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,ORIGIN) %>%
    iSumLengths() %>%
    dplyr::mutate(FISHERY="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp4 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH) %>%
    iSumLengths() %>%
    dplyr::mutate(FISHERY="All",ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp5 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,FISHERY,ORIGIN) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp6 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,FISHERY) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All",ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp7 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,ORIGIN) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All",FISHERY="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  tmp8 <- fish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP) %>%
    iSumLengths() %>%
    dplyr::mutate(MONTH="All",FISHERY="All",ORIGIN="All") %>%
    dplyr::select(names(tmp1)) %>%
    as.data.frame()
  
  flvls <- unique(fish$FISHERY)
  lenSum <- rbind(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8) %>%
    as.data.frame() %>%
    mutate(ORIGIN=factor(ORIGIN,levels=c("Native","Hatchery","All")),
           FISHERY=factor(FISHERY,levels=c(flvls,"All"))) %>%
    dplyr::arrange(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,ORIGIN)
  
}


#### Secondary Helpers ---------------------------------------------------------
# Turn month numbers into abbreviations and have Dec be first
iHndlMonth <- function(x) factor(month.abb[x],levels=month.abb[c(12,1:11)])

# Simplify "Weekend/Holiday" to "Weekend"
iHndlDaytype <- function(x) FSA::mapvalues(x,from="Weekend/Holiday",to="Weekend")

# Make more useful site description (site number plus site description)
iHndlSiteDesc <- function(s,d) paste0(s,"-",FSA::capFirst(d))

# Convert NAs to zeroes
iNA2zero <- function(x) ifelse(is.na(x),0,x)

## Find the proportio of iterviews at a SITE that are in each FISHERY type.
iFindPropIntsInFishery <- function(nofish) {
  ## Number of interviews at each SITE within MONTH, DAYTYPE, FISHERY
  tmp1 <- ints_NOFISH %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE,SITE) %>%
    dplyr::summarize(NINTS=dplyr::n()) %>%
    as.data.frame()
  ## Number of interviews at each SITE within MONTH, DAYTYPE
  tmp2 <- tmp1 %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,DAYTYPE,SITE) %>%
    dplyr::summarize(TTLINTS=sum(NINTS)) %>%
    as.data.frame()  
  ## Combine and compute proportion of interviews at a SITE within each FISHERY
  ## by MONTH, DAYTYPE ... return result
  dplyr::left_join(tmp1,tmp2,
                   by=c("SURVEY","ROUTE","UNIT","MONTH","DAYTYPE","SITE")) %>%
    dplyr::mutate(pIntsInFshry=NINTS/TTLINTS)
}

# Combines 
iFindTtlVehiclesInFishery <- function(d,pc) {
  dplyr::full_join(d,pc,by=c("SURVEY","ROUTE","MONTH","DAYTYPE","SITE")) %>%
    dplyr::rename(ttlVehSite=ttlVeh) %>%
    dplyr::mutate(ttlVehSiteFshry=ttlVehSite*pIntsInFshry) %>%
    dplyr::select(-cntDays,-ttlDays,-cntdVeh)
}

#
iHndlNoIntsButPressure <- function(d) {
  ## Find records with missing ttlVehSiteFshry (i.e., pressure but no interviews)
  MISS <- is.na(d$ttlVehSiteFshry)
  ## Ashland, 2nd Landing ... some effort for both bobbing and shallow
  tmp <- MISS & d$ROUTE=="Ashland" & d$SITE=="204-2nd Landing"
  ## Ashland, not 2nd Landing ... all shallow (assume no bobbing)
  tmp <- MISS & d$ROUTE=="Ashland" & d$SITE!="204-2nd Landing"
  d$FISHERY[tmp] <- "< 60 ft - Shallow"
  d$UNIT[tmp] <- "WI-2"
  d$ttlVehSiteFshry[tmp] <- d$ttlVehSite[tmp]
  d
}

iOrderSpecies <- function(spp) {
  lvls <- c("Lake Trout","Siscowet","Brook Trout","Splake",
            "Coho Salmon","Chinook Salmon","Rainbow Trout","Pink Salmon",
            "Brown Trout","Atlantic Salmon","Lake Whitefish","Round Whitefish",
            "Lake Herring","Rainbow Smelt","Burbot","Lake Sturgeon","Walleye",
            "Yellow Perch","Ruffe","Northern Pike",
            "Smallmouth Bass","Largemouth Bass",
            "Bluegill","Pumpkinseed","Sunfish spp","Black Crappie",
            "Crappie spp.","Rock Bass","White Perch","Catfish","Carp")
  if (any(!unique(spp) %in% lvls)) stop("A species not listed!!")
  factor(spp,levels=lvls)
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

## Tables ----------------------------------------------------------------------
tableCaptions <- function() {
  tables <- captioner::captioner(prefix="Table")
  tables(name="Table1",
         caption=paste("Number of pressure counts and number of observed",
                       "(i.e., counted) vehicles in those counts and total",
                       "fishable days and estimated total (i.e., expanded)",
                       "number of vehicles at each site by month and day type.",
                       "Compare this to 'ICE CREEL MONTHLY PRESSURE' Excel file."))
  tables(name="Table2",
         caption=paste("Number of interviews (N), percent of interviews (P) at",
                       "that site in each fishery type, and estimated total",
                       "(i.e., expanded) number of vehicles (Veh) in each fishery",
                       "and across all fishery types at each site by month",
                       "and day type. Compare this to 'ICE CREEL EXPANDED",
                       "PRESSURE' Excel file."))
  tables(name="Table3",
         caption=paste("Total effort and harvest summary for All Fish combined",
                       "by fishery type, month, and day type. Compare this to",
                       "columns A-K of 'ICE CREEL HARVEST' Excel file."))
  tables(name="Table4",
         caption=paste("Harvest rate and total harvest by species, fishery,",
                       "month, and daytype. Note that 'All' is shown for the",
                       "fishery only for species caught in more than one",
                       "fishery. Compare this to columns L-AD of 'ICE CREEL",
                       "HARVEST' Excel file."))
  tables(name="Table5",
         caption=paste("Number of fish measured for length and mean, standard",
                       "deviation (SD), minimum, and maximum total length",
                       "(in.) by species, month, origin (native or hatchery)",
                       "and fishery type. Note that 'All' is shown only for",
                       "months and origins where more than one month or origin",
                       "was observed. Compare this to 'ICE CREEL CATCH AVE'",
                       "Excel file."))
  
  
  
  ## Not needed as the main info is in the new Table 1.
  tables(name="TableX",
         caption=paste("Number of fishable days (i.e., 'good ice') at each site",
                       "by month and day type. These values are used to expand",
                       "observed pressure counts to total counts by site."))
  tables
}

table1 <- function(PC) {
  ## Prepare data.frame for huxtable
  PC <- dplyr::select(PC,-SURVEY,-ROUTE)
  tmp1 <- dplyr::filter(PC,DAYTYPE=="Weekday") %>%
    dplyr::select(-DAYTYPE)
  tmp2 <- dplyr::filter(PC,DAYTYPE=="Weekend") %>%
    dplyr::select(-DAYTYPE)
  tbl1 <- dplyr::full_join(tmp1,tmp2,by=c("MONTH","SITE")) %>%
    arrange(MONTH,SITE) %>%
    dplyr::mutate(# Remove repeated rows in MONTH variable
                  MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use="MONTH"),
                               "",as.character(MONTH))) %>%
    tibble::add_column(.,spc1="",.after="cntdVeh.x") %>%
    tibble::add_column(.,spc2="",.after="cntdVeh.y") %>%
    tibble::add_column(.,spc3="",.after="ttlVeh.x")
  ## Rows with a MONTH names (except first) will get extra space above
  spaceAbove <- which(tbl1$MONTH!="")[-1]
  
  ## Make the huxtable
  tbl2 <- as_hux(tbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=spaceAbove,col=everywhere,value=15) %>%
    # Convert NAs to dashes
    set_na_string(row=everywhere,col=-(1:2),value="--") %>%
    # right align all but leftmost two columns
    set_align(row=everywhere,col=-(1:2),value="right") %>%
    # No decimals for observed values and total days
    set_number_format(row=everywhere,col=c(3:4,6,9:10,12),value=0) %>%
    set_number_format(row=everywhere,col=c(7,13),value=1) %>%
    # Extra label at the top
    rbind(c("MONTH","SITE",rep(c("Days","Vehicles",""),3),"Days","Vehicles"),.) %>%
    rbind(c("","","Counted","","","Total","","","Counted","","","Total",""),.) %>%
    rbind(c("","","Weekdays","","","","","","Weekends","","","",""),.) %>%
    # Adjust headers
    merge_cells(row=1,col=3:7) %>%
    set_bottom_border(row=1,col=3,value=1) %>%
    merge_cells(row=1,col=9:13) %>%
    set_bottom_border(row=1,col=9,value=1) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    merge_cells(row=2,col=3:4) %>%
    set_bottom_border(row=2,col=3,value=1) %>%
    merge_cells(row=2,col=6:7) %>%
    set_bottom_border(row=2,col=6,value=1) %>%
    merge_cells(row=2,col=9:10) %>%
    set_bottom_border(row=2,col=9,value=1) %>%
    merge_cells(row=2,col=12:13) %>%
    set_bottom_border(row=2,col=12,value=1) %>%
    set_align(row=2,col=everywhere,value="center") %>%
    # Sets table & column widths
    set_width(0.9) %>%
    set_col_width(col=c(.1,.35,.1,.1,.025,.1,.1,.075,.1,.1,.025,.1,.1)) %>%
    iFinishTable(labelsRow=3,labelsCol=2)
  tbl2
}


table2 <- function(PC) {
  ## Prepare data.frame for huxtable
  fshrys <- unique(PC$FISHERY)
  numf <- length(fshrys)
  bys <- c("SURVEY","ROUTE","UNIT","MONTH","DAYTYPE","SITE")
  PC <- mutate(PC,pIntsInFshry=100*pIntsInFshry)
  tmp1 <- dplyr::select(PC,-NINTS,-pIntsInFshry,-ttlVehSiteFshry)
  tmp2 <- dplyr::select(PC,-TTLINTS,-ttlVehSite)
  tmp3 <- split(tmp2,tmp2$FISHERY)
  tmp4 <- dplyr::full_join(tmp3[[1]],tmp3[[2]],by=bys)
  for (i in 3:length(tmp3)) {
    tmp4 <- dplyr::full_join(tmp4,tmp3[[i]],by=bys)
  }
  tbl1 <- dplyr::full_join(tmp4,tmp1,by=bys) %>%
    dplyr::select(-dplyr::contains("FISHERY"),-SURVEY,-ROUTE,-UNIT) %>%
    dplyr::filter(!duplicated(.)) %>%
    dplyr::arrange(MONTH,DAYTYPE,SITE) %>%
    # Remove repeated rows in MONTH and DAYTYPE variables
    dplyr::mutate(MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use="MONTH"),
                               "",as.character(MONTH)),
                  DAYTYPE=ifelse(!FSA::repeatedRows2Keep(.,cols2use="DAYTYPE"),
                                 "",as.character(DAYTYPE)))
  
  acs <- which(grepl("ttlVehSiteFshry",names(tbl1)))
  for (i in rev(acs)) {
    tbl1 <- tibble::add_column(tbl1,"",.after=i)
    names(tbl1)[i+1] <- paste0("spc",i)
  }
  
  spaceAbove <- which(tbl1$DAYTYPE!="")[-1]
  
  toplbl <- c("","","")
  for (i in fshrys) toplbl <- c(toplbl,i,"","","")
  toplbl <- c(toplbl,"Total","")
  
  tbl2 <- as_hux(tbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=spaceAbove,col=everywhere,value=15) %>%
    # Convert NAs to dashes
    set_na_string(row=everywhere,col=-(1:3),value="--") %>%
    # right align all but leftmost three columns
    set_align(row=everywhere,col=-(1:3),value="right") %>%
    # No decimals for observed values and total days
    set_number_format(row=everywhere,col=contains("INTS"),value=0) %>%
    set_number_format(row=everywhere,col=contains("ttlVeh"),value=1) %>%
    set_number_format(row=everywhere,col=contains("pInts"),value=0) %>%
    # Extra label at the top
    rbind(c("MONTH","DAYTYPE","SITE",
            rep(c("N","P","Veh",""),numf),
            "N","Veh"),.) %>%
    rbind(toplbl,.)
  # Adjust headers
  for (i in 1:numf) {
    cols <- (4:6)+(i-1)*4
    tbl2 <- merge_cells(tbl2,row=1,col=cols) %>%
      set_bottom_border(row=1,col=cols[1],value=1)
  }
  tbl2 <- tbl2 %>%
    merge_cells(row=1,col=(0:1)+(numf+1)*4) %>%
    set_bottom_border(row=1,col=(numf+1)*4,value=1) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    # Sets table & column widths
    set_width(0.99) %>%
    set_col_width(col=c(.1,.1,.35,
                        rep(c(0.1,0.1,0.1,0.025),numf),
                        .1,.1)) %>%
    iFinishTable(labelsRow=2,labelsCol=3)
  tbl2
}


table3 <- function(H) {
  # Prepare data.frame for huxtable
  tbl1 <- H %>%
    dplyr::select(FISHERY,MONTH,DAYTYPE,
                  ttlVehFshry,anglersPerInt,ttlAnglers,
                  avgAnglerHours,ttlAnglerHours,percSucc,ttlSuccAnglers,
                  harvestRate,ttlHarvest) %>%
    dplyr::arrange(FISHERY,MONTH,DAYTYPE) %>%
    dplyr::mutate(# Remove repeated rows in MONTH and FISHERY variables
      MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use="MONTH"),
                   "",as.character(MONTH)),
      FISHERY=ifelse(!FSA::repeatedRows2Keep(.,cols2use="FISHERY"),
                   "",as.character(FISHERY))) %>%
    ## Remove rows that are repeats (for the numerics) of the row above it and
    ## if DAYTYPE=="All" (i.e., "All" is just for one daytype).
    dplyr::filter(!(!FSA::repeatedRows2Keep(.,
                              cols2ignore=c("FISHERY","MONTH","DAYTYPE")) &
                  DAYTYPE=="All"))

      
  ## Rows with a FISHERY names (except first) will get extra space above
  spaceAbove <- which(tbl1$FISHERY!="")[-1]
  ## Rows with a DAYTYPE=="All" (except lasst) will get extra space below
  spaceBelow <- which(tbl1$DAYTYPE=="All")
  spaceBelow <- spaceBelow[-length(spaceBelow)]
  
  ## Make the huxtable
  tbl2 <- as_hux(tbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=spaceAbove,col=everywhere,value=25) %>%
    set_bottom_padding(row=spaceBelow,col=everywhere,value=5) %>%
    # Convert NAs to dashes
    set_na_string(row=everywhere,col=-(1:3),value="--") %>%
    # right align all but leftmost thre columns
    set_align(row=everywhere,col=-(1:3),value="right") %>%
    # contol decimals
    set_number_format(row=everywhere,col=c(4,6,8,10,12),value=0) %>%
    set_number_format(row=everywhere,col=9,value=1) %>%
    set_number_format(row=everywhere,col=c(5,7),value=2) %>%
    set_number_format(row=everywhere,col=11,value=4) %>%
    # Extra label at the top
    rbind(c("FISHERY","MONTH","DAYTYPE","Vehicles","per Int","Anglers",
            "Hours","Angler Hrs","Success","Anglers","Rate","Harvest"),.) %>%
    rbind(c("","","","Total","Angler","Total",
            "Avg","Total","Perc","Success","Harvest","Total"),.) %>%
    set_align(row=1:2,col=everywhere,value="center") %>%
    # Sets table & column widths
    set_width(0.9) %>%
    set_col_width(col=c(.2,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1)) %>%
    iFinishTable(labelsRow=2,labelsCol=3)
  tbl2
}

table4 <- function(H) {
  # Prepare data.frame for huxtable
  ## Move DAYTYPEs into columns
  H <- select(H,-SURVEY,-ROUTE,-UNIT)
  tmp1 <- dplyr::filter(H,DAYTYPE=="Weekday") %>%
    dplyr::select(-DAYTYPE)
  tmp2 <- dplyr::filter(H,DAYTYPE=="Weekend") %>%
    dplyr::select(-DAYTYPE)
  tmp3 <- dplyr::filter(H,DAYTYPE=="All") %>%
    dplyr::select(-DAYTYPE)
  tbl1 <- dplyr::full_join(tmp1,tmp2,by=c("SPP","FISHERY","MONTH")) %>%
    dplyr::full_join(tmp3,by=c("SPP","FISHERY","MONTH")) %>%
    dplyr::filter(!is.na(SPP)) %>%
    dplyr::mutate(SPP=iOrderSpecies(SPP)) %>%
    dplyr::arrange(SPP,FISHERY,MONTH) %>%
    ## Add aesthetic spacing columns (for huxtable)
    tibble::add_column(.,spc1="",.after="ttlHarvest.x") %>%
    tibble::add_column(.,spc2="",.after="ttlHarvest.y")
  ## Drop the "All" FISHERYs for species captured in only one fishery
  ## (note the use of 2 in the filter because of an "All" fishery)
  SPP2dropAll <- dplyr::group_by(tbl1,SPP) %>%
    dplyr::summarize(numFshrys=length(unique(FISHERY))) %>%
    dplyr::filter(numFshrys==2)
  tbl1 <- filter(tbl1,!(SPP %in% SPP2dropAll$SPP & FISHERY=="All"))
  ## Drop the "All" MONTHs for species captured in only one month
  ## (note the use of 2 in the filter because of an "All" MONTH)
  SPP2dropAll <- dplyr::group_by(tbl1,SPP) %>%
    dplyr::summarize(numMons=length(unique(MONTH))) %>%
    dplyr::filter(numMons==2)
  tbl1 <- filter(tbl1,!(SPP %in% SPP2dropAll$SPP & MONTH=="All"))
  ## Remove repeated rows in SPP, FISHERY, MONTH variables (aesthetics in huxt)
  tbl1 <- tbl1 %>%
          dplyr::mutate(SPP=ifelse(!FSA::repeatedRows2Keep(.,cols2use="SPP"),
                                    "",as.character(SPP)),
              ## Use of both SPP and FISHERY (rather than just FISHERY) guards
              ## against cutting a FISHERY across species
              FISHERY=ifelse(!FSA::repeatedRows2Keep(.,cols2use=c("SPP","FISHERY")),
                             "",as.character(FISHERY)),
              MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use=c("SPP","MONTH")),
                           "",as.character(MONTH)))
  ## Rows with a MONTH names (except first) will get extra space above
  spaceAbove <- which(tbl1$SPP!="")[-1]

  ## Make the huxtable
  tbl2 <- as_hux(tbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=spaceAbove,col=everywhere,value=15) %>%
    # Convert NAs to dashes
    set_na_string(row=everywhere,col=-(1:3),value="--") %>%
    # right align all but leftmost two columns
    set_align(row=everywhere,col=-(1:3),value="right") %>%
    # No decimals for harvest rates, four for harvest rate
    set_number_format(row=everywhere,col=contains("Harvest"),value=0) %>%
    set_number_format(row=everywhere,col=contains("Rate"),value=4) %>%
    # Extra label at the top
    rbind(c("SPP","FISHERY","MONTH",rep(c("Rate","Harvest",""),2),
            "Rate","Harvest"),.) %>%
    rbind(c("","","",rep(c("Harvest","Total",""),2),"Harvest","Total"),.) %>%
    rbind(c("","","","Weekdays","","","Weekends","","","All",""),.) %>%
    # Adjust headers
    merge_cells(row=1,col=4:5) %>%
    set_bottom_border(row=1,col=4,value=1) %>%
    merge_cells(row=1,col=7:8) %>%
    set_bottom_border(row=1,col=7,value=1) %>%
    merge_cells(row=1,col=10:11) %>%
    set_bottom_border(row=1,col=10,value=1) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    # Sets table & column widths
    set_width(0.99) %>%
    set_col_width(col=c(.2,.3,.1,.15,.1,.025,.15,.1,.025,.15,.1)) %>%
    iFinishTable(labelsRow=3,labelsCol=3)
  tbl2
}

table5 <- function(f) {
  ## Prepare data.frame for huxtable
  fshrys <- unique(f$FISHERY)
  numf <- length(fshrys)
  bys <- c("SURVEY","ROUTE","UNIT","SPP","MONTH","ORIGIN")
  tmp1 <- split(f,f$FISHERY)
  tmp2 <- dplyr::full_join(tmp1[[1]],tmp1[[2]],by=bys)
  for (i in 3:numf) tmp2 <- dplyr::full_join(tmp2,tmp1[[i]],by=bys)
  
  tbl1 <- tmp2 %>%
    dplyr::select(-dplyr::contains("FISHERY"),-SURVEY,-ROUTE,-UNIT) %>%
    dplyr::filter(!duplicated(.)) %>%
    dplyr::mutate(SPP=iOrderSpecies(SPP)) %>%
    dplyr::arrange(SPP,MONTH,ORIGIN)
  
  ## Drop the "All" ORIGINs for species have only one ORIGIN
  ## (note the use of 2 in the filter because of an "All" ORIGIN)
  SPP2dropAll <- dplyr::group_by(tbl1,SPP) %>%
    dplyr::summarize(numOrigins=length(unique(ORIGIN))) %>%
    dplyr::filter(numOrigins==2)
  tbl1 <- filter(tbl1,!(SPP %in% SPP2dropAll$SPP & ORIGIN=="All"))

  ## Drop the "All" MONTHs for species have only one MONTH
  ## (note the use of 2 in the filter because of an "All" MONTH)
  SPP2dropAll <- dplyr::group_by(tbl1,SPP) %>%
    dplyr::summarize(numMons=length(unique(MONTH))) %>%
    dplyr::filter(numMons==2)
  tbl1 <- filter(tbl1,!(SPP %in% SPP2dropAll$SPP & MONTH=="All"))
  
  ## Remove repeated rows in SPP, MONTH and ORIGIN variables
  tbl1 <- tbl1 %>%
    dplyr::mutate(SPP=ifelse(!FSA::repeatedRows2Keep(.,cols2use="SPP"),
                             "",as.character(SPP)),
                  MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use=c("SPP","MONTH")),
                               "",as.character(MONTH)))
  
  ## Add extra columns for aesthetics in huxtable
  acs <- which(grepl("maxLen",names(tbl1)))
  for (i in rev(acs[-length(acs)])) {
    tbl1 <- tibble::add_column(tbl1,"",.after=i)
    names(tbl1)[i+1] <- paste0("spc",i)
  }
  
  spaceAbove <- which(tbl1$SPP!="")[-1]
  
  toplbl <- c("","","")
  for (i in fshrys) toplbl <- c(toplbl,i,"","","","","")
  toplbl <- toplbl[-length(toplbl)]
  
  tbl2 <- as_hux(tbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=spaceAbove,col=everywhere,value=15) %>%
    # Convert NAs to dashes
    set_na_string(row=everywhere,col=-(1:3),value="--") %>%
    # right align all but leftmost three columns
    set_align(row=everywhere,col=-(1:3),value="right") %>%
    # Set decimals
    set_number_format(row=everywhere,col=starts_with("n"),value=0) %>%
    set_number_format(row=everywhere,col=contains("Len"),value=1) %>%
    # Extra label at the top
    rbind(c("SPP","MONTH","ORIGIN",
            rep(c("n","Mean","SD","Min","Max",""),numf-1),
            "n","Mean","SD","Min","Max"),.) %>%
    rbind(toplbl,.)
  # Adjust headers
  for (i in 1:numf) {
    cols <- (4:8)+(i-1)*6
    tbl2 <- merge_cells(tbl2,row=1,col=cols) %>%
      set_bottom_border(row=1,col=cols[1],value=1)
  }
  tbl2 <- tbl2 %>%
    set_align(row=1,col=everywhere,value="center") %>%
    # Sets table & column widths
    set_width(0.99) %>%
    set_col_width(col=c(.2,.1,.1,
                        rep(c(0.1,0.1,0.1,0.1,0.1,0.025),numf-1),
                        0.1,0.1,0.1,0.1,0.1)) %>%
    iFinishTable(labelsRow=2,labelsCol=3)
  tbl2
}



tableX <- function(FD) {
  ## Prepare data.frame for huxtable
  tbl1 <- FD %>%
    dplyr::select(-SURVEY,-ROUTE,-YEAR) %>%
    ## Create columns of weekdays and weekends
    tidyr::spread(DAYTYPE,ttlDays) %>%
    dplyr::mutate('All Days'=iNA2zero(Weekday)+iNA2zero(Weekend),
                  # Remove repeated rows in MONTH variable
                  MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use="MONTH"),
                               "",as.character(MONTH)))
  ## Rows with a MONTH names (except first) will get extra space above
  spaceAbove <- which(tbl1$MONTH!="")[-1]
  
  ## Make the huxtable
  tbl2 <- as_hux(tbl1) %>%
    # Set all cell paddings
    set_all_padding(1) %>%
    set_right_padding(row=everywhere,col=everywhere,value=5) %>%
    set_top_padding(row=spaceAbove,col=everywhere,value=15) %>%
    # Convert NAs to dashes
    set_na_string(row=everywhere,col=-(1:2),value="--") %>%
    # right align all but leftmost two columns
    set_align(row=everywhere,col=-(1:2),value="right") %>%
    # Top label covers columns 3-5, centered, line underneath it
    rbind(c("MONTH","SITE","Weekday","Weekend","All Days"),.) %>%  
    rbind(c("","","DAY TYPE","",""),.) %>%
    merge_cells(row=1,col=3:5) %>% 
    set_align(row=1,col=everywhere,value="center") %>%
    set_bottom_border(row=1,col=3,value=1) %>%
    # Sets table & column widths
    set_width(0.4) %>%
    set_col_width(col=c(.15,.4,.15,.15,.15)) %>%
    iFinishTable(labelsRow=2,labelsCol=2)
  tbl2
}

## Old Helpers ----
#
sumPressureCount2 <- function(d) {
  # Change MONTHs to a character
  d <- mutate(d,MONTH=as.character(MONTH))
  # Get levels of MONTH and FISHERY
  mlvls <- unique(d$MONTH)
  flvls <- unique(d$FISHERY)
  # Summarize pressure counts by MONTH, FISHERY, and DAYTYPE
  tmp1 <- d %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     ttlVehFshry=sum(ttlVehSiteFshry,na.rm=TRUE))
  # Summarize pressure counts by MONTH and FISHERY, across DAYTYPEs
  tmp2 <- tmp1 %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     ttlVehFshry=sum(ttlVehFshry,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(tmp1))
  # Summarize pressure counts by MONTH, across DAYTYPEs and FISHERYs
  tmp3 <- tmp2 %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     ttlVehFshry=sum(ttlVehFshry,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All",FISHERY="All") %>%
    dplyr::select(names(tmp1))
  # Summarize pressure counts across DAYTYPEs, FISHERYs, and MONTHs
  tmp4 <- tmp3 %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     ttlVehFshry=sum(ttlVehFshry,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All",FISHERY="All",MONTH="All") %>%
    dplyr::select(names(tmp1))
  # Combine, rearrange, and return
  as.data.frame(rbind(tmp1,tmp2,tmp3,tmp4)) %>%
    mutate(DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All")),
           FISHERY=factor(FISHERY,levels=c(flvls,"All")),
           MONTH=factor(MONTH,levels=c(mlvls,"All"))) %>%
    arrange(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE)
}

#
sumObsEffortSuccess2 <- function(nofish) {
  # Change MONTHs to a character
  nofish <- mutate(nofish,MONTH=as.character(MONTH))
  # Get levels of MONTH and FISHERY
  mlvls <- unique(nofish$MONTH)
  flvls <- unique(nofish$FISHERY)
  # Summarized angling effort & success obs by MONTH, FISHERY, and DAYTYPE
  obsEff1 <- nofish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(nInts=dplyr::n(),
                     obsAnglers=sum(PERSONS),
                     obsHours=sum(HOURS),
                     obsAnglerHours=sum(PERSONS*HOURS),
                     obsSuccAnglers=sum(SUCCESS))
  # Summarized angling effort & success obs by MONTH and FISHERY, across DAYTYPEs.
  obsEff2 <- obsEff1 %>%
    dplyr::summarize(nInts=sum(nInts),
                     obsAnglers=sum(obsAnglers),
                     obsSuccAnglers=sum(obsSuccAnglers),
                     obsHours=sum(obsHours),
                     obsAnglerHours=sum(obsAnglerHours)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    select(names(obsEff1))
  # Summarized angling effort & success obs by MONTH, across DAYTYPEs and FISHERYs
  obsEff3 <- obsEff2 %>%
    dplyr::summarize(nInts=sum(nInts),
                     obsAnglers=sum(obsAnglers),
                     obsSuccAnglers=sum(obsSuccAnglers),
                     obsHours=sum(obsHours),
                     obsAnglerHours=sum(obsAnglerHours)) %>%
    dplyr::mutate(DAYTYPE="All",FISHERY="All") %>%
    select(names(obsEff1))
  # Summarized angling effort & success obs across DAYTYPEs, FISHERYs, & MONTHs
  obsEff4 <- obsEff3 %>%
    dplyr::summarize(nInts=sum(nInts),
                     obsAnglers=sum(obsAnglers),
                     obsSuccAnglers=sum(obsSuccAnglers),
                     obsHours=sum(obsHours),
                     obsAnglerHours=sum(obsAnglerHours)) %>%
    dplyr::mutate(DAYTYPE="All",FISHERY="All",MONTH="All") %>%
    select(names(obsEff1))
  # Combine, rearrange, and return
  as.data.frame(rbind(obsEff1,obsEff2,obsEff3,obsEff4)) %>%
    mutate(DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All")),
           FISHERY=factor(FISHERY,levels=c(flvls,"All")),
           MONTH=factor(MONTH,levels=c(mlvls,"All"))) %>%
    arrange(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE)
}

#
sumObsHarvest2 <- function(ints) {
  # Change MONTHs to a character
  ints <- mutate(ints,MONTH=as.character(MONTH))
  # Get levels of MONTH and FISHERY
  mlvls <- unique(ints$MONTH)
  flvls <- unique(ints$FISHERY)
  # Summarize harvest of each speices by MONTH, FISHERY, and DAYTYPE
  obsHarv1 <- ints %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(obsHarvest=sum(NUM))
  # Summarize harvest of each speices by MONTH and FISHERY, across DAYTYPEs
  obsHarv2 <- obsHarv1 %>%
    dplyr::summarize(obsHarvest=sum(obsHarvest)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(obsHarv1))
  # Summarize harvest of each speices by MONTH, across DAYTYPEs & FISHERYs
  obsHarv3 <- obsHarv2 %>%
    dplyr::summarize(obsHarvest=sum(obsHarvest)) %>%
    dplyr::mutate(DAYTYPE="All",FISHERY="All") %>%
    dplyr::select(names(obsHarv1))
  # Summarize harvest of each speices across DAYTYPEs, FISHERYs, and MONTHs
  obsHarv4 <- obsHarv3 %>%
    dplyr::summarize(obsHarvest=sum(obsHarvest)) %>%
    dplyr::mutate(DAYTYPE="All",FISHERY="All",MONTH="All") %>%
    dplyr::select(names(obsHarv1))
  # Combine, rearrange, and return
  as.data.frame(rbind(obsHarv1,obsHarv2,obsHarv3,obsHarv4)) %>%
    mutate(DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All")),
           FISHERY=factor(FISHERY,levels=c(flvls,"All")),
           MONTH=factor(MONTH,levels=c(mlvls,"All"))) %>%
    select(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE,obsHarvest) %>%
    arrange(SURVEY,ROUTE,UNIT,SPP,MONTH,FISHERY,DAYTYPE)
}


# Currently the harvest of all fish is stored as the variable NUM. This function
# moves this value to the FISH variable and adds a corresponding "All Fish" to
# the SPP variable. Thus, "All Fish" is effectively treated as another species,
# which makes it easier to summarize with SPP further below. Note that orig is
# the original interviews data.frame and nofish is the same except that NUM and
# SPP have been removed.
iHndlHarvestAllFish <- function(orig,nofish) {
  # Take data.frame that has no fish info (no SPP and FISH variables) and add
  # SPP with "All Fish" in it and rename the FISH (number of all fish harvested)
  # as NUM (which is the number of harvested fish of that species)
  tmp <- dplyr::mutate(nofish,SPP="All Fish") %>%
    dplyr::rename(NUM=FISH)
  # Remove the FISH variable (all fish harvested regardless of species) from the
  # original data.frame that has the harvest information for all species
  orig <- dplyr::select(orig,-FISH)
  # Row-bind the two data.frames together to create a new data.frame that has
  # "All Fish" in the SPP varible. However, first make sure that the variables
  # in both data.frames are in the same order. Finally, remeove all records for
  # which no SPP was recorded (i.e., this data.frame is used to estimate
  # harvest, so this is not needed).
  dplyr::select(tmp,names(orig)) %>%
    rbind(orig) %>%
    dplyr::filter(!is.na(SPP))
}