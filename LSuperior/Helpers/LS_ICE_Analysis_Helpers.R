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

#
sumPressureCount <- function(d) {
  tmp1 <- d %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     ttlVehFshry=sum(ttlVehSiteFshry,na.rm=TRUE))
  tmp2 <- tmp1 %>%
    dplyr::summarize(NINTS=sum(NINTS,na.rm=TRUE),
                     ttlVehFshry=sum(ttlVehFshry,na.rm=TRUE)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(tmp1))
  as.data.frame(rbind(tmp1,tmp2)) %>%
    mutate(DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All"))) %>%
    arrange(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE)
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
sumObsEffortSuccess <- function(nofish) {
  # Summarized observations about angling effort and success by MONTH, FISHERY,
  # and DAYTYPE.
  obsEff1 <- nofish %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
    dplyr::summarize(nInts=dplyr::n(),
                     obsAnglers=sum(PERSONS),
                     obsHours=sum(HOURS),
                     obsAnglerHours=sum(PERSONS*HOURS),
                     obsSuccAnglers=sum(SUCCESS))
  # Summarized observations about angling effort and success by MONTH and FISHERY,
  # across DAYTYPEs.
  obsEff2 <- obsEff1 %>%
    dplyr::summarize(nInts=sum(nInts),
                     obsAnglers=sum(obsAnglers),
                     obsSuccAnglers=sum(obsSuccAnglers),
                     obsHours=sum(obsHours),
                     obsAnglerHours=sum(obsAnglerHours)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    select(names(obsEff1))
  # Combine and return
  rbind(obsEff1,obsEff2) %>%
    mutate(DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All"))) %>%
    arrange(SURVEY,ROUTE,UNIT,MONTH,FISHERY,DAYTYPE) %>%
    as.data.frame()
}

#
sumObsHarvest <- function(ints) {
  # Summarize harvest of each speices by MONTH, FISHERY, and DAYTYPE
  obsHarv1 <- ints %>%
    dplyr::group_by(SURVEY,ROUTE,UNIT,MONTH,FISHERY,SPP,DAYTYPE) %>%
    dplyr::summarize(obsHarvest=sum(NUM))
  # Summarize harvest of each speices by MONTH and FISHERY, across DAYTYPEs
  obsHarv2 <- obsHarv1 %>%
    dplyr::summarize(obsHarvest=sum(obsHarvest)) %>%
    dplyr::mutate(DAYTYPE="All") %>%
    dplyr::select(names(obsHarv1))
  # Combine and return
  rbind(obsHarv1,obsHarv2) %>%
    mutate(DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All"))) %>%
    arrange(SURVEY,ROUTE,UNIT,MONTH,FISHERY,SPP,DAYTYPE) %>%
    as.data.frame()
}

#
expandHarvest <- function(h,f,pc) {
  dplyr::full_join(h,f,
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
    dplyr::full_join(pc,by=c("SURVEY","ROUTE","UNIT","MONTH","FISHERY","DAYTYPE")) %>%
    dplyr::select(-NINTS) %>%
    dplyr::mutate(ttlAnglers=ttlVehFshry*obsAnglersPerInt,
                  ttlAnglerHours=ttlAnglers*obsAvgAnglerHours,
                  ttlSuccAnglers=ttlAnglers*obsPercSucc,
                  ttlHarvest=ttlAnglerHours*obsHrate,
                  DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All"))) %>%
    dplyr::arrange(SURVEY,ROUTE,UNIT,MONTH,SPP,FISHERY,DAYTYPE) %>%
    as.data.frame()
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

# Currently the harvest of all fish is stored as the variable NUM. This function
# moves this value to FISH variable and adds a corresponding "ALL FISH" to the
# SPP variable. Thus, "ALL FISH" is effectively treated as another species,
# which makes it easier to summarize with SPP further below.
iHndlHarvestAllFish <- function(orig,nofish) {
  tmp <- dplyr::mutate(nofish,SPP="ALL FISH") %>%
    dplyr::rename(NUM=FISH)  ## mod
  orig <- dplyr::select(orig,-FISH) #orig1
  tmp <- select(tmp,names(orig)) %>%
    rbind(orig) %>%
    filter(!is.na(SPP))
  tmp
}

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
                       "number of vehicles at each site by month and day type."))
  tables(name="Table2",
         caption=paste("Number of interviews, proportion of interviews at that",
                       "a site in each fishery type, and estimated total",
                       "(i.e., expanded) number of vehicles in each fishery",
                       "and across all fishery types at each site by month",
                       "and day type."))
  tables(name="Table3",
         caption=paste("Total effort and harvest summary for ALL FISH combined."))
  
  ## Not needed as the main info is in the now Table 1.
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
    dplyr::mutate(# Remove repeated rows in MONTH and DAYTYPE variables
      MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use="MONTH"),
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
    set_number_format(row=everywhere,col=contains("pInts"),value=2) %>%
    # Extra label at the top
    rbind(c("MONTH","DAYTYPE","SITE",
            rep(c("Ints","Prop","Veh",""),numf),
            "Ints","Veh"),.) %>%
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
  ## Prepare data.frame for huxtable
  tbl1 <- H %>%
    dplyr::filter(SPP=="ALL FISH") %>%
    dplyr::select(FISHERY,MONTH,DAYTYPE,
                  ttlVehFshry,obsAnglersPerInt,ttlAnglers,
                  obsAvgAnglerHours,ttlAnglerHours,obsPercSucc,ttlSuccAnglers,
                  obsHrate,ttlHarvest) %>%
    dplyr::mutate(DAYTYPE=factor(DAYTYPE,levels=c("Weekday","Weekend","All"))) %>%
    dplyr::arrange(FISHERY,MONTH,DAYTYPE) %>%
    dplyr::mutate(# Remove repeated rows in MONTH and FISHERY variables
      MONTH=ifelse(!FSA::repeatedRows2Keep(.,cols2use="MONTH"),
                   "",as.character(MONTH)),
      FISHERY=ifelse(!FSA::repeatedRows2Keep(.,cols2use="FISHERY"),
                   "",as.character(FISHERY)))
      
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
    set_number_format(row=everywhere,col=c(5,7,9),value=2) %>%
    set_number_format(row=everywhere,col=11,value=4) %>%
    # Extra label at the top
    rbind(c("FISHERY","MONTH","DAYTYPE","Vehicles","per Int","Anglers",
            "Hours","Angler Hrs","Success","Anglers","Rate","Harvest"),.) %>%
    rbind(c("","","","Total","Angler","Total",
            "Avg","Total","Prop","Success","Harvest","Total"),.) %>%
    set_align(row=1:2,col=everywhere,value="center") %>%
    # highlight harvest columns
    set_background_color(row=everywhere,col=11:12,value="gray95") %>%
    # Sets table & column widths
    set_width(0.9) %>%
    set_col_width(col=c(.2,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1)) %>%
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
