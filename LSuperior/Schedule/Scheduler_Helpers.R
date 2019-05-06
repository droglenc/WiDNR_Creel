## General ----
### Packages
library(dplyr)


### Reads all data into one list
readInfo <- function(fldr,CLERK) {
  ## Get main filename
  finfo <- paste0(fldr,"Schedule_Info_LS.xlsx")
  ## Get indivdidual sheets as data.frames
  clerks <- iReadClerks(finfo,CLERK)
  routes <- iReadRoutes(finfo,clerks)
  shifts <- iReadShifts(finfo,clerks)
  ## Return as a list of data.frames
  list(clerks=clerks,routes=routes,shifts=shifts)
}

### Reads clerk specific information.
iReadClerks <- function(f,CLERK) {
  ## Restrict to the chosen CLERK
  ## Reduce data.frame variables
  df <- readxl::read_excel(f,sheet="Clerks") %>%
    FSA::filterD(clerk==CLERK) %>%
    dplyr::select(route,clerk,ctime) %>%
    as.data.frame()
  df
}


### Reads route specific information.
iReadRoutes <- function(f,clerks) {
  ## Expand the list of months to individual months
  ## Add a site variable that is combo of site number & description
  ## Make months an ordered factor variable
  ## Restrict to only the routes found in clerks
  ## Order rows
  ## Reduce and reorder data.frame variables
  df <- readxl::read_excel(f,sheet="Routes") %>%
    iExpandMonthsList() %>%
    dplyr::mutate(site=paste0(site_descrip," (#",site_no,")"),
                  month=factor(month,levels=month.abb)) %>%
    FSA::filterD(route %in% clerks$route) %>%
    dplyr::arrange(route,month,visit) %>%
    dplyr::select(route,month,site,visit,travel,peffort) %>%
    as.data.frame()
  df
}

### Reads shift specific information
iReadShifts <- function(f,clerks) {
  ## Expand the list of months to individual months
  ## Make months an ordered factor variable
  ## Restrict to the CLERK asked for
  ## Order rows
  ## Reduce and reorder data.frame variables
  df <- readxl::read_excel(f,sheet="Shifts",col_types="text") %>%
    iExpandMonthsList() %>%
    dplyr::mutate(month=factor(month,levels=month.abb)) %>%
    FSA::filterD(route %in% clerks$route) %>%
    dplyr::arrange(route,month,shift) %>%
    dplyr::select(route,month,shift,start,end) %>%
    as.data.frame()
  df
}

### Determine if date is a holiday
is.holiday <- function(x) {
  if (!lubridate::is.Date(x)) stop("'x' must be a 'Date' object.",call.=FALSE)
  ## Find the month, weekday name, and day of the month for use below
  MONTH <- lubridate::month(x,label=TRUE)
  WDAY <- lubridate::wday(x,label=TRUE)
  MDAY <- lubridate::mday(x)
  ## Identify if one of the following holidays
  dplyr::case_when(
    MONTH=="Jan" & MDAY==1 ~ TRUE,                 # New Years Day
    MONTH=="May" & MDAY>=25 & WDAY=="Mon" ~ TRUE,  # Memorial Day
    MONTH=="Jul" & MDAY==4 ~ TRUE,                 # 4th of July
    MONTH=="Sep" & MDAY<=7 & WDAY=="Mon" ~ TRUE,   # Labor Day
    MONTH=="Nov" & MDAY>=22 & WDAY=="Thu" ~ TRUE,  # Thanksgiving
    MONTH=="Dec" & MDAY==25 ~ TRUE,                # Christmas
    TRUE ~ FALSE                                   # Not a holiday
  )
}


### Convert month number to abbreviation
iCheckMonth <- function(x) {
  if (is.numeric(x)) {
    if (x<1 | x>12) 
      stop("Month number in 'x' must be in 1:12.",call.=FALSE)
    x <- month.abb[x]
  }
  x
}

### Check if year is appropriate and within acceptable range
iCheckYear <- function(x,min.year=2010,max.year=2099) {
  if (length(x)>1) stop("Only one 'year' is allowed.",call.=FALSE)
  if (!is.numeric(x)) stop("'year' is not numeric.",call.=FALSE)
  if (any(length(grep("[^[:digit:]]", format(x[1], scientific = FALSE)))))
    stop("'year' is not an integer.",call.=FALSE)
  if (x<min.year) stop(paste("'year' is before",min.year),call.=FALSE)
  if (x>max.year) stop(paste("'year' is after",max.year),call.=FALSE)
  x
}


##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==
## Scheduling ----
### Find Monday closest to beginning of the month
iFindStartDate <- function(MONTH,YEAR) {
  ## Check inputs
  MONTH <- iCheckMonth(MONTH)
  YEAR <- iCheckYear(YEAR)
  ## Make the first of the month a date
  tmp <- lubridate::dmy(paste(1,MONTH,YEAR,sep="-"))
  ## Move that date back to the previous Monday
  #tmp <- tmp - (lubridate::wday(tmp,week_start=1)-1)
  tmp
}

### Finds Sunday closest to end of the month
iFindLastDate <- function(MONTH,YEAR) {
  ## Check inputs
  MONTH <- iCheckMonth(MONTH)
  YEAR <- iCheckYear(YEAR)
  ## Find the last day of the month
  ### Find the first day of following month ...
  NEXT_MONTH <- lubridate::month(lubridate::dmy(paste(1,MONTH,YEAR,sep="-"))) + 1
  if (NEXT_MONTH>12) {
    NEXT_MONTH <- 1
    YEAR <- YEAR+1
  }
  ### ... and subtract one for last day of month
  tmp <- lubridate::dmy(paste(1,NEXT_MONTH,YEAR,sep="-")) - 1
  ## Find day of the week and add what it takes to make it a Sunday
  #tmp <- tmp + (7-lubridate::wday(tmp,week_start=1))
  tmp
}

### Days for which a creel survey should be conducted.
iFindDays2Creel <- function(data) {
  ## Vector to hold results ... initialize with "YES"es ("NO"s added below)
  CREEL <- rep("YES",nrow(data))
  ## Initialize the index for the previous week's day off
  prevind <- NULL
  ## Create weights for sampling the weekdays to have off ... Mon should have
  ## double the weight of Tues-Thurs as it can only be selected as "off" in one
  ## way (as the first day off) whereas the others can be selected in two ways
  ## (as either the first or second day off).
  swghts <- c(2,1,1,2)
  ## Cycle through the weeks
  for (i in seq_len(max(data$WEEK))) {
    ### Isolate just the weekdays of that week
    x <- data[data$WEEK==i & data$DAYTYPE=="WEEKDAY",]
    ## If only a three day or less work week at the beginning or end of the
    ## season then creel all days ... else find two days off
    if (!((i==1 | i==max(data$WEEK) & nrow(x)<4))) {
      ## Find two consecutive weekdays "off" (no creel)
      ### Find first day off of previous week (is in ind from prev iteration)
      prevind <- ifelse(is.null(prevind),0,ind[1])
      ### Find weekday indices for the given week, but don't include Fridays
      ind <- x$WDAYn
      ind <- ind[ind!=5]
      ### Find all indices where there are two consecutive weekdays; i.e., don't
      ###   give one day off between a holiday and a weekend.
      ind <- ind[which(diff(x$WDAYn)==1)]
      ### Don't allow 1st day off of previous week to be 1st day off this week
      ind <- ind[ind!=prevind]
      ### If previously had off Thurs-Fri, then don't let off Mon-Tues (this
      ### should minimize the two consecutive work days)
      if (prevind==4 & (1 %in% ind)) ind <- ind[-1]
      ### If previously had off Mon-Tues, then don't let off Thurs-Fri (this
      ### should minimize the long consecutive work days)
      if (prevind==1 & (4 %in% ind)) ind <- ind[-length(ind)]
      junk <- paste("Week:",i,
                    "; Previous start day was",prevind,
                    "; Choices for current start",paste(ind,collapse=","))
      print(junk)
      ### randomly sample a "starting day off"
      if (length(ind)>1) ind <- sample(ind,1,prob=swghts[ind])
      ### then find the next day off (i.e., the day after the starting day off)
      ind <- c(ind,ind+1)
      ## Add a "NO" to CREEL for the two days off
      CREEL[data$WEEK==i & data$WDAY %in% x$WDAY[ind]] <- "NO"
    }
  }
  ## Return the vector
  CREEL
}

### Finds daily shifts according to the following assumptions:
###   1. Approx. equal number of shifts per weekday for the MONTH
###   2. Randomly select shift for first weekday/holiday of the week and then
###      alternate so a shift appears on at least one weekend day.
iFindShifts <- function(data) {
  ## Initialize vector of SHIFTs with blanks
  SHIFT <- character(nrow(data))
  ## Fill in SHIFTs on non-creel days with NA
  SHIFT[data$CREEL=="NO"] <- NA
  ## Process by month
  mos <- as.character(unique(data$MONTH))
  for (i in mos) {
    ### Handle weekdays
    #### Find rows to replace (is a creel, weekday, correct month)
    rows <- which(data$CREEL=="YES" & data$DAYTYPE=="WEEKDAY" & data$MONTH==i)
    #### Find the number of rows to replace
    nrows <- length(rows)
    #### Fill in SHIFTs on creel days with the randomized shift.
    SHIFT[rows] <- sample(rep(c("am","pm"),(floor(nrows)/2)+1),nrows)
    ### Handle weekends and holidays (together)
    rows <- which(data$CREEL=="YES" & data$DAYTYPE!="WEEKDAY" & data$MONTH==i)
    for (j in unique(data[rows,]$WEEK)) {
      rows <- which(data$CREEL=="YES" & data$DAYTYPE!="WEEKDAY" &
                      data$MONTH==i & data$WEEK==j)
      #### pick random start from 1st 2 positions of am/pm list below
      tmp <- sample(1:2,1)
      #### choose am/pm based on random start and number needed
      SHIFT[rows] <- c("am","pm","am","pm")[tmp:(tmp+length(rows)-1)]
    }
  } 
  ## Return the SHIFTs vector
  SHIFT
}

### Finds daily route according to the following assumptions:
###   1. For clerks with only one route, same route every day.
###   2. For clerks with two routes, choose random route for ...
###      a. first weekday for a two week period and then alternate (thus
###         same number of routes for two-week period, but not the same days
###         in more than two consecutive weeks).
###      b. first weekend/holiday for two-seek period and then other route for
###         other weekend day, and then reverse for the next weekend (thus
###         not same order on more than two weekends in a row).
iFindRoutes <- function(data,clerks) {
  ## Initialize vector of ROUTEs with blanks & then non-creel days with NA
  ROUTE <- character(nrow(data))
  ROUTE[data$CREEL=="NO"] <- NA
  ## Find the unique routes in data and sampling proportion
  ROUTES <- clerks$route
  CTIMES <- clerks$ctime
  CTIMES <- CTIMES/sum(CTIMES)

  if (length(ROUTES)==1) {
    ## If only one route, fill in ROUTES on creel days with that name
    rows <- which(data$CREEL=="YES")
    ROUTE[rows] <- rep(ROUTES,length(rows))
  } else {
    ## If two routes, then must do more work.
    wks <- seq(1,max(data$WEEK),2) 
    for (i in wks) {
      ### Handle weekends/holidays
      rows <- which(data$CREEL=="YES" & data$DAYTYPE!="WEEKDAY"
                    & data$WEEK %in% c(i,i+1))
      #### randomly order routes, then reverse, and add one more random choice
      #### in case it is a week with five weekends/holidays
      tmp <- sample(ROUTES,2)
      tmp <- c(tmp,rev(tmp),sample(ROUTES,1))
      ROUTE[rows] <- tmp[1:length(rows)]
      ### Handle weekdays
      rows <- which(data$CREEL=="YES" & data$DAYTYPE=="WEEKDAY"
                    & data$WEEK %in% c(i,i+1))
      #### randomly order routes, repeat as needed, pick as many as needed
      tmp <- sample(ROUTES,2)
      ROUTE[rows] <- rep(tmp,4)[1:length(rows)]
    }
  }
  ## Return the ROUTEs vector
  ROUTE
}

### Assigns unique number for a particular randomized bus route.
iAssignBusRouteIDs <- function(data) {
  ## Initialize vector of IDs with blanks
  IDs <- character(nrow(data))
  ## Fill in IDs on non-creel days with NA
  IDs[data$CREEL=="NO"] <- NA
  ## Find the number of days that will have a creel survey
  tmp <- nrow(data[data$CREEL=="YES",])
  ## Fill in IDs on creel days with a unique number
  IDs[data$CREEL=="YES"] <- 1:tmp
  ## Return the IDs vector
  IDs
}

### Produces (and prints) a summary of the generated schedule.
iSchedSummary <- function(sched) {
  ## Show frequency of ams and pms by month
  cat("\nFrequency of Days by Month and Day Type\n")
  print(addmargins(xtabs(~MONTH+SHIFT+ROUTE,
                         data=FSA::filterD(sched,CREEL=="YES")),
                   margin=1:2))
  ## Show frequency of day types by month
  cat("\nFrequency of Days by Month and Day Type\n")
  print(addmargins(xtabs(~MONTH+DAYTYPE+ROUTE,
                         data=FSA::filterD(sched,CREEL=="YES")),
                   margin=1:2))
  ## Show frequency of days of the week by month
  cat("\nFrequency of Days by Month and Day of the Week\n")
  print(addmargins(xtabs(~MONTH+WDAY+ROUTE,
                         data=FSA::filterD(sched,CREEL=="YES")),
                   margin=1:2))
  ## Show "days off" by month
  cat("\nFrequency of Days OFF by Month and Day of the Week\n")
  print(addmargins(xtabs(~MONTH+WDAY,
                         data=FSA::filterD(sched,CREEL=="NO")),
                   margin=1:2))
  ## Show lengths of runs of "YESes" (number of consecutive days worked)
  tmp <- rle(sched$CREEL)
  tmp <- table(tmp$values,tmp$lengths)["YES",]
  cat("Frequency of Consecutive Days Worked\n")
  print(tmp)
  cat("\n")
}

makeSchedule <- function(LAKE,YEAR,info,SEED,show_summary=TRUE) {
  ## Check inputs
  YEAR <- iCheckYear(YEAR)
  ## Set the random number seed
  if (!is.null(SEED)) set.seed(SEED)
  ## Set filename for schedule data
  fout <- paste(YEAR,toupper(LAKE),
                paste0(unique(info$clerks$route),collapse="_"),
                "schedule.csv",sep="_")
  ## Get start and end data from the information in info
  start_date <- iFindStartDate(info$routes$month[1],YEAR)
  end_date <- iFindLastDate(info$routes$month[nrow(info$routes)],YEAR)
  
  ## Create the schedule
  ### Find all dates between the start and end date
  ### Add a variable that contains the ROUTE name
  ### Find the week since start_date (beginning of the survey)
  ### Find weekdays, both in words and as a number
  ### Identify day types (weekend, holiday, or weekday)
  ### Determine on which days a creel survey should be conducted
  ### Randomly choose an am or pm shift for each day
  ### Add random bus route timing letters (within a month)
  sched <- data.frame(DATE=start_date+0:(end_date-start_date)) %>%
    dplyr::mutate(LAKE=LAKE,
                  MONTH=lubridate::month(DATE,label=TRUE),
                  WEEK=lubridate::isoweek(DATE)-lubridate::isoweek(DATE[1])+1,
                  WDAYn=lubridate::wday(DATE,week_start=1),
                  WDAY=lubridate::wday(DATE,week_start=1,label=TRUE),
                  DAYTYPE=dplyr::case_when(
                    chron::is.weekend(DATE) ~ "WEEKEND",
                    is.holiday(DATE) ~ "HOLIDAY",
                    TRUE ~ "WEEKDAY")) %>%
    tibble::add_column(CREEL=iFindDays2Creel(.)) %>%
    tibble::add_column(ROUTE=iFindRoutes(.,clerks)) %>%
    tibble::add_column(SHIFT=iFindShifts(.)) %>%
    tibble::add_column(DAILY_SCHED=iAssignBusRouteIDs(.)) %>%
    select(LAKE,ROUTE,DATE,MONTH,WEEK,WDAY,DAYTYPE,CREEL,SHIFT,DAILY_SCHED)
  ## Write schedule to CSV file
  write.csv(sched,file=fout,quote=FALSE,na="",row.names=FALSE)
  ## Show summaries if asked to
  if (show_summary) iSchedSummary(sched)
  ## Return the written filename
  fout
}

readSchedule <- function(f) {
  readr::read_csv(f)
}


##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==
## Calendar ----
### Packages
library(ggplot2)


### Makes the calendar header
iMakeCalHeader <- function(pth="") {
  g <- grid::rasterGrob(magick::image_read(paste0(pth,"WiDNR_logo.jpg")),
                        interpolate=TRUE)
  header <- data.frame(x=1:10,y=1) %>% 
    ggplot(aes(x,y)) +
    geom_blank() +
    annotation_custom(g,xmin=0,xmax=2.5,ymin=-Inf,ymax=Inf) +
    annotate("text",label="Lake Superior Creel Survey Schedule",x=2.25,y=1,
             size=6,hjust=0) +
    theme_void()
  header
}


makeCalendar <- function(sched,MONTH1,pth="",width,height,PDF=FALSE) {
  ## Make calendar header
  header <- iMakeCalHeader(pth)
  
  ## Read and modify schedule file
  ### Add an "activity" variable that combines route and shift (for printing)
  sched <- sched %>%
    dplyr::mutate(DATE=lubridate::ymd(DATE),
                  activity=ifelse(CREEL=="NO","NO CREEL",
                                  paste0(ROUTE,"\n",SHIFT,"\n(",
                                         DAILY_SCHED,")"))) %>%
    dplyr::filter(MONTH==MONTH1)
  ## Find year from the schedule
  YEAR <- lubridate::year(sched$DATE[1])
  ## Get first and last days of this month
  start_date <- lubridate::make_date(YEAR,which(month.abb==MONTH1))
  end_date <- start_date + months(1) - 1
  ## Get number of days in the month
  days <- lubridate::mday(end_date)-lubridate::mday(start_date)
  ## List of dates ...
  ## ... with date and activity
  mon_cal <- data.frame(DATE=start_date+0:days,x=0L,y=0L) %>%
    left_join(sched,by="DATE") %>%
    mutate(color=as.factor(ifelse(CREEL=="NO",1,0)),
           title=lubridate::month(DATE,label=TRUE,abbr=FALSE))
  ## Make calendar page for "this" month
  cal <- ggplot(mon_cal,aes(x=x,y=y)) + 
    ### makes each individual day (faceted below)
    geom_text(aes(label=activity,color=color),
              data=filter(mon_cal,!is.na(activity)),
              size=3,fontface="bold") +
    sugrrants::facet_calendar(~DATE,format="%a-%e") +
    labs(x="",y="",title=paste0(mon_cal$title[[1]],", ",YEAR)) +
    theme_bw() +
    theme(
      strip.background=element_rect(fill="yellow"),
      strip.text=element_text(color="black",face="bold"),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      axis.ticks=element_blank(),
      axis.text=element_blank()
    ) +
    guides(color="none") +
    scale_color_manual(values=c("black","red")) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))
  
  page <- header + patchwork::plot_spacer() + cal +
    patchwork::plot_layout(ncol=1,heights=c(0.4,0.2,3))
  print(page)
}



##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==
## Bus Routes ----
### Packages
library(kableExtra)

### The information files contains lists of months in one cell of the
### spreadsheet. This expands those lists by putting months in one row and
### repeating the other pertinent information.
iExpandMonthsList <- function(data) {
  ## Make empty data.frame with same structure as data
  data2 <- data[FALSE,]
  ## Expland the list of months
  for (i in 1:nrow(data)) {
    mons <- unlist(strsplit(data$month[i],", "))
    tmp <- data[rep(i,each=length(mons)),] 
    tmp$month <- mons
    data2 <- rbind(data2,tmp)
  }
  ## Return the expanded data.frame
  data2
}

## Reverse order of route information
iReverseRoute <- function(data) {
  ### adjust inter-site travel times
  data$travel <- c(data$travel[nrow(data)],data$travel[-nrow(data)])
  ### Reverse visit order
  data <- data[order(data$visit,decreasing=TRUE),]
  ### ... and then adjust the visit order (though this is not needed again)
  data$visit <- seq.int(nrow(data))
  data
}

## Adjusts efforts when rounding caused total effort to != total time in shift
iAdjustTimeAtSite <- function(ttlEffort,ttlTime,sites,peffort) {
  ### Find total effor time (after rounding)
  ttlTime2 <- sum(ttlEffort)
  ### If the two sum of the effort does not equal ttlTime (total time in the
  ### shift) then randomly select sites according to their peffort to either
  ### subtract or add a minute (depending how the sum matches the time in shift)
  ### from the total effort. Basically randomly adds or subtracts minutes from
  ### the most probable sites.
  if (ttlTime2!=ttlTime) {
    #### make a table of number of minutes to add/subtract from each site
    tmp <- table(sample(unique(sites),abs(ttlTime-ttlTime2),
                        replace=TRUE,prob=peffort/100))
    #### get indices of which sites will received the addition/subtraction
    tmp2 <- unique(sites) %in% rownames(tmp)
    #### do the addition/subtraction
    if (ttlTime<ttlTime2) ttlEffort[tmp2] <- ttlEffort[tmp2]-tmp
    else ttlEffort[tmp2] <- ttlEffort[tmp2]+tmp
  }
  ## Return the adjusted effort vector
  ttlEffort
}

## Make a route from first site to end (i.e., follow order of sites for route)
iMakeOrderedRoute <- function(sites,travel,ttlEffort) {
  ### Create "locations"
  #### Interleave sites with inter-site travel time notes
  ### And add first site onto end (like traveling back to beginning)
  locs <- ggplot2:::interleave(sites,paste0("TRAVEL (",travel," mins)"))
  locs <- c(locs,sites[1])
  
  ### Create times at each "location"
  #### Interleave times at site with travel times
  #### Put 0 at beginning (for beginning of route)
  #### Then find cumulative sum of times to show day progression
  times <- ggplot2:::interleave(ttlEffort,travel)
  times <- cumsum(c(0,times))
  
  ### Put together as a data.frame to return
  data.frame(TIME=times,LOCATION=locs)
}

## Adjust (wrap the route) for a random starting time
iAdjustRoute4RandomStart <- function(data,mins) {
  ### Find random starting time
  rnd <- base::sample(0:data$TIME[length(data$TIME)],1)
  ### Which "location" is split (last negative diff b/w times and random #)
  ### Add a new line for this time if no time equals random time
  if (!any((data$TIME-rnd)==0)) {
    data <- rbind(data,data[Position(isTRUE,(data$TIME-rnd)<0,right=TRUE),])
    data$TIME[nrow(data)] <- rnd
  }
  ### All times before rnd should have max time added to them
  ###   and then subtract rnd from each so that rnd time is set to 0
  data$TIME[data$TIME<rnd] <- data$TIME[data$TIME<rnd] + max(data$TIME)
  data$TIME <- data$TIME - rnd
  ### Add an "End of Shift" line
  data <- rbind(data,data.frame(TIME=mins,LOCATION="END OF SHIFT"))
  ### Then reorder, remove duplicate rows (from wrapping df), fix row numbers
  data <- data[order(data$TIME),]
  data <- data[!duplicated(data),]
  rownames(data) <- seq.int(nrow(data))
  ### Returned adjusted data.frame
  data
}


iMakeBusRoute <- function(routes,shifts,ROUTE,SHIFT,MONTH1,allow_reverse=TRUE) {
  ## Isolate the pertintent route information
  routeInfo <- routes[routes$route==ROUTE & routes$month==MONTH1,]
  ## Possibly (if allowed) change the visit order (based on a coin-flip)
  if (allow_reverse & runif(1)<0.5) routeInfo <- iReverseRoute(routeInfo)
  ## Get vectors of route information
  sites <- routeInfo$site
  travel <- routeInfo$travel
  peffort <- routeInfo$peffort
  
  ### Isolate the pertinent shift information
  shiftInfo <- shifts[shifts$route==ROUTE & shifts$month==MONTH1 & shifts$shift==SHIFT,]
  ## Find total minutes from start to end
  start <- as.POSIXct(shiftInfo$start,format="%H:%M")
  end <- as.POSIXct(shiftInfo$end,format="%H:%M")
  mins <- unclass(difftime(end,start,units="mins"))
  
  ## Convert peffort into minutes at each site
  ### Find total effort exclusive of total travel time
  ttlTime <- mins-sum(travel)
  ttlEffort <- round(ttlTime*peffort/100,0)
  ### Adjust effort slightly if the sum of rounded efforts != total time in shift
  ttlEffort <- iAdjustTimeAtSite(ttlEffort,ttlTime,sites,peffort)
  
  ## Make a route from first site to end
  ## Adjust (wrap the route) for a random starting time
  df <- iMakeOrderedRoute(sites,travel,ttlEffort)
  df <- iAdjustRoute4RandomStart(df,mins)
  
  ## Convert mins to actual time-of-day (must convert mins to secs)
  df$TIME <- format(start+df$TIME*60,format="%H:%M")
  
  ## Return data.frame
  df
}

iPrintBusRouteHeader <- function(x) {
  x <- x[,c("LAKE","ROUTE","SHIFT","DATE","DAYTYPE","DAILY_SCHED")]
  x$DATE <- format(lubridate::ymd(x$DATE),format="%m/%d/%Y")
  x$DAYTYPE <- paste(x$DAYTYPE,ifelse(x$DAYTYPE=="WEEKDAY","(1)","(2)"),sep=" ")
  x <- t(x)
  colnames(x) <- c("Information")
  ## Make Kable
  kt <- kable(x,booktabs=TRUE,linesep="") %>%
    kable_styling(full_width=FALSE,
                  position="left",
                  latex_options=c("hold_position")) %>%
    column_spec(1,bold=TRUE) %>%
    row_spec(0,bold=TRUE)
  ## Return huxtable
  kt
}

iPrintBusRoute <- function(brdf) {
  ## Add the ARRIVED, DEPARTED, and COUNT columns
  travORendRows <- grepl("TRAVEL",brdf$LOCATION) | grepl("END",brdf$LOCATION)
  brdf$ARRIVED <- ifelse(travORendRows,"","_ _ : _ _")
  brdf$DEPARTED <- brdf$ARRIVED
  brdf$COUNT <- ifelse(travORendRows,"","_ _ _")
  
  ## Make the kable
  kt <- kable(brdf,format="latex",booktabs=TRUE,linesep="") %>%
    kable_styling(full_width=FALSE,
                  position="left",
                  latex_options=c("hold_position")) %>%
    column_spec(1,bold=TRUE) %>%
    row_spec(0,bold=TRUE)
  ## Return kable
  kt
}
