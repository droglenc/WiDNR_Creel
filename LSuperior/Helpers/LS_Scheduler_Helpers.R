## General ----
### Packages
suppressPackageStartupMessages(library(dplyr))


### Print Calendars and Bus Routes
printForClerk <- function(LAKE,YEAR,CLERK,SEED,WDIR,RDIR,SCHED) {
  message("Writing file, please be patient ...",appendLF=FALSE)
  foutpre <- paste0(LAKE,"_",YEAR,"_",CLERK)
  try(detach("package:kableExtra",unload=TRUE),silent=TRUE)
  rmarkdown::render(input=paste0(WDIR,"Helpers/LS_Scheduler_Template.Rmd"),
                    params=list(LAKE=LAKE,YEAR=YEAR,CLERK=CLERK,
                                SEED=SEED,SCHED=SCHED,WDIR=WDIR),
                    output_file=paste0(foutpre,".pdf"),
                    output_dir=RDIR,
                    output_format="pdf_document",
                    clean=FALSE,quiet=TRUE)
  ## Remove kableExtra package so that the function can be run again w/o error
  detach("package:kableExtra",unload=TRUE)
  ## Remove an intermediate directory and files
  unlink(paste0(RDIR,foutpre,"_files"),recursive=TRUE)
  unlink(paste0(RDIR,foutpre,".tex"))
  message("Done. See",paste0(foutpre,'.pdf'),"\n   in",RDIR)
}

### Reads all data into one list
readInfo <- function(CLERK,WDIR) {
  ## Get main filename
  fn <- paste0(WDIR,"Helpers/LS_Scheduler_Info.xlsx")
  ## Get indivdidual sheets as data.frames
  clerks <- iReadClerks(fn,CLERK)
  routes <- iReadRoutes(fn,clerks)
  shifts <- iReadShifts(fn,clerks)
  ## Return as a list of data.frames
  list(clerks=clerks,routes=routes,shifts=shifts)
}

### Reads clerk specific information.
iReadClerks <- function(fn,CLERK) {
  df <- readxl::read_excel(fn,sheet="Clerks") %>%
    ## Restrict to the chosen CLERK
    FSA::filterD(clerk==CLERK) %>%
    ## Reduce data.frame variables
    dplyr::select(route,clerk,ctime)
  as.data.frame(df)
}


### Reads route specific information.
iReadRoutes <- function(fn,clerks) {
  df <- readxl::read_excel(fn,sheet="Routes") %>%
    ## Expand the list of months to individual months
    iExpandMonthsList() %>%
    ## Add a site variable that is combo of site number & description
    ## Make months an ordered factor variable
    dplyr::mutate(site=paste0(site_descrip," (#",site_no,")"),
                  month=factor(month,levels=month.abb)) %>%
    ## Restrict to only the routes found in clerks
    FSA::filterD(route %in% clerks$route) %>%
    ## Order rows
    dplyr::arrange(route,month,visit) %>%
    ## Reduce and reorder data.frame variables
    dplyr::select(route,month,site,visit,travel,peffort)
  as.data.frame(df)
}

### Reads shift specific information
iReadShifts <- function(fn,clerks) {
  df <- readxl::read_excel(fn,sheet="Shifts",col_types="text") %>%
    ## Expand the list of months to individual months
    iExpandMonthsList() %>%
    ## Make months an ordered factor variable
    dplyr::mutate(month=factor(month,levels=month.abb)) %>%
    ## Restrict to the CLERK asked for
    FSA::filterD(route %in% clerks$route) %>%
    ## Order rows
    dplyr::arrange(route,month,shift) %>%
    ## Reduce and reorder data.frame variables
    dplyr::select(route,month,shift,start,end)
  as.data.frame(df)
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
### Finds first day of the given MONTH in the given YEAR
iFindStartDate <- function(MONTH,YEAR) {
  ## Check inputs
  MONTH <- iCheckMonth(MONTH)
  YEAR <- iCheckYear(YEAR)
  ## Make the first of the month a date
  lubridate::dmy(paste(1,MONTH,YEAR,sep="-"))
}

### Finds last day of the given MONTH in the given YEAR
iFindLastDate <- function(MONTH,YEAR) {
  ## Check inputs
  MONTH <- iCheckMonth(MONTH)
  YEAR <- iCheckYear(YEAR)
  ## Find the first day of following month ...
  NEXT_MONTH <- lubridate::month(lubridate::dmy(paste(1,MONTH,YEAR,sep="-"))) + 1
  if (NEXT_MONTH>12) {
    NEXT_MONTH <- 1
    YEAR <- YEAR+1
  }
  ## ... and subtract one for last day of MONTH
  lubridate::dmy(paste(1,NEXT_MONTH,YEAR,sep="-")) - 1
}

### Days for which a creel survey should be conducted.
iFindDays2Creel <- function(data) {
  ## Vector of weekday names
  WKDY_NAMES <- c("Mon","Tue","Wed","Thu","Fri")
  ## Vector to hold results ... initialize with "YES"es ("NO"s added below)
  CREEL <- rep("YES",nrow(data))
  ## Initialize the index for the previous week's day off
  prevDOFF <- NULL
  ## Create weights for sampling the weekdays to have off ... Mon/Thurs should
  ## have at least double the weight of Tues/Wed because Mon can only be
  ## selected as "off" in one way (as the first day off) and Fri can only be
  ## selected as "off" in one way (if Thurs is selected).
  swghts <- c(4,1,1,4)
  ## Cycle through the weeks
  maxWeek <- max(data$WEEK)
  for (i in seq_len(maxWeek)) {
    ### Isolate just the weekdays of that week
    DOFF <- FSA::filterD(data,WEEK==i,DAYTYPE=="WEEKDAY")$WDAY
    ## If only a three day or less work week at the beginning or end of the
    ## season then creel all days ... else find two days off
    if (!((i==1 | i==maxWeek & length(DOFF)<4))) {
      ## Find two consecutive weekdays "off" (no creel)
      ### Find weekday indices for the given week
      DOFF <- which(WKDY_NAMES %in% DOFF)
      ### Find all indices where there are two consecutive weekdays; i.e., don't
      ###   give one day off between a holiday and a weekend.
      DOFF <- DOFF[which(diff(DOFF)==1)]
      ### Find first day off of previous week
      ### And don't allow it to be the first day off this week
      if (!is.null(prevDOFF)) {
        prevDOFFind <- which(WKDY_NAMES %in% prevDOFF)
        DOFF <- DOFF[DOFF!=prevDOFFind]  
      }
      ### If previously had off Thurs-Fri, then don't let off Mon-Tues (this
      ### should minimize the two consecutive work days). If previously had off
      ### Mon-Tues, then don't let off Thurs-Fri (this should minimize the long
      ### consecutive work days)
      if (!is.null(prevDOFF)) {
        if (prevDOFF=="Thu" & (1 %in% DOFF)) DOFF <- DOFF[DOFF!=1]
        if (prevDOFF=="Mon" & (4 %in% DOFF)) DOFF <- DOFF[DOFF!=4]
      }
      ### randomly sample a "starting day off"
      if (length(DOFF)>1) DOFF <- sample(DOFF,1,prob=swghts[DOFF])
      ### then find the next day off (i.e., the day after the starting day off)
      DOFF <- c(DOFF,DOFF+1)
      ## Add a "NO" to CREEL for the two days off
      CREEL[data$WEEK==i & data$WDAY %in% WKDY_NAMES[DOFF]] <- "NO"
      ## Remember previous day off
      prevDOFF <- WKDY_NAMES[DOFF[1]]
    }
  }
  ## Return the vector
  CREEL
}

### Finds daily shifts according to assumptions in scheduler_assumptions doc.
iFindShifts <- function(data) {
  ## Initialize vector of SHIFTs with blanks, put NA for non-creel days
  SHIFT <- character(nrow(data))
  SHIFT[data$CREEL=="NO"] <- NA
  
  ## Process by months within routes
  for (i in as.character(unique(data$ROUTE))) {
    for (j in as.character(unique(data$MONTH))) {
      ### Handle weekdays
      #### Rows to replace (a creel, a weekday, correct route & month)
      rows <- which(data$CREEL=="YES" & data$DAYTYPE=="WEEKDAY" &
                      data$ROUTE==i & data$MONTH==j)
      nrows <- length(rows)
      #### Fill in SHIFTs on creel days with the randomized shift ... a set of
      #### shifts is created (chcs) with equal numbers of am and pm (may have
      #### one more of either if nrows is odd), these are then shuffled and
      #### placed into the SHIFT vector. Thus there should be roughly equal
      #### numbers of am and pm per month per route
      chcs <- rep(c("am","pm"),(floor(nrows)/2))
      if (length(chcs)!=nrows) chcs <- c(chcs,sample(c("am","pm"),1))
      SHIFT[rows] <- sample(chcs)
      
      ### Handle weekends and holidays ... same process as above for weekeday
      rows <- which(data$CREEL=="YES" & data$DAYTYPE!="WEEKDAY" &
                      data$ROUTE==i & data$MONTH==j)
      nrows <- length(rows)
      chcs <- rep(c("am","pm"),(floor(nrows)/2))
      if (length(chcs)!=nrows) chcs <- c(chcs,sample(c("am","pm"),1))
      SHIFT[rows] <- sample(chcs)
    }
  }
  ## Return SHIFTs vector
  SHIFT
}

### Finds daily route according to assumptions in schedular_assumptions doc
iFindRoutes <- function(data,clerks) {
  ## Initialize vector of ROUTEs with blanks & non-creel days with NA
  ROUTE <- character(nrow(data))
  ROUTE[data$CREEL=="NO"] <- NA
  ## Find the unique routes in data and sampling proportion (make sum to 100%)
  ROUTES <- clerks$route
  CTIMES <- clerks$ctime
  CTIMES <- CTIMES/sum(CTIMES)

  if (length(ROUTES)==1) {
    ## If only one route, fill in ROUTES on creel days with that name
    rows <- which(data$CREEL=="YES")
    ROUTE[rows] <- rep(ROUTES,length(rows))
  } else {
    ## If two routes, then must do more work.
    ### Going to sequence by two-week periods
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

### Produces (and prints) a summary of the generated schedule.
iSchedSummary <- function(d) {
  ## Put weekends and holidays together
  d <- dplyr::mutate(d,DAYTYPE2=FSA::mapvalues(DAYTYPE,
                                               from=c("HOLIDAY","WEEKEND"),
                                               to=c("WKEND/HOL","WKEND/HOL")))
  ## Get the Yes and No creel days
  dyes <- FSA::filterD(d,CREEL=="YES")
  dno <- FSA::filterD(d,CREEL=="NO")
  
  ## Separate summaries by route
  for (i in unique(d$ROUTE[complete.cases(d$ROUTE)])) {
    cat("\n\nSchedule Summaries for Route: ",i,"\n")

    cat("\nFrequency of Days by Month, Shift, and Day Type\n")
    tmp <- xtabs(~MONTH+SHIFT+DAYTYPE2,data=FSA::filterD(dyes,ROUTE==i))
    print(addmargins(tmp,margin=1:2))
    
    cat("\nFrequency of WORK Days by Month and Day of the Week\n")
    tmp <- xtabs(~MONTH+WDAY,data=FSA::filterD(dyes,ROUTE==i))
    print(addmargins(tmp,margin=1:2))
  }
  cat("\nFrequency of OFF Days by Month and Day of the Week\n")
  tmp <- xtabs(~MONTH+WDAY,data=dno)
  print(addmargins(tmp,margin=1:2))

  cat("\nFrequency of Consecutive Days Worked (all routes)\n")
  tmp <- rle(d$CREEL)
  tmp <- table(tmp$values,tmp$lengths)["YES",]
  print(tmp)
  cat("\n")
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

makeSchedule <- function(LAKE,YEAR,CLERK,SEED,WDIR,RDIR,
                         show_summary=TRUE,show_calendars=TRUE) {
  ## Check inputs
  YEAR <- iCheckYear(YEAR)
  ## Set the random number seed
  if (!is.null(SEED)) set.seed(SEED)
  ## get the required information
  info <- readInfo(CLERK,WDIR)
  ## Set filename for schedule data
  if (!dir.exists(RDIR)) dir.create(RDIR)
  fout <- paste0(RDIR,LAKE,"_",YEAR,"_",CLERK,"_","schedule.csv")
  ## Get start and end data from the information in info
  start_date <- iFindStartDate(info$routes$month[1],YEAR)
  end_date <- iFindLastDate(info$routes$month[nrow(info$routes)],YEAR)
  
  ## Create the schedule
  ### Find all dates between the start and end date
  sched <- data.frame(DATE=start_date+0:(end_date-start_date)) %>%
    ### Find the month
    ### Find the week since start_date (beginning of the survey)
    ### Find days of week name
    ### Identify day types (weekend, holiday, or weekday)
    dplyr::mutate(MONTH=lubridate::month(DATE,label=TRUE),
                  WEEK=lubridate::isoweek(DATE)-lubridate::isoweek(DATE[1])+1,
                  WDAY=lubridate::wday(DATE,week_start=1,label=TRUE),
                  DAYTYPE=dplyr::case_when(
                    chron::is.weekend(DATE) ~ "WEEKEND",
                    is.holiday(DATE) ~ "HOLIDAY",
                    TRUE ~ "WEEKDAY")) %>%
    ### Determine on which days a creel survey should be conducted
    tibble::add_column(CREEL=iFindDays2Creel(.)) %>%
    ### Add a variable that contains the ROUTE name
    tibble::add_column(ROUTE=iFindRoutes(.,info$clerks)) %>%
    ### Determine am or pm shift for each day
    tibble::add_column(SHIFT=iFindShifts(.)) %>%
    ### Select perticular variables.
    select(WEEK,MONTH,DATE,WDAY,DAYTYPE,SHIFT,ROUTE,CREEL)
  ## Write schedule to CSV file
  write.csv(sched,file=fout,quote=FALSE,na="",row.names=FALSE)
  ## Show summaries if asked to
  if (show_summary) iSchedSummary(sched)
  ## Show calendars if asked to
  if (show_calendars) {
    cat("Printing preliminary calendars (look for separate window) ...")
    if (!"windows" %in% names(dev.list())) windows(width=7,height=7,record=TRUE)
    for (i in as.character(unique(sched$MONTH)))
      makeCalendar(sched,MONTH1=i,WDIR,add_DS=FALSE,
                   header="Lake Superior Creel Schedule - PRELIMINARY")
    cat(" DONE!!\n\n")
  }
  ## Return the written filename
  cat("The preliminary schedule written to",fout,"\n")
  fout
}

readSchedule <- function(fn,WDIR,show_summary=TRUE,
                         show_calendars=FALSE,new_window=TRUE) {
  ## Read schedule file and add a column for daily schedule
  d <- readr::read_csv(fn,col_types="dccccccc") %>%
    dplyr::mutate(DATE=lubridate::parse_date_time(DATE,c("ymd","mdy")),
                  DATE=lubridate::ymd(DATE)) %>%
    tibble::add_column(DAILY_SCHED=iAssignBusRouteIDs(.))
  ## Show summaries if asked to
  if (show_summary) iSchedSummary(d)
  ## Show calendars if asked to
  if (show_calendars) {
    win_exists <- ifelse("windows" %in% names(dev.list()),TRUE,FALSE)
    if (new_window | !win_exists) windows(width=7,height=7,record=TRUE)
    for (i in as.character(unique(d$MONTH)))
      makeCalendar(d,MONTH1=i,WDIR,add_DS=TRUE)
  }
  ## Return data.frame
  invisible(d)
}


##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==##==--==
## Calendar ----
### Packages
suppressPackageStartupMessages(library(ggplot2))


### Makes the calendar header
iMakeCalHeader <- function(header,WDIR) {
  fn <- paste0(WDIR,"helpers/WiDNR_logo.jpg")
  g <- grid::rasterGrob(magick::image_read(fn),interpolate=TRUE)
  header <- data.frame(x=1:10,y=1) %>% 
    ggplot(aes(x,y)) +
    geom_blank() +
    annotation_custom(g,xmin=0,xmax=2.5,ymin=-Inf,ymax=Inf) +
    annotate("text",label=header,x=2.25,y=1,
             size=6,hjust=0) +
    theme_void()
  header
}


makeCalendar <- function(d,MONTH1,WDIR,
                         header="Lake Superior Creel Schedule",
                         width,height,add_DS=TRUE) {
  ## Make calendar header
  header <- iMakeCalHeader(header,WDIR)
  
  ## Read and modify schedule file
  d <- dplyr::filter(d,MONTH==MONTH1)
  ### Add an "activity" variable that combines route, shift, and, optionally,
  ### the daily bus route schedule number. Will say "NO CREEL" for days off.
  tmp <- paste0(d$ROUTE,"\n",d$SHIFT)
  if (add_DS) tmp <- paste0(tmp,"\n(",d$DAILY_SCHED,")")
  tmp[d$CREEL=="NO"] <- "NO CREEL"
  d <- mutate(d,activity=tmp)
    
  ## Find year from the schedule
  YEAR <- lubridate::year(d$DATE[1])
  ## Get first and last days of this month
  start_date <- lubridate::make_date(YEAR,which(month.abb==MONTH1))
  end_date <- start_date + months(1) - 1
  ## Get number of days in the month
  days <- lubridate::mday(end_date)-lubridate::mday(start_date)
  ## List of dates ...
  ## ... with date and activity
  mon_cal <- data.frame(DATE=start_date+0:days,x=0L,y=0L) %>%
    left_join(d,by="DATE") %>%
    mutate(color=as.factor(ifelse(CREEL=="NO",1,0)),
           title=lubridate::month(DATE,label=TRUE,abbr=FALSE))
  ## Make calendar page for "this" month
  cal <- ggplot(mon_cal,aes(x=x,y=y)) + 
    ### makes each individual day (faceted below)
    geom_text(aes(label=activity,color=color),
              data=filter(mon_cal,!is.na(activity)),
              size=3,fontface="bold") +
    sugrrants::facet_calendar(~DATE,format="%a-%e",week_start=7) +
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


iMakeBusRoute <- function(info,LAKE,ROUTE,SHIFT,MONTH1,allow_reverse=TRUE) {
  routes <- info$routes
  shifts <- info$shifts
  ## Isolate the pertintent route information
  routeInfo <- routes[routes$route==ROUTE & routes$month==MONTH1,]
  ## Possibly (if allowed) change the visit order (based on a coin-flip)
  if (allow_reverse & runif(1)<0.5) routeInfo <- iReverseRoute(routeInfo)
  ## Get vectors of route information
  sites <- routeInfo$site
  travel <- routeInfo$travel
  peffort <- routeInfo$peffort
  
  ### Isolate the pertinent shift information
  shiftInfo <- shifts[shifts$route==ROUTE & shifts$month==MONTH1 &
                        shifts$shift==SHIFT,]
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
  df <- iMakeOrderedRoute(sites,travel,ttlEffort) %>%
    ## Adjust (wrap the route) for a random starting time
    iAdjustRoute4RandomStart(mins) %>%
    ## Convert mins to actual time-of-day (must convert mins to secs)
    mutate(TIME=format(start+TIME*60,format="%H:%M")) %>%
    ## Add data entry locations
    mutate(ARRIVED=ifelse(grepl("TRAVEL",LOCATION) | grepl("END",LOCATION),
                          "","_ _ : _ _"),
           DEPARTED=ARRIVED,
           COUNT=ifelse(grepl("TRAVEL",LOCATION) | grepl("END",LOCATION),
                        "","_ _ _"))
  
  ## Return data.frame
  df
}

iPrintBusRoute <- function(brdf) {
  travORendRows <- grepl("TRAVEL",brdf$LOCATION) | grepl("END",brdf$LOCATION)
  ## Make the kable
  kt <- knitr::kable(brdf,format="latex",booktabs=TRUE,
                     linesep="",align=c("l","l","c","c","c")) %>%
    kableExtra::kable_styling(full_width=FALSE,
                              position="left",
                              latex_options=c("hold_position")) %>%
    kableExtra::column_spec(1,bold=TRUE) %>%
    kableExtra::row_spec(0,bold=TRUE) %>%
    kableExtra::row_spec(which(travORendRows),background="#EAEBED")
  ## Return kable
  kt
}
