## Load often used packages
library(dplyr)
library(ggplot2)


## General helpers ----
### Determine if a date is one of the identified holidays
is.holiday <- function(x) {
  if (!lubridate::is.Date(x)) stop("'x' must be a 'Date' object.",call.=FALSE)
  ## Find the month, weekday name, and day of the month for use below
  MONTH <- lubridate::month(x,label=TRUE,abbr=FALSE)
  WDAY <- lubridate::wday(x,label=TRUE,abbr=FALSE)
  MDAY <- lubridate::mday(x)
  ## Identify if it is one of the following holidays
  dplyr::case_when(
    MONTH=="January" & MDAY==1 ~ TRUE,                      # New Years Day
    MONTH=="May" & MDAY>=25 & WDAY=="Monday" ~ TRUE,        # Memorial Day
    MONTH=="July" & MDAY==4 ~ TRUE,                         # 4th of July
    MONTH=="September" & MDAY<=7 & WDAY=="Monday" ~ TRUE,   # Labor Day
    MONTH=="November" & MDAY>=22 & WDAY=="Thursday" ~ TRUE, # Thanksgiving
    MONTH=="December" & MDAY==25 ~ TRUE,                    # Christmas
    TRUE ~ FALSE                                            # Not a holiday
  )
}

### Converts month number to abbreviation
iCheckMonth <- function(x) {
  if (is.numeric(x)) {
    if (x<1 | x>12) 
      stop("Month number in 'x' is incorrect (<0 or >12).",call.=FALSE)
    x <- month.abb[x]
  }
  x
}

### Make sure year is appropriate and within range of acceptable years
iCheckYear <- function(x,min.year=2010,max.year=2099) {
  if (length(x)>1) stop("Only one 'year' is allowed.",call.=FALSE)
  if (!is.numeric(x)) stop("'year' is not numeric.",call.=FALSE)
  if (any(length(grep("[^[:digit:]]", format(x[1], scientific = FALSE)))))
    stop("'year' is not an integer.",call.=FALSE)
  if (x<min.year) stop(paste("'year' is before",min.year),call.=FALSE)
  if (x>max.year) stop(paste("'year' is after",max.year),call.=FALSE)
  x
}




## Helpers for making the schedule ----
### Finds the Monday closest to the beginning of the given month
iFindStartDate <- function(month,year) {
  ## Check inputs
  month <- iCheckMonth(month)
  year <- iCheckYear(year)
  ## Make the first of the month a date
  tmp <- lubridate::dmy(paste(1,month,year,sep="-"))
  ## Move that date back to the previous Monday
  tmp - (lubridate::wday(tmp,week_start=1)-1)
}

### Finds the Sunday closest to the end of the given month
iFindLastDate <- function(month,year) {
  ## Check inputs
  month <- iCheckMonth(month)
  year <- iCheckYear(year)
  ## Find the last day of the month
  ### Find the day of the first day of the following month ...
  m <- lubridate::month(lubridate::dmy(paste(1,month,year,sep="-")))
  if (m<12) {
    m <- m+1
  } else {
    m <- 1
    year <- year+1
  }
  ### ... and then subtract one to get the last day of the month
  tmp <- lubridate::dmy(paste(1,m,year,sep="-")) - 1
  ## Find day of the week and add what it takes to make it a Sunday
  tmp + (7-lubridate::wday(tmp,week_start=1))
}

### Find the days for which a creel survey should be conducted. Assumes:
###   - Determination is on a weekly basis
###   - Creel survey for every weekend day and holiday
###   - Two consecutive weekdays will be "off" during each week
iFindDays2Creel <- function(data) {
  ## Vector to hold results ... initialize with "YES"es ("NO"s added below)
  CREEL <- rep("YES",nrow(data))
  ## Cycle through the weeks
  for (i in unique(data$WEEK)) {
    ### Isolate just that week's data
    x <- dplyr::filter(data,WEEK==i)
    ## Find the weekdays in x
    tmp <- x[x$DAYTYPE=="WEEKDAY",]
    ## Find two consecutive weekdays "off" (no creel)
    ### Find all indices where there are two consecutive weekdays
    ### i.e., don't give one day off between a holiday and a weekend
    ### also ignore the last weekeday because it cannot be the first weekday
    ###      of the two days off
    ind <- which(diff(tmp$WDAYn)==1)
    ### randomly sample a "starting day off"
    ind <- sample(ind,1)
    ### then find the next day off (i.e., the day after the starting day off)
    ind <- c(ind,ind+1)
    ## Add a "NO" to CREEL for the two days off
    CREEL[data$WEEK==i & data$WDAY %in% tmp$WDAY[ind]] <- "NO"
  }
  ## Return the vector
  CREEL
}

### Randomly shuffles a roughly equal number of "am"s and "pm"s. Thus, the
### shift for any given day is random but the total number of "am"s and "pm"s
### is restricted to be very nearly equal
iFindShifts <- function(data) {
  tmp <- data[data$CREEL=="YES",]
  aps <- sample(rep(c("am","pm"),(floor(nrow(tmp))/2)+1),nrow(tmp))
  SHIFT <- character(nrow(data))
  SHIFT[data$CREEL=="NO"] <- NA
  SHIFT[data$CREEL=="YES"] <- aps
  SHIFT
}

### Assigns letter for a particular randomized bus route within a month.
iAssignBusRouteLetters <- function(data) {
  mons <- lubridate::month(data$date)
  BR <- character(nrow(data))
  for (i in unique(mons)) {
    tmp <- data[lubridate::month(data$date)==i & data$CREEL=="YES",]
    BR[lubridate::month(data$date)==i & data$CREEL=="YES"] <- sample(LETTERS[1:nrow(tmp)])
  }
  BR
}

makeSchedule <- function(YEAR,LAKE,ROUTE,info,show_summary=TRUE) {
  ## Check inputs
  YEAR <- iCheckYear(YEAR)
  
  ## Get start and end data from the information in info
  start_date <- iFindStartDate(info$month[1],YEAR)
  end_date <- iFindLastDate(info$month[nrow(info)],YEAR)
  
  ## Create the schedule0
  ### Find all dates between the start and end date
  ### Add a variable that contains the ROUTE name
  ### Find the week since start_date (beginning of the survey)
  ### Find weekdays, both in words and as a number
  ### Identify day types (weekend, holiday, or weekday)
  ### Determine on which days a creel survey should be conducted
  ### Randomly choose an am or pm shift for each day
  ### Add random bus route timing letters (within a month)
  sched <- data.frame(date=start_date+0:(end_date-start_date)) %>%
    dplyr::mutate(route=ROUTE,
                  WEEK=lubridate::isoweek(date)-lubridate::isoweek(date[1])+1,
                  WDAYn=lubridate::wday(date,week_start=1),
                  WDAY=lubridate::wday(date,week_start=1,label=TRUE),
                  DAYTYPE=dplyr::case_when(
                    chron::is.weekend(date) ~ "WEEKEND",
                    is.holiday(date) ~ "HOLIDAY",
                    TRUE ~ "WEEKDAY")) %>%
    tibble::add_column(CREEL=iFindDays2Creel(.)) %>%
    tibble::add_column(shift=iFindShifts(.)) %>%
    tibble::add_column(schedule=iAssignBusRouteLetters(.))
  ## Write the result out to a file
  fout <- paste0(YEAR,"_",LAKE,"_",ROUTE,"_Schedule.csv")
  write.csv(sched,paste0(fldr,fout),quote=FALSE,row.names=FALSE)
  ## Show summaries if asked to
  if (show_summary) {
    ## Show lengths of runs of "YESes" (number of consecutive days worked)
    tmp <- rle(sched$CREEL)
    tmp <- table(tmp$values,tmp$lengths)["YES",]
    cat("Frequency of Consecutive Days Worked\n")
    print(tmp)
    ## Show frequency of day types by month
    sched <- mutate(sched,MON=droplevels(lubridate::month(date,label=TRUE)))
    cat("\nFrequency of Days by Month and Day Type\n")
    print(addmargins(xtabs(~MON+DAYTYPE,data=filter(sched,CREEL=="YES"))))
    ## Show frequency of days of the week by month
    cat("\nFrequency of Days by Month and Day of the Week\n")
    print(addmargins(xtabs(~MON+WDAY,data=filter(sched,CREEL=="YES"))))
    ## Show frequency of ams and pms by month
    cat("\nFrequency of Days by Month and Day Type\n")
    print(addmargins(xtabs(~MON+shift,data=filter(sched,CREEL=="YES"))))
    cat("\n")
  }
  ## Return the filename
  fout
}






## Helpers for making the calendar
### Makes the calendar header
iMakeCalHeader <- function(pth) {
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


makeCalendar <- function(file,pth="LSuperior//Schedule//",width,height) {
  ## Make calendar header ------------------------------------------------------
  header <- iMakeCalHeader(pth)
  
  ## Read and modify schedule file ---------------------------------------------
  ### Add an "activity" variable that combines route and shift (for printing)
  sched <- read.csv(paste0(pth,file),stringsAsFactors=FALSE) %>%
    mutate(date=lubridate::ymd(date),
           activity=ifelse(CREEL=="NO","NO CREEL",
                           paste0(route,"\n",shift,"\n(",schedule,")")))
  ## Find year and starting and ending months from the schedule
  year <- unique(lubridate::year(sched$date))
  start_mon <- lubridate::month(sched$date[1])
  end_mon <- lubridate::month(sched$date[nrow(sched)])
  
  ## Gets Jan-1 of current year
  JAN1 <- lubridate::make_date(year)
  ## Gets number of days in the year
  days <- ifelse(lubridate::leap_year(JAN1),365,364)
  ## List of dates ...
  ## ... with date and activity
  year_cal <- data.frame(date=JAN1+0:days,x=0L,y=0L) %>%
    left_join(sched,by="date") %>%
    mutate(color=as.factor(ifelse(CREEL=="NO",1,0)),
           title=lubridate::month(date,label=TRUE,abbr=FALSE))
  
  pdf(paste0(pth,tools::file_path_sans_ext(file),"_SCHEDULE.pdf"),
      width=width,height=height)
  
  for (i in (start_mon:end_mon)) {
    ## Get first and last days of the "this" month
    start_date <- lubridate::make_date(year,i)
    end_date <- start_date + months(1)
    ## Get list of dates for just "this" month
    tbl_cal <- filter(year_cal,date >= start_date,date < end_date)
    ## Make calendar page for "this" month
    cal <- ggplot(tbl_cal,aes(x=x,y=y)) + 
      ### makes each individual day (faceted below)
      geom_text(aes(label=activity,color=color),
                data=filter(tbl_cal,!is.na(activity)),
                size=3,fontface="bold") +
      sugrrants::facet_calendar(~date,format="%a-%e") +
      labs(x="",y="",title=paste0(tbl_cal$title[[1]],", ",year)) +
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
  dev.off()
}




## Helpers for making the bus routes ----

iExpandMonthsList <- function(data) {
  data2 <- data[FALSE,]
  for (i in 1:nrow(data)) {
    mons <- unlist(strsplit(data$month[i],", "))
    tmp <- data[rep(i,each=length(mons)),] 
    tmp$month <- mons
    data2 <- rbind(data2,tmp)
  }
  data2
}


readRoutes <- function(f,sheet="Routes") {
  ## Read in route information from Excel
  ### Expland the list of months to individual months
  ### Add a site variable that is combo of site number & description
  ### Make months an ordered factor variable
  ### Order rows
  ### Reduce and reorder data.frame variables
  df <- readxl::read_excel(f,sheet=sheet) %>%
    iExpandMonthsList() %>%
    dplyr::mutate(site=paste0(site_descrip," (#",site_no,")"),
                  month=factor(month,levels=month.name)) %>%
    dplyr::arrange(area,creel,route,month,visit) %>%
    dplyr::select(area,creel,route,month,site,visit,travel,peffort) %>%
    as.data.frame()
  ## Return the data.frame
  df
}

readShifts <- function(f,sheet="Shifts") {
  ## Read in route information from Excel
  ### Expand the list of months to individual months
  ### Make months an ordered factor variable
  ### Order rows
  df <- readxl::read_excel(f,sheet=sheet,col_types="text") %>%
    iExpandMonthsList() %>%
    dplyr::mutate(month=factor(month,levels=month.name)) %>%
    dplyr::arrange(area,creel,route,month,shift) %>%
    as.data.frame()
  ## Return the data.frame
  df
}




busRoute <- function(data,sites,visit,travel,peffort,
                     start,end,allow_reverse=TRUE) {
  ## Possibly (if allowed) change the visit order (based on a coin-flip)
  if (allow_reverse & runif(1)<0.5) {
    ### adjust inter-site travel times
    data$travel <- c(data$travel[nrow(data)],data$travel[-nrow(data)])
    ### Reverse visit order
    data <- data[order(data$visit,decreasing=TRUE),]
    ### ... and then adjust the visit order (though this is not needed again)
    data$visit <- seq.int(nrow(data))
  }
  
  ## Get vectors of information
  sites <- data[,sites]
  travel <- data[,travel]
  peffort <- data[,peffort]

  ## Find total minutes from start to end
  start <- as.POSIXct(start,format="%H:%M")
  end <- as.POSIXct(end,format="%H:%M")
  mins <- unclass(difftime(end,start,units="mins"))
  
  ## Convert peffort into minutes at each site
  ### Find total effort exclusive of total travel time
  ttlTime <- mins-sum(travel)
  teffort <- round(ttlTime*peffort/100,0)
  ### Adjust effort slightly if the sum of rounded efforts != total time in shift
  #### Randomly select sites according to their probability to either subtract
  ####   or add a minute (depending how the sum matches the time in shift) from
  ####   the total effort.
  ttlTime2 <- sum(teffort)
  if (ttlTime!=ttlTime2) {
    ##### make a table of number of minutes to add/subtract from each site
    tmp <- table(sample(unique(sites),abs(ttlTime-ttlTime2),
                        replace=TRUE,prob=peffort/100))
    ##### get indices of which sites will received the addition/subtraction
    tmp2 <- unique(sites) %in% rownames(tmp)
    ##### do the addition/subtraction
    if (ttlTime<ttlTime2) teffort[tmp2] <- teffort[tmp2]-tmp
      else teffort[tmp2] <- teffort[tmp2]+tmp
  }
  
  ## Make a route from first site to end
  ### Create vector of "locations"
  #### Interleave sites with travel time notes
  locs <- ggplot2:::interleave(sites,paste0("TRAVEL (",travel," mins)"))
  #### And add first site onto end (like traveling back to beginning)
  locs <- c(locs,sites[1])
  ### Get vector of times for each "location"
  #### Interleave times at site with travel times
  times <- ggplot2:::interleave(teffort,travel)
  #### Put 0 at beginning (for beginning of route)
  times <- c(0,times)
  #### Then find cumulative sum of times to show day progression
  times <- cumsum(times)
  
  ## Put together as a data.frame
  df <- data.frame(Time=times,Location=locs)

  ## Adjust (wrap the route) for a random starting time
  ### Find random starting time
  rnd <- base::sample(0:times[length(times)],1)
  ### Which "location" is split (last negative diff b/w times and random #)
  ### Add a new line for this time if no time equals random time
  if (!any((times-rnd)==0)) {
    df <- rbind(df,df[Position(isTRUE,(times-rnd)<0,right=TRUE),])
    df$Time[nrow(df)] <- rnd
  }
  ### All times before rnd should have max time added to them
  ###   and then subtract rnd from each so that rnd time is set to 0
  df$Time[df$Time<rnd] <- df$Time[df$Time<rnd] + max(df$Time)
  df$Time <- df$Time - rnd
  ### Add an "End of Shift" line
  df <- rbind(df,data.frame(Time=mins,Location="END OF SHIFT"))
  ### Then reorder, remove duplicate rows (from wrapping df), fix row numbers
  df <- df[order(df$Time),]
  df <- df[!duplicated(df),]
  rownames(df) <- seq.int(nrow(df))
  
  ## Convert mins to actual time-of-day (must convert mins to secs)
  df$Time <- format(start+df$Time*60,format="%H:%M")
  
  ## Return data.frame
  df
}

