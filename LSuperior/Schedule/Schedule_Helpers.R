library(dplyr)

readRoutes <- function(loc,sheet="Routes") {
  ## Read in route information from Excel file ---------------------------------
  df <- readxl::read_excel(loc,sheet=sheet)
  
  ## Expand the months list ----------------------------------------------------
  df2 <- df[FALSE,]
  for (i in 1:nrow(df)) {
    mnths <- unlist(strsplit(df$month[i],", "))
    tmp <- df[rep(i,each=length(mnths)),] 
    tmp$month <- mnths
    df2 <- rbind(df2,tmp)
  }
  
  ## Modify data.frame for use -------------------------------------------------
  ### Add a site variable that is combo of site number & description
  ### Make months an ordered factor variable
  ### Order rows
  ### Reduce and reorder data.frame variables
  df2 <- df2 %>%
    dplyr::mutate(site=paste0(site_descrip," (#",site_no,")"),
                  month=factor(month,levels=month.name)) %>%
    dplyr::arrange(area,creel,route,month,visit) %>%
    dplyr::select(area,creel,route,month,site,visit,travel,peffort)
  
  ## Return the data.frame -----------------------------------------------------
  as.data.frame(df2)
}




busRoute <- function(data,sites,visit,travel,peffort,
                     start,end,allow_reverse=TRUE) {
  ## Possibly (if allowed) change the visit order (based on a coin-flip) -------
  if (allow_reverse & runif(1)<0.5) {
    ### adjust inter-site travel times
    data$travel <- c(data$travel[nrow(data)],data$travel[-nrow(data)])
    ### Reverse visit order
    data <- data[order(data$visit,decreasing=TRUE),]
    ### ... and then adjust the visit order (though this is not needed again)
    data$visit <- seq.int(nrow(data))
  }
  
  ## Get vectors of information ------------------------------------------------
  sites <- data[,sites]
  travel <- data[,travel]
  peffort <- data[,peffort]

  ## Find total minutes from start to end --------------------------------------
  start <- as.POSIXct(start,format="%H:%M")
  end <- as.POSIXct(end,format="%H:%M")
  mins <- unclass(difftime(end,start,units="mins"))
  
  ## Convert peffort into minutes at each site ---------------------------------
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
  
  ## Make a route from first site to end ---------------------------------------
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
  
  ## Put together as a data.frame ----------------------------------------------
  df <- data.frame(Time=times,Location=locs)

  ## Adjust (wrap the route) for a random starting time ------------------------
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
  
  ## Convert mins to actual time-of-day (must convert mins to secs) ------------
  df$Time <- format(start+df$Time*60,format="%H:%M")
  
  ## Return data.frame ---------------------------------------------------------
  df
}

