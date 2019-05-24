## Load Packages ----
suppressPackageStartupMessages(library(huxtable))

## Tables ----
table1 <- function(LOC,SDATE,FDATE) {
  ## Make table caption
  cap <- paste(iMakeMainCap(LOC,SDATE,FDATE),
               "Number of days by day type and assumed fishing day length (h).",
               " These values will be used as expansion factors.<br><br>")
  ## Prepare data.frame for huxtable
  ##   Read saved file
  ##   Remove the year and creat columns of weekdays and weekends
  ##   Add a TOTAL days column and convert months to a character type (which is
  ##     needed to add the total row later)
  ##   Rearrange and rename some columns
  ##   Add on a total row with a TOTAL label
  calTbl1 <- read.csv(paste0(lubridate::year(SDATE),"_SUPERIOR/","calSum.csv")) %>%
    dplyr::select(-YEAR) %>%
    dplyr::mutate(MONTH=ordered(MONTH,levels=format(ISOdate(2004,1:12,1),"%B"))) %>%
    tidyr::spread(DAYTYPE,DAYS) %>%
    dplyr::mutate(TOTAL=WEEKDAY+WEEKEND,
                  MONTH=as.character(MONTH)) %>%
    dplyr::select(MONTH,WEEKDAY,WEEKEND,TOTAL,DAYLEN) %>%
    dplyr::rename(`LENGTH`=DAYLEN) %>%
    dplyr::bind_rows(dplyr::summarize_at(.,vars(WEEKDAY:TOTAL),'sum')) %>%
    dplyr::mutate(MONTH=replace(MONTH,is.na(MONTH),"Total"))
  ## Make the huxtable
  calTbl2 <- as_hux(calTbl1,add_colnames=TRUE) %>%
    set_align(row=everywhere,col=-1,"right") %>%        # right align all but leftmost
    rbind(c("","DAY TYPE","","","DAY"),.) %>%           # Extra label at the top
    merge_cells(row=1,2:4) %>%                          #  covers columns 2,3,4
    set_align(row=1,col=everywhere,"center") %>%        #  will be centered
    set_bottom_border(row=1,col=2,1) %>%                #  and a line underneath it
    set_top_border(row=1,col=everywhere,2) %>%          # Line above top of table
    set_bottom_border(row=final(),col=everywhere,2) %>% # Line below bottom of table
    set_bottom_border(row=2,col=everywhere,1) %>%       # Line below variable names
    set_bold(row=1:2,col=everywhere,TRUE) %>%           # Bold first two rows of labels
    set_bold(row=everywhere,col=1,TRUE) %>%             # Bold first column of labels
    set_background_color(row=final(),col=everywhere,"gray95") %>%  # Shade total row
    set_width(0.3) %>%                                  # Sets table & column widths
    set_col_width(col=c(.2,.2,.2,.2,.2)) %>%
    set_caption(cap) %>%                                # Puts on the caption
    set_caption_pos("topleft")
  ## Print out to the file
  iWriteTable(calTbl2,LOC,SDATE,1)
}


table2 <- function(LOC,SDATE,FDATE) {
  ## Make table caption
  cap <- paste(iMakeMainCap(LOC,SDATE,FDATE),
               "Number of interviews (N) and amount of interviewed effort",
               "(Hours) by state, day type, type of fishery, and month.<br><br>")
  ## Read in file
  tmp <- read.csv(paste0(lubridate::year(SDATE),"_SUPERIOR/","intvdEffort.csv")) %>%
    dplyr::select(-YEAR) %>%
    dplyr::mutate(MONTH=ordered(MONTH,levels=format(ISOdate(2004,1:12,1),"%B")),
                  FISHERY=iMvFishery(FISHERY),
                  STATE=iMvStates(STATE)) %>%
    droplevels()
  
  ## Get a list of months, including "TOTAL"
  mos <- c(levels(tmp$MONTH),"TOTAL")
  
  ## Prepare data.frame for huxtable
  ## Summary table of number and hours of interviews by state, day type, fishery
  ##   and month. Must convert "All" words to "TOTAL", moved the fourth row to
  ##   be column names, added months to N and SUM of column names, and then
  ##   deleted the top four rows (will be added when making huxtable)
  intsTbl1 <- as.matrix(tabular((STATE+1)*(DAYTYPE+1)*(FISHERY+1) ~ 
                                  (MONTH+1)*(NINTS+HOURS)*sum,data=tmp))
  intsTbl1[intsTbl1=="All"] <- "TOTAL"
  colnames(intsTbl1) <- c(intsTbl1[4,1:3],
                          paste(rep(mos,each=2),
                                toupper(intsTbl1[4,-(1:3)]),sep="_"))
  intsTbl1 <- data.frame(intsTbl1[-(1:4),])
  
  ## Need to find which rows to shade as a sum
  dtLbls <- which(intsTbl1$FISHERY=="TOTAL")
  dtLbls2 <- dtLbls[-length(dtLbls)]+1
  
  ## Make the huxtable
  intsTbl2 <- as_hux(intsTbl1) %>%
    set_all_padding(1) %>%
    set_na_string(row=everywhere,col=-(1:2),"--") %>%
    set_background_color(row=dtLbls,  # Shade total rows
                         col=everywhere,"gray95") %>%
    set_top_padding(row=dtLbls2,col=everywhere,15) %>%
    set_number_format(row=everywhere,                    # No decimals on N
                      col=ends_with("SUM"),0) %>%
    set_number_format(row=everywhere,                    # Two decimals on SUM
                      col=ends_with("SUM.1"),2) %>%
    rbind(c("STATE","DAY TYPE","FISHERY",
            rep(c("N","Hours"),length(mos))),.) %>%      # Extra label at the top
    rbind(c("","","",c(rbind(mos,""))),.) %>%
    set_align(row=-1,col=-(1:3),value="right") %>%       # Right align values
    set_bottom_border(row=2,col=everywhere,1) %>%        # Line below column labels
    set_top_border(row=1,col=everywhere,2) %>%           # Line above top of table
    set_bottom_border(row=final(),col=everywhere,2) %>%  # Line below bottom of table
    set_bold(row=1:2,col=everywhere,TRUE) %>%            # Bold first two row labels
    set_bold(row=everywhere,col=1:3,TRUE) %>%            # Bold first 3 column labels
    set_width(0.6) %>%                                   # Sets table width
    set_caption(cap) %>% 
    set_caption_pos("topleft")
  
  ## Creates month labels over N and Sum ... generic for different #s of months
  for (i in 1:length(mos)) {
    tmp <- 2*(i+1)
    intsTbl2 <- merge_cells(intsTbl2,1,tmp:(tmp+1)) %>%
      set_bottom_border(1,tmp,1) %>%
      set_align(1,tmp,"center")
  }
  ## Print out to the file
  iWriteTable(intsTbl2,LOC,SDATE,2)
}

table3 <- function(LOC,SDATE,FDATE) {
  ## Make table caption
  cap <- paste(iMakeMainCap(LOC,SDATE,FDATE),
               "Monthly pressure count summary by month and day type",
               "(includes non-fishing effort).<br><br>")
  ## Read in file
  tmp <- read.csv(paste0(lubridate::year(SDATE),"_SUPERIOR/","pcount.csv")) %>%
    dplyr::select(-YEAR) %>%
    dplyr::mutate(MONTH=ordered(MONTH,levels=format(ISOdate(2004,1:12,1),"%B"))) %>%
    droplevels()
  
  ## Function to take the square root of a sum ... used for computing the SD
  ##   from summed variances below.
  sumsqrt <- function(x) sqrt(sum(x))

  ## Table of days sampled and total (SD) party hours by month and daytype.
  ##   Must convert "All" words to "TOTAL", removed the first two rows (labels
  ##   that will be added back with hustable), renamed columns
  tmpTbl1 <- as.matrix(tabular((MONTH+1)*(DAYTYPE+1)~(NCOUNT)*sum*Format(digits=0)+
                                 (COUNT)*sum*Format(digits=7)+
                                 (VCOUNT)*sumsqrt*Format(digits=7),data=tmp))
  tmpTbl1[tmpTbl1=="All"] <- "TOTAL"
  tmpTbl1 <- tmpTbl1[-c(1,2),]
  colnames(tmpTbl1) <- c("MONTH","DAYTYPE","NCOUNT","COUNT","SDCOUNT")
  
  ## Find which rows to shade as a sum
  dtLbls <- which(tmpTbl1[,"DAYTYPE"]=="TOTAL")
  dtLbls2 <- dtLbls[-length(dtLbls)]+1
  
  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    set_all_padding(1) %>%
    set_na_string(row=everywhere,col=-(1:2),"--") %>%
    set_background_color(row=dtLbls,  # Shade total rows
                         col=everywhere,"gray95") %>%
    set_top_padding(row=dtLbls2,col=everywhere,15) %>%
    set_number_format(row=everywhere,                    # No decimals on N
                      col="NCOUNT",0) %>%
    set_number_format(row=everywhere,                    # One decimal on SUM
                      col=c("COUNT","SDCOUNT"),2) %>%
    rbind(c("MONTH","DAY TYPE","Sampled","Total","St. Dev."),.) %>%
    rbind(c("","","Days","Party Hours",""),.) %>%
    merge_cells(row=1,col=4:5) %>%
    set_bottom_border(row=1,col=4,1) %>%
    set_align(row=1,col=everywhere,"center") %>%
    set_align(row=-1,col=-(1:2),value="right") %>%       # Right align values
    set_bottom_border(row=2,col=everywhere,1) %>%        # Line below column labels
    set_top_border(row=1,col=everywhere,2) %>%           # Line above top of table
    set_bottom_border(row=final(),col=everywhere,2) %>%  # Line below bottom of table
    set_bold(row=1:2,col=everywhere,TRUE) %>%            # Bold first two row labels
    set_bold(row=everywhere,col=1:3,TRUE) %>%            # Bold first 3 column labels
    set_width(0.25) %>%                                  # Sets table width
    set_caption(cap) %>% 
    set_caption_pos("topleft")
  
  ## Print out to the file
  iWriteTable(tmpTbl2,LOC,SDATE,3)
}

table4 <- function(LOC,SDATE,FDATE) {
  ## Make table caption
  cap <- paste(iMakeMainCap(LOC,SDATE,FDATE),
               "Monthly pressure count summary by waters, fishery type, month",
               " and day type.<br><br>")
  ## Read in file
  tmp <- read.csv(paste0(lubridate::year(SDATE),"_SUPERIOR/","effortSum.csv")) %>%
    dplyr::select(-YEAR) %>%
    dplyr::mutate(MONTH=ordered(MONTH,levels=format(ISOdate(2004,1:12,1),"%B")),
                  WATERS=factor(WATERS,levels=c("WISCONSIN","NON-WISCONSIN")),
                  FISHERY=iMvFishery(FISHERY)) %>%
    droplevels()
  
  ## Function to take the square root of a sum ... used for computing the SD
  ##   from summed variances below.
  sumsqrt <- function(x) sqrt(sum(x))
  
  ## Table of days sampled and total (SD) party hours by month and daytype.
  ##   Must convert "All" words to "TOTAL", removed the first two rows (labels
  ##   that will be added back with hustable), renamed columns
  tmpTbl1 <- tabular((WATERS)*(FISHERY)*(MONTH+1)*(DAYTYPE+1)~
                       (NINTS+HOURS+TRIPS+INDHRS+PHOURS)*sum*Format(digits=7)+
                       (VHOURS+VTRIPS+VINDHRS+VPHOURS)*sumsqrt*Format(digits=7),
                     data=tmp)
  tmpTbl1 <- as.matrix(tmpTbl1)
  tmpTbl1[tmpTbl1=="All"] <- "TOTAL"
  colnames(tmpTbl1) <- c(tmpTbl1[2,1:4],tmpTbl1[1,5:13])
  tmpTbl1 <- tmpTbl1[-(1:2),]
  tmpTbl1 <- as.data.frame(tmpTbl1,stringsAsFactors=FALSE)
  tmpTbl1[,5:13] <- lapply(tmpTbl1[,5:13],as.numeric)
  tmpTbl1 <- tmpTbl1 %>%
    mutate(PARTY=INDHRS/PHOURS,
           MTRIP=PHOURS/TRIPS) %>%
    rename(SDHOURS=VHOURS,SDTRIPS=VTRIPS,SDINDHRS=VINDHRS,SDPHOURS=VPHOURS) %>%
    select(WATERS:DAYTYPE,PHOURS,SDPHOURS,PARTY,INDHRS,SDINDHRS,MTRIP,TRIPS,SDTRIPS)
  tmpTbl1[is.na(tmpTbl1)] <- NA
  
  ## Find which rows to shade as a sum
  dtLbls <- which(tmpTbl1[,"DAYTYPE"]=="TOTAL")
  dtLbls2 <- dtLbls[-length(dtLbls)]+1
  
  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    set_all_padding(1) %>%
    set_na_string(row=everywhere,col=-(1:4),value="--") %>%
    set_background_color(row=dtLbls,  # Shade total rows
                         col=everywhere,"gray95") %>%
    set_top_padding(row=dtLbls2,col=everywhere,15) %>%
    set_number_format(row=everywhere,
                      col=c("PHOURS","INDHRS","TRIPS"),0) %>%
    set_number_format(row=everywhere,
                      col=c("SDPHOURS","PARTY","SDINDHRS","MTRIP","SDTRIPS"),2) %>%
    rbind(c("WATERS","FISHERY","MONTH","DAY TYPE","Total","SD","Party",
            "Total","SD","Length","Total","SD"),.) %>%
    rbind(c("","","","","Party Hours","","Persons/","Ind. Hours","",
            "Mean Trip","Total Trips",""),.) %>%
    merge_cells(row=1,col=5:6) %>%
    set_bottom_border(row=1,col=5,1) %>%
    merge_cells(row=1,col=8:9) %>%
    set_bottom_border(row=1,col=8,1) %>%
    merge_cells(row=1,col=11:12) %>%
    set_bottom_border(row=1,col=11,1) %>%
    set_align(row=1,col=everywhere,value="center") %>%
    set_align(row=1,col=c(7,10),value="right") %>%
    set_align(row=-1,col=-(1:2),value="right") %>%       # Right align values
    set_bottom_border(row=2,col=everywhere,1) %>%        # Line below column labels
    set_top_border(row=1,col=everywhere,2) %>%           # Line above top of table
    set_bottom_border(row=final(),col=everywhere,2) %>%  # Line below bottom of table
    set_bold(row=1:2,col=everywhere,TRUE) %>%            # Bold first two row labels
    set_bold(row=everywhere,col=1:3,TRUE) %>%            # Bold first 3 column labels
    set_width(0.6) %>%                                   # Sets table width
    set_caption(cap) %>% 
    set_caption_pos("topleft")
  
  ## Print out to the file
  iWriteTable(tmpTbl2,LOC,SDATE,4)
}

table5 <- function(LOC,SDATE,FDATE) {
  ## Make table caption
  cap <- paste(iMakeMainCap(LOC,SDATE,FDATE),
               "Total harvest and harvest rates based on fishery-specific",
               " angling hours by waters, fishery, species, month and",
               "day type.<br><br>")
  ## Read in file
  tmp <- read.csv(paste0(lubridate::year(SDATE),"_SUPERIOR/","harvestSum.csv")) %>%
    dplyr::select(-YEAR) %>%
    dplyr::mutate(MONTH=ordered(MONTH,levels=format(ISOdate(2004,1:12,1),"%B")),
                  WATERS=factor(WATERS,levels=c("WISCONSIN","NON-WISCONSIN")),
                  FISHERY=iMvFishery(FISHERY),
                  SPECIES=iMvSpecies(SPECIES)) %>%
    droplevels()
  
  ## Function to take the square root of a sum ... used for computing the SD
  ##   from summed variances below.
  sumsqrt <- function(x) sqrt(sum(x))
  
  ## Table of days sampled and total (SD) party hours by month and daytype.
  ##   Must convert "All" words to "TOTAL", removed the first two rows (labels
  ##   that will be added back with hustable), renamed columns
  tmpTbl1 <- tabular((WATERS)*(FISHERY+1)*(SPECIES)*(MONTH+1)*DropEmpty(which="row")~
                       (DAYTYPE+1)*(HARVEST+INDHRS)*sum*Format(digits=7)+
                       (DAYTYPE+1)*(VHARVEST)*sumsqrt*Format(digits=7),
                     data=tmp)
  tmpTbl1 <- as.matrix(tmpTbl1)
  tmpTbl1[tmpTbl1=="All"] <- "TOTAL"
  colnames(tmpTbl1) <- c(tmpTbl1[4,1:4],
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
  tmpTbl1[is.na(tmpTbl1)] <- NA
  
  ## Find which rows to shade as a sum
  dtLbls <- which(tmpTbl1$MONTH=="TOTAL")
  dtLbls2 <- dtLbls[-length(dtLbls)]+1
  
  ## Make the huxtable
  tmpTbl2 <- as_hux(tmpTbl1) %>%
    set_all_padding(1) %>%
    set_na_string(row=everywhere,col=-(1:4),value="--") %>%
    set_background_color(row=dtLbls,  # Shade total rows
                         col=everywhere,"gray95") %>%
    set_top_padding(row=dtLbls2,col=everywhere,15) %>%
    set_number_format(row=everywhere,
                      col=c("WEEKDAYHARVEST","WEEKENDHARVEST","TOTALHARVEST"),0) %>%
    set_number_format(row=everywhere,
                      col=c("WEEKDAYSDHARVEST","WEEKENDSDHARVEST","TOTALSDHARVEST"),2) %>%
    set_number_format(row=everywhere,
                      col=c("WEEKDAYHRATE","WEEKENDHRATE","TOTALHRATE"),4) %>%
    rbind(c("WATERS","FISHERY","SPECIES","MONTH","Number","SD","Angler-Hr",
            "Number","SD","Angler-Hr","Number","SD","Angler-Hr"),.) %>%
    rbind(c("","","","","Harvest","","Harvest/","Harvest","","Harvest/",
            "Harvest","","Harvest/"),.) %>%
    rbind(c("","","","","WEEKDAY","","","WEEKEND","","","TOTAL","",""),.) %>%
    merge_cells(row=1,col=5:7) %>%
    set_bottom_border(row=1,col=5,1) %>%
    merge_cells(row=1,col=8:10) %>%
    set_bottom_border(row=1,col=8,1) %>%
    merge_cells(row=1,col=11:13) %>%
    set_bottom_border(row=1,col=11,1) %>%
    merge_cells(row=2,col=5:6) %>%
    set_bottom_border(row=2,col=5,1) %>%
    merge_cells(row=2,col=8:9) %>%
    set_bottom_border(row=2,col=8,1) %>%
    merge_cells(row=2,col=11:12) %>%
    set_bottom_border(row=2,col=11,1) %>%
    set_align(row=1:2,col=everywhere,value="center") %>%
    set_align(row=-(1:2),col=-(1:2),value="right") %>%   # Right align values
    set_bottom_border(row=3,col=everywhere,1) %>%        # Line below column labels
    set_top_border(row=1,col=everywhere,2) %>%           # Line above top of table
    set_bottom_border(row=final(),col=everywhere,2) %>%  # Line below bottom of table
    set_bold(row=1:3,col=everywhere,TRUE) %>%            # Bold first two row labels
    set_bold(row=everywhere,col=1:4,TRUE) %>%            # Bold first 3 column labels
    set_width(0.7) %>%                                   # Sets table width
    set_caption(cap) %>% 
    set_caption_pos("topleft")
  
  ## Print out to the file
  iWriteTable(tmpTbl2,LOC,SDATE,5)
}

## Internal Helpers ----
iMvLoc <- function(x) {
  codes <- c("ash","byf","cpw","lsb","rdc","sax","sup","wsh")
  names <- c("Ashland","Bayfield","Corny-Port Wing","Little Sand Bay",
             "Red Cliff","Saxon","Superior","Washburn")
  plyr::mapvalues(x,from=codes,to=names,warn=FALSE)
}

iMakeMainCap <- function(LOC,SDATE,FDATE) {
  paste0(lubridate::year(SDATE)," LAKE SUPERIOR CREEL SURVEY<br>",
         iMvLoc(LOC),", ",format(SDATE,format="%m/%d/%y")," - ",
         format(FDATE,format="%m/%d/%y"),"<br><br>")
}

iWriteTable <- function(h,LOC,SDATE,TABLE) {
  suppressWarnings(dir.create(paste0(year(SDATE),"_SUPERIOR")))
  fn <- paste0(year(SDATE),"_SUPERIOR/LSCreel_",year(SDATE),"_",
               iMvLoc(LOC),"_Table",TABLE,".html")
  huxtable::quick_html(h,file=fn)
}
