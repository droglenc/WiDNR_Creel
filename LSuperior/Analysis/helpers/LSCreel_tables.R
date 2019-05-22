## Load Packages ----
suppressPackageStartupMessages(library(huxtable))

## Tables ----
table1 <- function(calSum,LOC,SDATE,FDATE,OUT=c("html","pdf","docx")) {
  OUT <- match.arg(OUT)
  ## Make table caption
  cap <- paste(iMakeMainCap(LOC,SDATE,FDATE),
               "Number of days by day type and assumed fishing day length (h).",
               " These values will be used as expansion factors.<br><br>")
  ## Prepare data.frame for huxtable
  ##   Remove the year and creat columns of weekdays and weekends
  ##   Add a TOTAL days column and convert months to a character type (which is
  ##     needed to add the total row later)
  ##   Rearrange and rename some columns
  ##   Add on a total row with a TOTAL label
  calTbl1 <- calSum %>%
    dplyr::select(-YEAR) %>%
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
  iWriteTable(calTbl2,LOC,SDATE,1,OUT)
}


table2 <- function(ints,LOC,SDATE,FDATE,OUT=c("html","pdf","docx")) {
  OUT <- match.arg(OUT)
  ## Make table caption
  cap <- paste(iMakeMainCap(LOC,SDATE,FDATE),
               "Number of interviews (N) and actual amount of interviewed effort",
               "(Hours) by state, day type, and type of fishery.<br><br>")
  ## Get a list of months, including "TOTAL"
  mos <- c(levels(ints$MONTH),"TOTAL")
  
  ## Prepare data.frame for huxtable
  ## Summary table of number and hours of interviews by state, day type, fishery
  ##   and month. Must convert "All" words to "TOTAL", moved the fourth row to
  ##   be column names, added months to N and SUM of column names, and then
  ##   deleted the top four rows (will be added when making huxtable)
  intsTbl1 <- as.matrix(tabular((STATE+1)*(DAYTYPE+1)*(FISHERY+1) ~ (MONTH+1)*(HOURS)*((n=1)+sum),data=ints))
  intsTbl1[intsTbl1=="All"] <- "TOTAL"
  colnames(intsTbl1) <- c(intsTbl1[4,1:3],
                          paste(rep(mos,each=2),
                                toupper(intsTbl1[4,-(1:3)]),sep="_"))
  intsTbl1 <- data.frame(intsTbl1[-(1:4),])
  
  ## Need to find which rows to shade as a sum
  dtLbls <- which(intsTbl1$FISHERY=="TOTAL")
  dtLbls <- dtLbls[-length(dtLbls)]+1
  
  ## Make the huxtable
  intsTbl2 <- as_hux(intsTbl1) %>%
    set_all_padding(1) %>%
    set_na_string(row=everywhere,col=-(1:2),"--") %>%
    set_background_color(row=where(intsTbl1$FISHERY=="TOTAL"),  # Shade total rows
                         col=everywhere,"gray95") %>%
    set_top_padding(row=dtLbls,col=everywhere,15) %>%
    set_number_format(row=everywhere,                    # No decimals on N
                      col=ends_with("N"),0) %>%
    set_number_format(row=everywhere,                    # One decimal on SUM
                      col=ends_with("SUM"),2) %>%
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
  iWriteTable(intsTbl2,LOC,SDATE,2,OUT)
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

iWriteTable <- function(h,LOC,SDATE,TABLE,OUT) {
  suppressWarnings(dir.create(paste0(year(SDATE),"_SUPERIOR")))
  fn <- paste0(year(SDATE),"_SUPERIOR/LSCreel_",year(SDATE),"_",
               iMvLoc(LOC),"_Table",TABLE,".",OUT)
  if (OUT=="html") huxtable::quick_html(h,file=fn)
  else if (OUT=="pdf") huxtable::quick_pdf(h,file=fn)
  else if (OUT=="docx") huxtable::quick_docx(h,file=fn)
}
