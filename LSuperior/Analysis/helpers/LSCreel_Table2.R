## Table 2 =====================================================================
##    DHO ... this generally recreates Iyob's Table 2 in HTML

## Caption name and expored file name
cap <- paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR: ",
             LOC,format(SDATE,format="%m/%d/%y"),"-",
             format(FDATE,format="%m/%d/%y"),"<br><br>",
             "Number of interviews (N) and actual amount of interviewed effort",
             "(Hours) by state, day type, and type of fishery.<br><br>")
fn <- paste0("LSCreel_",LOC,"_",year(SDATE),"_Table2.html")

## Get a list of months, including "TOTAL"
mos <- c(levels(ints$MONTH),"TOTAL")


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
  set_bold(row=1:2,col=everywhere,TRUE) %>%            # Bold first two rows of labels
  set_bold(row=everywhere,col=1:3,TRUE) %>%            # Bold first 3 columns of labels
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

## Prints the table to a file
quick_html(intsTbl2,file=fn)

## cleaning up ... these are not needed after this
rm(cap,fn,mos,dtLbls,intsTbl1,intsTbl2)
