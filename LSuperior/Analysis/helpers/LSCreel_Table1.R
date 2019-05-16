## Table 1 =====================================================================
##    DHO ... this recreates Iyob's Table 1

## Caption name and expored file name
cap <- paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR: ",
             LOC,format(SDATE,format="%m/%d/%y"),"-",
             format(FDATE,format="%m/%d/%y"),"<br><br>",
             "Number of days by day type and the assumed fishing day length (h). ",
             "These results will be used as expansion factors.<br><br>")
fn <- paste0("LSCreel_",LOC,"_",year(SDATE),"_Table1",".html")

## Summary table of the number of weekdays and weekends per month
##   Remove the year and interleave the weekdays and weekends
##   Add a TOTAL days column and convert months to a character type (which is
##     needed to add the total row later)
##   Rearrange and rename somee columns
##   Add on a total row with a TOTAL label
calTbl1 <- calSum %>%
  select(-YEAR) %>%
  spread(DAYTYPE,DAYS) %>%
  mutate(TOTAL=WEEKDAY+WEEKEND,
         MONTH=as.character(MONTH)) %>%
  select(MONTH,WEEKDAY,WEEKEND,TOTAL,DAYLEN) %>%
  rename(`LENGTH`=DAYLEN) %>%
  bind_rows(summarize_at(.,vars(WEEKDAY:TOTAL),'sum')) %>%
  mutate(MONTH=replace(MONTH,is.na(MONTH),"Total"))

## Make the huxtable
calTbl2 <- as_hux(calTbl1,add_colnames=TRUE) %>%
  set_align(row=everywhere,col=-1,"right") %>%        # right align all but leftmost
  rbind(c("","DAY TYPE","","","DAY"),.) %>%           # Extra label at the top
  merge_cells(row=1,2:4) %>%                          #  covers columsn 2,3,4
  set_align(row=1,col=everywhere,"center") %>%        #  will be centered
  set_bottom_border(row=1,col=2,1) %>%                #  and have a line underneath it
  set_top_border(row=1,col=everywhere,2) %>%          # Line above top of table
  set_bottom_border(row=final(),col=everywhere,2) %>% # Line below bottom of table
  set_bottom_border(row=2,col=everywhere,1) %>%       # Line below variable names
  set_bold(row=1:2,col=everywhere,TRUE) %>%           # Bold first two rows of labels
  set_bold(row=everywhere,col=1,TRUE) %>%             # Bold first column of labels
  set_background_color(row=final(),col=everywhere,"gray95") %>%  # Shade total row
  set_width(0.3) %>%                                  # Sets table & column widths
  set_col_width(col=c(.2,.2,.2,.2,.2)) %>%
  set_caption(cap) %>%                                # Puts on the caption
  set_caption_pos("topleft") %>%
  quick_html(file=fn)

## cleaning up ... these are not needed after this
rm(cap,fn,calTbl1,calTbl2)
