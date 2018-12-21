library(dplyr)
library(ggplot2)

print_calendar <- function(file,year,mon_start=1,mon_end=12,
                           pth="LSuperior//Schedule//",width,height) {
  g <- grid::rasterGrob(magick::image_read(paste0(pth,"WiDNR_logo.jpg")),
                        interpolate=TRUE)
  header <- data.frame(x=1:10,y=1) %>% 
    ggplot(aes(x,y)) +
    geom_blank() +
    annotation_custom(g,xmin=0,xmax=2.5,ymin=-Inf,ymax=Inf) +
    annotate("text",label="Lake Superior Creel Survey Schedule",x=2.25,y=1,
             size=6,hjust=0) +
    theme_void()

  ## Read in dates and ...
  ## ... add on the year
  ## ... determine if any dates are repeated
  creel <- read.csv(paste0(pth,file),stringsAsFactors=FALSE) %>%
    mutate(date=lubridate::dmy(date),
           activity=paste0(route,ifelse(!is.na(shift),
                                        paste0("\n",shift),""),
                           ifelse(!is.na(schedule),
                                  paste0("\n(",schedule,")"),"")))
  ## Gets Jan-1 of current year
  min_date <- lubridate::make_date(year)
  ## Gets number of days in the year
  days <- ifelse(lubridate::leap_year(min_date),365,364)
  ## List of dates ...
  ## ... with date and activity
  year_cal <- data.frame(date=min_date+0:days,x=0L,y=0L) %>%
    left_join(creel,by="date") %>%
    mutate(color=as.factor(ifelse(route=="NO CREEL",1,0)),
           title=lubridate::month(date,label=TRUE,abbr=FALSE))
  
  pdf(paste0(pth,tools::file_path_sans_ext(file),"_SCHEDULE.pdf"),
             width=width,height=height)
  
  for (i in (mon_start:mon_end)) {
    ## Get first and last days of the "this" month
    start_date <- lubridate::make_date(year,i)
    end_date <- start_date + months(1)
    ## Get list of dates for just "this" month
    tbl_cal <- filter(year_cal,date >= start_date,date < end_date)
    ## Make calendar page for "this" month
    cal <- ggplot(tbl_cal,aes(x=x,y=y)) + 
      ### makes each individual day (faceted below)
      geom_text(
        aes(label=activity,color=color),data=tbl_cal %>% filter(!is.na(activity)),
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
      scale_color_manual(values=c("black","red"))
    
    page <- header + patchwork::plot_spacer() + cal +
      patchwork::plot_layout(ncol=1,heights=c(0.4,0.2,3))
    print(page)
  }
  dev.off()
}

print_calendar(file="Test_Dates.csv",year=2019,mon_start=1,mon_end=2)
