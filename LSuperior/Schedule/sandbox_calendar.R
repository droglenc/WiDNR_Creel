library(dplyr)
library(ggplot2)


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


print_calendar <- function(file,pth="LSuperior//Schedule//",width,height) {
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

print_calendar(file="Test_Dates.csv")
