source("LSuperior/Schedule/Schedule_Helpers.R")

year <- 2019
start_date <- "1-Apr"
end_date <- "6-Oct"
start_date <- lubridate::dmy(paste(start_date,year))
end_date <- lubridate::dmy(paste(end_date,year))

## Find all dates between the start and end date
sched <- data.frame(date=start_date+0:(end_date-start_date)) %>%
  dplyr::mutate(WEEK=lubridate::isoweek(date)-lubridate::isoweek(date[1])+1,
         WDAYn=lubridate::wday(date,week_start=1),
         WDAY=lubridate::wday(date,label=TRUE),
         DAYTYPE=dplyr::case_when(
           chron::is.weekend(date) ~ "WEEKEND",
           is.holiday(date) ~ "HOLIDAY",
           TRUE ~ "WEEKDAY")) %>%
  tibble::add_column(CREEL=findWeeklyCreelDays(.),
                     shift=sample(c("am","pm"),nrow(.),replace=TRUE)) %>%
  dplyr::mutate(shift=ifelse(CREEL=="NO",NA,shift),
                schedule="STOPS",
                route="ROUTE")

write.csv(sched,"LSuperior/Schedule/Test_Dates.csv",quote=FALSE,row.names=FALSE)
