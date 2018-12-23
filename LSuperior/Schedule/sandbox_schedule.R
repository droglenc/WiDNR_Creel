here::here()
fldr <- "LSuperior/Schedule/"
source(paste0(fldr,"Schedule_Helpers.R"))

## User Inputs
YEAR <- 2019
LAKE <- "Superior"
ROUTE <- c("Corny-PW")

## Read in Route and Shift information
f <- paste0(fldr,"Schedule_Info_LS.xlsx")
LSroutes <- readRoutes(f,sheet="Routes") %>%
  dplyr::filter(route %in% ROUTE)
LSshifts <- readShifts(f,sheet="Shifts") %>%
  dplyr::filter(route %in% ROUTE)

## Make the schedule
fn <- makeSchedule(YEAR,LAKE,ROUTE,LSroutes)

makeCalendar(file=fn)
