#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
# PROGRAM TO ANALYZE "BUS ROUTE" TYPE LAKE SUPERIOR CREEL
#   SINGLE ROUTE  -   INTEGRATED EFFORT AT LANDING COUNT
#     VERSION 1         JULY, 2016  (Iyob T)
#     VERSION 2         XXXX, 201X  (Derek O)
#
#  DIRECTIONS:
#   * Fill in information under "USER-SPECIFIED INFORMATION"
#   * Source the script.
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

## USER-SPECIFIED INFORMATION
## Enter creel location (one of "ash","byf","cpw","lsb","rdc","sax","sup", "wsh")
LOC <- "wsh"
## Enter start and end dates (must be two digits mon/day and four-digit year)
SDATE <- "05/21/2014"
FDATE <- "09/30/2014"
## Enter type of input data file to use (must be "CSV" or "SAS")
FTYPE <- "CSV"
## Enter table numbers to print (must be between 1 and 9)
TABLES <- c(1:8)
## Enter figure numbers to print (must be between 1 and 9)
FIGURES <- c(1:3)

#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
# DO NOT CHANGE ANYTHING BENEATH HERE
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
YEAR <- lubridate::year(as.Date(SDATE,"%m/%d/%Y"))
LOC2 <-  plyr::mapvalues(LOC,warn=FALSE,
                         from=c("ash","byf","cpw","lsb","rdc","sax","sup","wsh"),
                         to=c("Ashland","Bayfield","Corny-Port Wing",
                              "Little Sand Bay","Red Cliff","Saxon","Superior",
                              "Washburn"))
WDIR <- paste0(here::here(),"/LSuperior/Analysis/")
RDIR <- paste0(WDIR,YEAR,"_LSUPERIOR")

rmarkdown::render(input=paste0(WDIR,"helpers/LSCreel_Tables_Template.Rmd"),
                  params=list(LOC=LOC,SDATE=SDATE,FDATE=FDATE,
                              FTYPE=FTYPE,TABLES=TABLES,FIGURES=FIGURES,
                              WDIR=WDIR,RDIR=RDIR),
                  output_file=paste0(YEAR,"_",LOC2,"_Tables.html"),
                  output_dir=RDIR,
                  output_format="html_document",
                  clean=TRUE,quiet=TRUE)
