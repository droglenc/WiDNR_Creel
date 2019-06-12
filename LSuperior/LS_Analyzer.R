#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
# PROGRAM TO ANALYZE "BUS ROUTE" TYPE LAKE SUPERIOR CREEL
#   SINGLE ROUTE  -   INTEGRATED EFFORT AT LANDING COUNT
#     XXXX, 201X (version 2 - Derek O);  JULY, 2016 (version 1 - Iyob T)
#
#  DIRECTIONS:
#   * Make sure interview and count data files are in a "data" folder inside a
#     folder called "LS_Analysis_YEAR" (where YEAR is replaced with the year
#     to be analyzed).
#   * Complete information under "USER-SPECIFIED INFORMATION" below.
#   * Source this script.
#   * See resulting files in LS_Analysis_YEAR folder.
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

## USER-SPECIFIED INFORMATION
## Enter creel location must be "ash","byf","cpw","lsb","rdc","sax","sup", "wsh"
##   Process multiple locations with multiple codes separated by commas
LOCATION <- c("ash","sup","wsh")
## Enter start and end dates (must be two digits mon/day and four-digit year)
##   If multiple locations, separate dates with commas (or use one common date)
START_DATE <- c("05/16/2014","05/21/2014","05/14/2014")
END_DATE <- c("09/30/2014")
## Enter type of input data file to use (must be "CSV" or "SAS")
FTYPE <- "SAS"
## Enter table numbers to print (must be between 1 and 9)
TABLES <- c(1:8)
## Enter figure numbers to print (must be between 1 and 9)
FIGURES <- c(1:9)
## Enter TRUE to show file in web browser
SHOW_FILE <- TRUE

#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
# DO NOT CHANGE ANYTHING BENEATH HERE
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
if (length(LOCATION)>1 & length(START_DATE)==1)
  START_DATE <- rep(START_DATE,length(LOCATION))
if (length(LOCATION)>1 & length(END_DATE)==1)
  END_DATE <- rep(END_DATE,length(LOCATION))
LOCATION2 <-  FSA::mapvalues(LOCATION,warn=FALSE,
                              from=c("ash","byf","cpw","lsb","rdc","sax","sup","wsh"),
                              to=c("Ashland","Bayfield","Corny-Port Wing",
                                   "Little Sand Bay","Red Cliff","Saxon","Superior",
                                   "Washburn"))

for (i in seq_along(LOCATION)) {
  message("Processing '",LOCATION[i],"' location.")
  YEAR <- lubridate::year(as.Date(START_DATE[i],"%m/%d/%Y"))
  WDIR <- paste0(here::here(),"/LSuperior/")
  RDIR <- paste0(WDIR,"LS_Analysis_",YEAR,"/")
  OUTFILE <- paste0("Analysis_",LOCATION2[i],"_",YEAR,".html")
  rmarkdown::render(input=paste0(WDIR,"Helpers/LS_Analysis_Template.Rmd"),
                    params=list(LOC=LOCATION[i],
                                SDATE=START_DATE[i],FDATE=END_DATE[i],
                                FTYPE=FTYPE,TABLES=TABLES,FIGURES=FIGURES,
                                WDIR=WDIR,RDIR=RDIR),
                    output_dir=RDIR,output_file=OUTFILE,
                    output_format="html_document",
                    clean=TRUE,quiet=TRUE)
  if (SHOW_FILE) utils::browseURL(paste0(RDIR,"/",OUTFILE))
}
