#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
# PROGRAM TO ANALYZE "BUS ROUTE" TYPE LAKE SUPERIOR CREEL
#   SINGLE ROUTE  -   INTEGRATED EFFORT AT LANDING COUNT
#     XXXX, 201X (version 2 - Derek O);  JULY, 2016 (version 1 - Iyob T)
#
#  DIRECTIONS:
#   * Make sure interview and count data files are in a "data" folder inside a
#     folder called "LS_Analysis_YEAR" (where YEAR is replaced with the year
#     to be analyzed).
#   * Complete an information file (see previous example) and save in same folder.
#   * Source this script (and choose information files in the dialog box).
#   * See resulting files in LS_Analysis_YEAR folder.
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
# DO NOT CHANGE ANYTHING BENEATH HERE
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

fns <- choose.files(filters=Filters["R",])

for (i in seq_along(fns)) {
  # Read user=specified information
  source(fns[i])
  message("Processing '",LOCATION,"' location ...",appendLF=FALSE)
  # Make longer location name
  LOCATION2 <-  FSA::mapvalues(LOCATION,warn=FALSE,
                               from=c("ash","byf","cpw","lsb","rdc","sax","sup","wsh"),
                               to=c("Ashland","Bayfield","Corny-Port Wing",
                                    "Little Sand Bay","Red Cliff","Saxon","Superior",
                                    "Washburn"))
  YEAR <- lubridate::year(as.Date(START_DATE,"%m/%d/%Y"))
  WDIR <- paste0(here::here(),"/LSuperior/")
  RDIR <- paste0(WDIR,"LS_Analysis_",YEAR,"/")
  OUTFILE <- paste0("Analysis_",LOCATION2,"_",YEAR,".html")
  rmarkdown::render(input=paste0(WDIR,"Helpers/LS_Analysis_Template.Rmd"),
                    params=list(LOC=LOCATION,
                                SDATE=START_DATE,FDATE=END_DATE,
                                DAY_LENGTH=DAY_LENGTH,
                                CNTS_FILE=CNTS_FILE,INTS_FILE=INTS_FILE,
                                TABLES=TABLES,FIGURES=FIGURES,
                                WDIR=WDIR,RDIR=RDIR),
                    output_dir=RDIR,output_file=OUTFILE,
                    output_format="html_document",
                    clean=TRUE,quiet=TRUE)
  if (SHOW_FILE) utils::browseURL(paste0(RDIR,"/",OUTFILE))
  message(" Done")
}
