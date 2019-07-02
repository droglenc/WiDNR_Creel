#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
#
# PROGRAM TO ANALYZE "BUS ROUTE" TYPE LAKE SUPERIOR CREEL
#   SINGLE ROUTE - INTEGRATED EFFORT AT LANDING COUNT
#
# DIRECTIONS:
#  1. Put interview and count data files in a "data" folder inside a folder
#     called "LS_Analysis_YEAR" (where YEAR is replaced with the year to be
#     analyzed ... e.g., LS_ANALYSIS_2019).
#  2. Put length-weight regression Excel file in the same "data" folder from 1.
#  3. Complete an information file (see example from a previous year) and save
#     in same "data" folder from 1.
#  4. Source this script (and choose the information file(s) in the dialog box).
#  5. See resulting files in LS_Analysis_YEAR folder ... the html files are the
#     overall report and the CSV files are intermediate data files that may be
#     loaded into a database for future analyses.
#  5. OPTIONALLY, combine CSV files across routes by running the following code
#     AFTER this script has been sourced ==>  combineCSV(RDIR,YEAR)
#
# R VERSIONS (CONVERTED FROM SAS): 
#     XXXX, 201X (version 2 - Derek O)
#     JULY, 2016 (version 1 - Iyob T)
#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Allows user to choose 1 or more "information" files that correspond to a
## specific location.
fns <- choose.files(filters=Filters["R",])

## Iterates through those files/locations
for (i in seq_along(fns)) {
  # Read user-specified information
  source(fns[i])
  message("Processing '",LOCATION,"' location ...",appendLF=FALSE)
  # Make longer location name
  LOCATION2 <-  FSA::mapvalues(LOCATION,warn=FALSE,
                               from=c("ash","byf","cpw","lsb","rdc","sax","sup","wsh"),
                               to=c("Ashland","Bayfield","Corny-Port Wing",
                                    "Little Sand Bay","Red Cliff","Saxon","Superior",
                                    "Washburn"))
  # Extract the analysis year
  YEAR <- lubridate::year(as.Date(START_DATE,"%m/%d/%Y"))
  # Create the working directory (base directory + "LSuperior" folder). This is
  # where the helper files and rmarkdown template are located.
  WDIR <- file.path(here::here(),"LSuperior")
  # Create the results directory (working directory + "LS_Analysis" + YEAR)
  # This should be the same as where the information file was read from
  RDIR <- dirname(fns[i])
  # Create a name for the report output file ("Analysis_" + location + year).
  OUTFILE <- paste0("Analysis_",LOCATION2,"_",YEAR,".html")
  # Render the markdown report file with the information from above
  rmarkdown::render(input=file.path(WDIR,"Helpers","LS_Analysis_Template.Rmd"),
                    params=list(LOC=LOCATION,
                                SDATE=START_DATE,FDATE=END_DATE,
                                DAY_LENGTH=DAY_LENGTH,
                                CNTS_FILE=CNTS_FILE,INTS_FILE=INTS_FILE,
                                TABLES=TABLES,FIGURES=FIGURES,
                                WDIR=WDIR,RDIR=RDIR),
                    output_dir=RDIR,output_file=OUTFILE,
                    output_format="html_document",
                    clean=TRUE,quiet=TRUE)
  # Show the file in a browswer if SHOW_FILE=TRUE
  if (SHOW_FILE) utils::browseURL(file.path(RDIR,OUTFILE))
  message(" Done")
}
