#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
#
# PROGRAM TO ANALYZE "BUS ROUTE" TYPE LAKE SUPERIOR CREEL
#   SINGLE ROUTE - INTEGRATED EFFORT AT LANDING COUNT
#
# DIRECTIONS:
#  1. Create a "LS_OPEN_YEAR" folder (where YEAR is replaced with the year
#     to be analyzed ... e.g., LS_OPEN_2019) inside "LSuperior" folder.
#  2. Use Access macro to extract interview, count, and fish data files into a
#     "data" folder inside the folder from 1. Make sure the year is set properly
#     (i.e., to the desired year) in the Access query before running the macro.
#  3. Put length-weight regression Excel file in the "data" folder from 2.
#  4. Complete an information file for each route (see example from a previous
#     year; e.g., Ashland_Open_2019_info.R) and save in same folder from 1.
#  5. Source this script (and choose the information file(s) in the dialog box).
#     The figures and Table 9 can be excluded by setting both below to FALSE.
MAKE_FIGURES <- TRUE
MAKE_TABLE9 <- FALSE
#  6. See resulting files in folder from 1 ... the html file is the overall
#     report and the CSV files are intermediate data files that may be loaded
#     into a database for future analyses.
#  7. Combine CSV files across routes by setting the following to TRUE
COMBINE_CSV_FILES <- TRUE

# R VERSIONS (CONVERTED FROM SAS): 
#     XXXX, 2019 (version 3 - Derek O)
#     SEPTEMBER, 2019 (version 2 - Derek O)
#     JULY, 2016 (version 1 - Iyob T)
#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE (unless you know what you are doing)!!!
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Allows user to choose 1 or more "information" files that correspond to a
## specific open-water route. User will need to browse to where the information
## files are located (see directions above).
message("!! Choose 'information' file(s) in the dialog box",
        " (may be behind other windows) !!")
fns <- choose.files(filters=Filters["R",])
# Create working directory (base directory + "LSuperior" folder). This is where
# the helper files and rmarkdown template are located.
WDIR <- file.path(here::here(),"LSuperior")
# Create results directory from where user-selected information file(s) are.
RDIR <- dirname(fns[1])

## Iterate through those files/routes producing the HTML report and the 
## intermediate CSV files (see directios above)
for (i in seq_along(fns)) {
  # Read user-specified information file
  source(fns[i])
  message("Creating report and data files for '",LOCATION,"' route ...",appendLF=FALSE)
  # Extract analysis year from the START_DATE in the information file.
  YEAR <- lubridate::year(as.Date(START_DATE,"%m/%d/%Y"))
  # Create a name for the report output file.
  OUTFILE <- paste0(LOCATION,"_Open_",YEAR,"_Report.html")
  # Render markdown report file with the information from above
  rmarkdown::render(input=file.path(WDIR,"Helpers","LS_Open_Analysis_Template.Rmd"),
                    params=list(LOC=LOCATION,YR=YEAR,
                                SDATE=START_DATE,FDATE=END_DATE,
                                DAY_LENGTH=DAY_LENGTH,
                                CNTS_FILE=CNTS_FILE,INTS_FILE=INTS_FILE,
                                WDIR=WDIR,RDIR=RDIR,
                                MAKE_FIGURES=MAKE_FIGURES,MAKE_TABLE9=MAKE_TABLE9),
                    output_dir=RDIR,output_file=OUTFILE,
                    output_format="html_document",
                    clean=TRUE,quiet=TRUE)
  # Show the file in a browswer
  utils::browseURL(file.path(RDIR,OUTFILE))
  message(" Done")
}

if (COMBINE_CSV_FILES) combineCSV(RDIR,YEAR)