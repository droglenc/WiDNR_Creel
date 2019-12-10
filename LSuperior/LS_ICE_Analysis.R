#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
#
# PROGRAM TO ANALYZE LAKE SUPERIOR ICE CREEL
#
# DIRECTIONS:
#  1. Create a "LS_ICE_YEAR" folder (where YEAR is replaced with the year
#     to be analyzed ... e.g., LS_ICE_2019) inside "LSuperior" folder.
#  2. Use Access macro to extract interview, fdays, count, and fish data files
#      into a "data" folder inside the folder from 1.
#  3. Enter the year for the analysis here.
YEAR <- 2019
#  4. Make TRUE below to combine resultant CSV files for all routes to one file.
COMBINE_CSV_FILES <- TRUE
#  5. Source this script (and choose the information file in the dialog box).
#  6. See resulting files in folder from 1 ... the html file is the overall
#     report and the CSV files are intermediate data files that may be loaded
#     into a database for future analyses.
#
# R VERSIONS (CONVERTED FROM EXCEL): 
#     XXX, 2019 (version 1 - Derek O)
#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=


#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE THESE UNLESS THE ACCESS DATABASE MACRO HAS CHANGED!!!
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

CNTS_FILE <- "qry_ice_counts_4R.xlsx"
FDAY_FILE <- "qry_ice_fdays_4R.xlsx"
INTS_FILE <- "qry_ice_interviews_4R.xlsx"
FISH_FILE <- "qry_ice_fish_4R.xlsx"


#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE (unless you know what you are doing)!!!
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

# Create working directory for helper files and rmarkdown template.
WDIR <- file.path(here::here(),"LSuperior")
## Allows user to choose the appropriate folder created in 1 above.
message("!! Choose YEAR folder file in the dialog box (may be behind other windows) !!")
RDIR <- choose.dir(default=WDIR)

# Open the interviews file to find ROUTEs with interviews
intvs <- readxl::read_excel(file.path(RDIR,"data",INTS_FILE))
ROUTE <- unique(intvs$ROUTE)
  

## Iterate through the routes, produce an HTML report and intermediate CSV files
for (LOCATION in ROUTE) {
  # Handle slashes in location names
  LOCATION2 <- gsub("/","",LOCATION)
  message("Creating report and data files for '",LOCATION,
          "' route ...",appendLF=FALSE)
  # Create a name for the report output file ("Analysis_" + location + year).
  OUTFILE <- paste0(LOCATION2,"_Ice_",YEAR,"_Report.html")
  # Render the markdown report file with the information from above
  rmarkdown::render(input=file.path(WDIR,"Helpers","LS_Ice_Analysis_Template.Rmd"),
                    params=list(LOC=LOCATION,LOC2=LOCATION2,YR=YEAR,
                                WDIR=WDIR,RDIR=RDIR),
                    output_dir=RDIR,output_file=OUTFILE,
                    output_format="html_document",
                    clean=TRUE,quiet=TRUE)
  # Show the file in a browswer
  utils::browseURL(file.path(RDIR,OUTFILE))
  message(" Done")
}

if (COMBINE_CSV_FILES) combineCSV(RDIR,YEAR)
