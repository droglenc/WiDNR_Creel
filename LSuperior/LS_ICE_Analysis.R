#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
#
# PROGRAM TO ANALYZE LAKE SUPERIOR ICE CREEL
#
# DIRECTIONS:
#  1. Create a "LS_ICE_YEAR" folder (where YEAR is replaced with the year
#     to be analyzed ... e.g., LS_ICE_2019) inside "LSuperior" folder.
#  2. Use Access macro to extract interview, fdays, count, and fish data files
#      into a "data" folder inside the folder from 1.
#  3. Complete an information file (see example from a previous year; e.g,
#      Ice_2019_info.R) and save in same folder from 1.
#  4. Source this script (and choose the information file in the dialog box).
#  5. See resulting files in folder from 1 ... the html file is the overall
#     report and the CSV files are intermediate data files that may be loaded
#     into a database for future analyses.
#
# R VERSIONS (CONVERTED FROM EXCEL): 
#     XXX, 2019 (version 1 - Derek O)
#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=



#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE (unless you know what you are doing)!!!
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Allows user to choose the appropriate "information" file from the folder
## created in 1 in the directions above.
message("!! Choose 'information' file in the dialog box",
        " (may be behind other windows) !!")
fn <- choose.files(filters=Filters["R",])
# Read user-specified information file
source(fn)
# Create results directory from where user-selected information file(s) are.
RDIR <- dirname(fn)
# Create working directory for helper files and rmarkdown template.
WDIR <- file.path(here::here(),"LSuperior")

## Iterate through the routes given in the information file. Produce an HTML
## report and intermediate CSV files (see directions above)
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
