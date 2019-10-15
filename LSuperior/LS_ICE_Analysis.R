#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
#
# PROGRAM TO ANALYZE LAKE SUPERIOR ICE CREEL
#
# DIRECTIONS:
#  1. Create
#
# R VERSIONS (CONVERTED FROM EXCEL): 
#     XXX, 2019 (version 1 - Derek O)
#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

## Allows user to choose 1 or more "information" files that correspond to a
## specific ice route. User will need to browse to where the information files
## are located (see directions above).
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
  # Handle slashes in location names
  LOCATION2 <- gsub("/","",LOCATION)
  message("Creating report and data files for '",LOCATION,"' route ...",appendLF=FALSE)
  # Create a name for the report output file ("Analysis_" + location + year).
  OUTFILE <- paste0(LOCATION2,"_Ice_",YEAR,"_Report.html")
  # Render the markdown report file with the information from above
  rmarkdown::render(input=file.path(WDIR,"Helpers","LS_Ice_Analysis_Template.Rmd"),
                    params=list(LOC=LOCATION,LOC2=LOCATION2,
                                WDIR=WDIR,RDIR=RDIR),
                    output_dir=RDIR,output_file=OUTFILE,
                    output_format="html_document",
                    clean=TRUE,quiet=TRUE)
  # Show the file in a browswer
  utils::browseURL(file.path(RDIR,OUTFILE))
  message(" Done")
}
