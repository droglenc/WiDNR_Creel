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

ROUTE2USE <- c("Ashland","Washburn/Bayfield","Saxon","Superior")[3]
YEAR <- 2019
# Working directory (base + "LSuperior"); where helper and rmarkdown files located.
WDIR <- file.path(here::here(),"LSuperior")
RDIR <- choose.dir(WDIR)

## Iterates through those files/locations
for (i in seq_along(ROUTE2USE)) {
  message("Creating report and data files for '",ROUTE2USE,"' route ...",appendLF=FALSE)
  # Create a name for the report output file ("Analysis_" + location + year).
  OUTFILE <- paste0("ICE_",ROUTE2USE,"_",YEAR,"_Report.html")
  # Render the markdown report file with the information from above
  rmarkdown::render(input=file.path(WDIR,"Helpers","LS_Ice_Analysis_Template.Rmd"),
                    params=list(LOC=ROUTE2USE,
                                WDIR=WDIR,RDIR=RDIR),
                    output_dir=RDIR,output_file=OUTFILE,
                    output_format="html_document",
                    clean=TRUE,quiet=TRUE)
  # Show the file in a browswer
  utils::browseURL(file.path(RDIR,OUTFILE))
  message(" Done")
}
