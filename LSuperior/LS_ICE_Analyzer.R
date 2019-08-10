#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
#
# PROGRAM TO ANALYZE LAKE SUPERIOR ICE CREEL
#
# DIRECTIONS:
#  1. 
#
# R VERSIONS (CONVERTED FROM EXCEL): 
#     AUGUST, 2016 (version 1 - Derek O)
#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!
#
# DO NOT CHANGE ANYTHING BENEATH HERE
#
#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!#!-!

ROUTE2USE <- c("Ashland","Washburn","Saxon","Superior")[1]
YEAR <- 2019
# Working directory (base + "LSuperior"); where helper and rmarkdown files located.
WDIR <- file.path(here::here(),"LSuperior")
RDIR <- choose.dir(WDIR)

## Iterates through those files/locations
for (i in seq_along(ROUTE2USE)) {
  message("Processing '",ROUTE2USE,"' location ...",appendLF=FALSE)
  # Create a name for the report output file ("Analysis_" + location + year).
  OUTFILE <- paste0("ICE_Analysis_",ROUTE2USE,"_",YEAR,".html")
  # Render the markdown report file with the information from above
  rmarkdown::render(input=file.path(WDIR,"Helpers","LS_ICE_Analysis_Template.Rmd"),
                    params=list(LOC=ROUTE2USE,
                                WDIR=WDIR,RDIR=RDIR),
                    output_dir=RDIR,output_file=OUTFILE,
                    output_format="html_document",
                    clean=TRUE,quiet=TRUE)
  # Show the file in a browswer
  utils::browseURL(file.path(RDIR,OUTFILE))
  message(" Done")
}
