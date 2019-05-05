# Change these as needed (make sure to run each line).
#   YEAR ... to make the calendar for
#   CLERK ... A = Ashland-Washburn
#             B = Bayfield/Red Cliff/Little Sand Bay and Corny/Herbster/Port Wing
#             C = Saxon
#             D = Superior
#   SEED ... Random number seed ... enter number for repeatability, otherwise NULL
YEAR <- 2019
CLERK <- "B"
SEED <- NULL

# Run this to create the schedule (set the working directory)
rmarkdown::render(input="Scheduler_Template.Rmd",
                  params=list(YEAR=YEAR,CLERK=CLERK,SEED=SEED),
                  output_file=paste0(YEAR,"_Superior_",CLERK,".pdf"),
                  output_format="pdf_document",
                  clean=FALSE,quiet=TRUE)
