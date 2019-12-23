## The following can be used to test files written by LS_Open_Analysis.R.
cat("\014")

## Choose two database (i.e., CSV) files to compare
dfR <- read.csv(file.choose())  # new file
dfM <- read.csv(file.choose())  # old (base) file
### two types of comparisons ... if both are true then CSV files match
res <- dplyr::all_equal(Rdf,Mdf)
res <- unlist(lapply(Map("==",Rdf,Mdf),FUN=all,na.rm=TRUE))

## Choose two report (i.e., HTML) files to compare
tmpM <- file.choose()
tmpR <- file.choose()
### If only difference is in the date run, then HTML files match
diffr::diffr(tmpM,tmpR)
