## The following tests that the three main CSV files written from the
## LS_Analyzer.R script are the same. This does not ensure that the tables and
## figures in the results HTML page are equal but it does mean that they are
## based on the same data (if this returns all TRUEs)

## Set some directories and get list of CSV in the matching directory
WDIR <- file.path(here::here(),"LSuperior","LS_Analysis_2014")
RDIR <- file.path(WDIR,"data")
MDIR <- file.path(WDIR,"R_RESULTS_KNOWN_TO_MATCH_SAS")
FNs <- list.files(MDIR,pattern = "\\.csv$")

## Compare the files in results and matching directory
cat("\014")
for (i in seq_along(FNs)) {
  cat("Comparing",FNs[i],"\n")
  Rdf <- read.csv(file.path(RDIR,FNs[i]),stringsAsFactors=FALSE)
  Mdf <- read.csv(file.path(MDIR,FNs[i]),stringsAsFactors=FALSE)
  res <- dplyr::all_equal(Rdf,Mdf)
  cat("  Results from dplyr::all_equal():")
  if (res) cat(" TRUE\n")
  else {
    cat("\n")
    print(res)
  }
  cat("  Results from cell-by-cell comps:")
  res <- Map("==",Rdf,Mdf)
  res <- unlist(lapply(res,FUN=all,na.rm=TRUE))
  if (all(res)) cat(" TRUE\n")
  else {
    cat("\n")
    print(res)
  }
  cat("\n")
}

## You can try this on the html pages but there are differences due to dates, etc.
## Probably not worth it.
#tmpM <- file.path(MDIR,"Analysis_Ashland_2014.html")
#tmpR <- file.path(RDIR,"Analysis_Ashland_2014.html")
#diffr::diffr(tmpM,tmpR)

