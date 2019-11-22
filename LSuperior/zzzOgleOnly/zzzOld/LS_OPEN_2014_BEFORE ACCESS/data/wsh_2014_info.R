## Enter names of counts and interviews data files (either SAS or CSV files)
CNTS_FILE <- "wshcnts.sas7bdat"
INTS_FILE <- "wshints.sas7bdat"
## Enter creel location must be "ash","byf","cpw","lsb","rdc","sax","sup", "wsh"
LOCATION <- "wsh"
## Enter start and end dates (must be two digits mon/day and four-digit year)
START_DATE <- "05/14/2014"
END_DATE <- "09/30/2014"
## Enter day length for each month
DAY_LENGTH <- c(Jan=00,Feb=00,Mar=00,Apr=00,May=14,Jun=14,
                Jul=14,Aug=14,Sep=13,Oct=00,Nov=00,Dec=00)
