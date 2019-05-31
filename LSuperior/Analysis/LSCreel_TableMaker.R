#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=
#
# PROGRAM TO ANALYZE "BUS ROUTE" TYPE LAKE SUPERIOR CREEL
#   SINGLE ROUTE  -   INTEGRATED EFFORT AT LANDING COUNT
#
#   VERSION 1         JULY, 2016  (Iyob T)
#   VERSION 2         XXXX, 201X  (Derek O)
#
#  DIRECTIONS:
#   * Fill in initials for filename below at LOC
#   * Fill in effective state and final dates for creel below at SDATE & FDATE
#
#  NOTES:
#   * Counts (for Lake Superior) are average number of parties present during
#     the wait time, not total effort seen during the wait time.
#   * Only official holidays are New Years, Memorial Day, July Fourth, and Labor
#     Day (Thanksgiving and Christmas are not included).
#   * FINCLIP=99 means length field has number of fish harvested.
#
#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=#=-=

## User-Specified Information ----
## Enter creel clerk location
LOC <- "ash"  # must be one of "ash","byf","cpw","lsb","rdc","sax","sup", "wsh"
## Enter start and end dates of creel
SDATE <- "05/21/2014" # must use two digits mon/day and four-digit year format
FDATE <- "09/30/2014"

## Setup ----
source("LSCreel_DataPrep.R")
makeTables(tblcap,fnpre)

# For Testing
#table1(tblcap,fnpre)
