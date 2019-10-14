########## Adjust these as needed #############
### SAS file names
FNcnts <- "ashcnts.sas7bdat"
FNints <- "ashints.sas7bdat"
### This will be the ROUTE in the final CSV file
ROUTE2USE <- "Ashland"
### This will be the SURVEY in the final CSV file
YEAR <- 2014
UNIT <- 2  # ... note using a single UNIT may be problematic for some routes
### This is where I have the SAS data files stored ... I am using an RSTUDIO
### project (so here::here() gets that) and then a folder called LSuperior and
### then a folder called LS_Anaylsis_2014 (for the 2014 data).
FLDR <- file.path(here::here(),"LSuperior",paste0("LS_Analysis_",YEAR))
### This is where the files will be output to.
outpath <- file.path("C:","Users","dogle","Documents")




## Read SAS Counts data, add ROUTE and SURVEY fields
d <- haven::read_sas(file.path(FLDR,"data",FNcnts)) %>%
  dplyr::mutate(ROUTE=ROUTE2USE,SURVEY=YEAR) %>%
  dplyr::select(SURVEY,ROUTE,MONTH,DAY,YEAR,DAYTYPE,STARTHH,STARTMM,STOPHH,STOPMM,SITE,COUNT)
str(d)

## Read SAS interview data
d2 <- haven::read_sas(file.path(FLDR,"data",FNints)) %>%
  ## Add interview ID number, UNIT, make 4-digit YEAR, add SURVEY, add ROUTE
  dplyr::mutate(INTERVIEW=1:dplyr::n(),
                UNIT=UNIT,
                YEAR=YEAR+2000,
                SURVEY=YEAR,
                ROUTE=ROUTE2USE)

## Remove columns in interview data that are all missing (e.g., SPEC, CLIP)
allNA <- sapply(d2,function(x) {
  if (is.character(x)) all(is.na(x) | x=="")
  else all(is.na(x))
})
d2 <- d2[,!allNA]                

d2 <- select(d2,INTERVIEW,SURVEY,ROUTE,MONTH,DAY,YEAR,DAYTYPE,SITE,FISHERY,STATUS,
             PERSONS,SUCCESS,RES,STATE,UNIT,STARTHH,STARTMM,STOPHH,STOPMM,FISH,
             contains("SPEC"),contains("CLIP"),contains("LEN"))

## Get the main information about the interview
mainInts <- dplyr::select(d2,INTERVIEW:FISH)


## Isolate species and make long format
specInts <- dplyr::select(d2,INTERVIEW,contains("SPEC")) %>%
  tidyr::gather(tmp,SPP,-INTERVIEW) %>%
  dplyr::select(-tmp) %>%
  ## Change missing SPECCODEs to actual NAs
  dplyr::mutate(SPP=ifelse(SPP=="",NA,SPP))

## Isolate clips and make long format
clipInts <- dplyr::select(d2,INTERVIEW,contains("CLIP")) %>%
  tidyr::gather(tmp,CLIP,-INTERVIEW) %>%
  dplyr::select(-tmp)

## Isolate lengths, make long
lenInts <- dplyr::select(d2,INTERVIEW,contains("LEN")) %>%
  tidyr::gather(tmp,LEN,-INTERVIEW) %>%
  dplyr::select(-tmp)

## Put species, clips, and lengths back together
## Reduce to only those where a species, clip, or length was recorded (this
##   allows for one to be missing ... most commonly a length)
sclInts <- cbind(specInts,
                 clipInts[,"CLIP",drop=FALSE],
                 lenInts[,"LEN",drop=FALSE]) %>%
  dplyr::filter(!(is.na(SPP) & is.na(CLIP) & is.na(LEN)))

## Join on main interview information with species, clip, length info
d2long <- dplyr::left_join(mainInts,sclInts,by="INTERVIEW") %>%
  dplyr::arrange(INTERVIEW) %>%
  dplyr::mutate(SPP=as.numeric(SPP))


## Get just the fish information ...
d2fish <- dplyr::select(d2long,SURVEY,ROUTE,INTERVIEW,SPP,LEN,CLIP) %>%
  dplyr::filter(!(is.na(SPP) & is.na(CLIP) & is.na(LEN)))
## ... so that we can add Num field that is number of fish caught by species
##     within an interview
tmp <- dplyr::group_by(d2long,INTERVIEW,SPP) %>%
  dplyr::summarize(Num=n()) %>%
  dplyr::mutate(Num=ifelse(is.na(SPP),NA,Num))

## Get the interview information ...
tmp2 <- dplyr::select(d2long,INTERVIEW:SPP)
## ... but only one set per interview and SPP combination
tmp2 <- tmp2[!duplicated(tmp2),]
## Append counts of fish to interview information
d2ints <- dplyr::left_join(tmp2,tmp,by=c("INTERVIEW","SPP"))


## Write out the files
write.csv(d,file.path(outpath,paste0(ROUTE2USE,"_",YEAR,"_COUNTS.csv")),
          quote=FALSE,na="",row.names=FALSE)
write.csv(d2ints,file.path(outpath,paste0(ROUTE2USE,"_",YEAR,"_INTERVIEWS.csv")),
          quote=FALSE,na="",row.names=FALSE)
write.csv(d2fish,file.path(outpath,paste0(ROUTE2USE,"_",YEAR,"_FISH.csv")),
          quote=FALSE,na="",row.names=FALSE)

