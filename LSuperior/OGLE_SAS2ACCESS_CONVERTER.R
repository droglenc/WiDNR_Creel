ROUTE2USE <- "Ashland"
FNcnts <- "ashcnts.sas7bdat"
FNints <- "ashints.sas7bdat"
YEAR <- 2014
FLDR <- paste0("LS_Analysis_",YEAR)

d <- haven::read_sas(file.path(here::here(),"LSuperior",FLDR,"data",FNcnts)) %>%
  dplyr::mutate(ROUTE=ROUTE2USE,
                SURVEY=YEAR) %>%
  dplyr::select(SURVEY,ROUTE,MONTH,DAY,YEAR,DAYTYPE,STARTHH,STARTMM,STOPHH,STOPMM,SITE,COUNT)
str(d)

d2 <- haven::read_sas(file.path(here::here(),"LSuperior",FLDR,"data",FNints)) %>%
  ## Add interview ID number
  dplyr::mutate(INTERVIEW=1:dplyr::n(),
                UNIT=2,
                YEAR=YEAR+2000,
                SURVEY=YEAR,
                ROUTE=ROUTE2USE)

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

## Isolate species, clips, and lengths and make long format
## Change missing SPECCODEs to actual NAs
specInts <- dplyr::select(d2,INTERVIEW,contains("SPEC")) %>%
  tidyr::gather(tmp,SPP,-INTERVIEW) %>%
  dplyr::select(-tmp) %>%
  dplyr::mutate(SPP=ifelse(SPP=="",NA,SPP))

## Isolate clips, make long, change code to word, add clipped variable
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

d2long <- dplyr::left_join(mainInts,sclInts,by="INTERVIEW") %>%
  dplyr::arrange(INTERVIEW) %>%
  dplyr::mutate(SPP=as.numeric(SPP))

str(d2long)

d2fish <- select(d2long,SURVEY,ROUTE,INTERVIEW,SPP,LEN,CLIP) %>%
  dplyr::filter(!(is.na(SPP) & is.na(CLIP) & is.na(LEN)))

tmp <- group_by(d2long,INTERVIEW,SPP) %>%
  summarize(Num=n()) %>%
  mutate(Num=ifelse(is.na(SPP),NA,Num))
tmp2 <- select(d2long,INTERVIEW:SPP)
tmp2 <- tmp2[!duplicated(tmp2),]
d2ints <- dplyr::left_join(tmp2,tmp,by=c("INTERVIEW","SPP"))

tmp <- file.path("C:","Users","dogle","Documents")
write.csv(d,file.path(tmp,paste0(ROUTE2USE,"_",YEAR,"_COUNTS.csv")),
          quote=FALSE,na="",row.names=FALSE)
write.csv(d2ints,file.path(tmp,paste0(ROUTE2USE,"_",YEAR,"_INTERVIEWS.csv")),
          quote=FALSE,na="",row.names=FALSE)
write.csv(d2fish,file.path(tmp,paste0(ROUTE2USE,"_",YEAR,"_FISH.csv")),
          quote=FALSE,na="",row.names=FALSE)

