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
LOC <- "sup"  # must be one of "ash","byf","cpw","lsb","rdc","sax","sup", "wsh"
SDATE <- "05/21/2014" # must use two digits mon/day/year format
FDATE <- "09/30/2014"



## Setup ----
setwd(paste0(here::here(),"/LSuperior/Analysis"))
source("helpers/LSCreel_helpers.R")
### Converts SDATE and FDATE to useful objects
SDATE <- as.Date(SDATE,"%m/%d/%Y")
FDATE <- as.Date(FDATE,"%m/%d/%Y")


## Expansion factors (Table 1) ----
### Make data.frame of dates from starting to ending date (entered above)
###    include the year, month, numerical day in the month, wordy day of the
###    week, and what type of day it is (Weekend or Weekday). Holidays are coded
###    as weekends.
#!!!!!! This matches Iyob's 'calendar' after his line 91 except this has YEAR,
#!!!!!!   MDAY, and WDAY variables.
calendar <- data.frame(DATE=seq(SDATE,FDATE,1)) %>%
  mutate(YEAR=year(DATE),
         MONTH=month(DATE,label=TRUE,abbr=FALSE),
         MONTH=droplevels(MONTH),
         MDAY=mday(DATE),
         WDAY=wday(DATE,label=TRUE,abbr=TRUE),
         DAYTYPE=iMvDaytype(WDAY,MONTH,MDAY)
  )

### Counts the number of weekend and weekday days in each month and includes a
###   variable that is the fishing day length.
#!!!!!! This matches Iyob's 'calendar1' after his line 97
calSum <- calendar %>%
  group_by(YEAR,MONTH,DAYTYPE) %>%
  summarize(DAYS=n()) %>%
  mutate(DAYLEN=iMvDaylen(MONTH)) %>%
  as.data.frame()

rm(calendar) # cleaning up ... these are not used further

## Make Table 1
#!!!!!! This matches Iyob's Table 1' after his line 115
source("helpers/LSCreel_Table1.R")


## Interview Data ----
### Read and prepare interviews file
###   Remove days with no effort between SDATE and FDATE (HOURS will be NA)
###   Remove unneeded variables
###   Drop unused levels
### HOURS is fishing effort by the party.
#!!!!!! This largely matches Iyob's 'ints' after his line 129 ... this includes
#!!!!!! a YEAR, MDAY, and WDAY variables; DATE is a different format; and I
#!!!!!! dropped the CLIPXX, LENXX, and SPECXX that had no data.
ints <- readInterviewData(LOC,SDATE,FDATE,dropCLS=TRUE,dropHM=TRUE) %>%
  filter(!is.na(HOURS)) %>%
  select(-FISH,-RES,-SUCCESS,-DAY) %>%
  droplevels()

### Table 2 -- Number of interviews and interviewed fishing effort by strata
#!!!!!! This matches Iyob's Table 2 after his line 149
source("helpers/LSCreel_Table2.R")


## Summarize Fishing Effort ----
### Summarized interviewed effort by MONTH, DAYTYPE, WATERS, FISHERY
###   N= Number of interviews
###   HOURS= Total interviewed effort (hours) of ALL parties
###   VHOURS= Square of HOURS (in SAS this is uncorrected sum-of-squares)
###   MTRIP= Mean interviewed effort (hours) by COMPLETED parties
###   PROP= Proportion of total interviewed effort for month-daytype that is in
###         a given waters-fishery. Should sum to 1 within each month-daytype
###         Check with: group_by(effort,MONTH,DAYTYPE) %>% summarize(sum(PROP))
###   PARTY= Party size (person's per party)
#!!!!!! This matches Iyob's 'effort' after his line 183
effort <- sumEffort(ints)
head(effort)

## Count Data ----
### Finds various varsions of dates (note that DATE had to be handled
###   differently than above b/c four rather than two digits used here).
### Convert missing COUNTs to zeroes
### Calculate the "WAIT" time (hours at the site)
### Convert average counts (the original COUNT variable) to "total effort"
###   during shift (by muliplying by the WAIT time) so that multiple shifts on
###   each day can be combined (from original SAS code).
### Remove days with no effort between SDATE and FDATE (WAIT will be NA)
#!!!!!! This largely matches Iyob's 'counts' after his line 195 except his has
#!!!!!! STARTHH, STARTMM, STOPHH, STOPMM, START, and STOP which he does not use
#!!!!!! again and mine has MDAY and WDAY (consider removing). Also, this has one
#!!!!!! fewer records than Iyob's because one of the 27-Sep had a bad STARTHH.
#!!!!!! Finally, Iyob's code did not restrict to within the survey period or
#!!!!!! convert missing counts to zeroes, but the SAS code did.
counts <- readPressureCountData(LOC,SDATE,FDATE,dropHM=TRUE) %>%
  filter(!is.na(WAIT)) %>%
  select(DATE,YEAR,MONTH,DAY,MDAY,WDAY,DAYTYPE,SITE,COUNT,WAIT)





###### WORKED DOWN TO HERE #######



# Number of species???
n_spp <- 28





#COMBINES MULTIPLE COUNTS FOR EACH DAY
counts1=aggregate(cbind(COUNT,WAIT)~DAYTYPE+MONTH+SITE+DATE, data=counts,sum)
counts1$DAYLEN=16
counts1$COUNT=counts1$COUNT*counts1$DAYLEN/counts1$WAIT
#SUM EFFORT OVER LOCATIONS FOR EACH SHIFT/DATE; GIVES ENTIRE LAKE DAILY EFFORT ESTIMATES
counts2=aggregate(cbind(COUNT)~DAYTYPE+MONTH+DATE, data=counts1,sum)

#AVERAGE DAILY ESTIMATES; SEPARATELY BY MONTH AND DAYTYPE
counts3=aggregate(cbind(COUNT)~DAYTYPE+MONTH, data=counts2,function(x) c(COUNT=mean(x),VCOUNT=var(x),NCOUNT=length(x)))
counts3=cbind(counts3[,1:2],counts3[,3])    #this is so colname are not prefixed
#EXPAND DAILY AVERAGE BY TOTAL NUMBER OF DAYS
counts4=merge(counts3,calendar1[,!(names(calendar1) %in% c("DAYLEN"))],by=c('MONTH','DAYTYPE'))
counts4=counts4[order(match(counts4$MONTH,month.name),counts4$DAYTYPE),]

counts4$COUNT=counts4$COUNT*counts4$DAYS
counts4$VCOUNT=counts4$VCOUNT/counts4$NCOUNT*counts4$DAYS*counts4$DAYS

counts=aggregate(cbind(COUNT,VCOUNT,NCOUNT,DAYS)~MONTH+DAYTYPE,data=counts4, sum)
counts$SDCOUNT=sqrt(counts[,4])

############### TABLE 3    TOTAL HARVEST AND HARVEST RATES

counts5=tabular((MONTH+1)*(DAYTYPE+1)~(COUNT+VCOUNT)*sum*Format(digits=5)*Justify(r)+(NCOUNT+DAYS)*sum*Format(digits=0)*Justify(r)+VCOUNT*sum*Format(digits=7), data=counts4)
#St Dev is calcualted below
counts_a=as.matrix(counts5)
counts6=cbind(as.matrix(counts5, format=as.numeric)[,1:4],sprintf("%.2f",sqrt(as.matrix(counts5, format=as.numeric)[,5])) )

rr=cbind("",counts_a[3:20,2:6],counts6[1:18,5])[,c(1,2,5,3,7)]

rr=ifelse(rr=="All","TOTAL",rr)
rr[,2] <- mapply(function(val)
    paste0("<span style='font-weight: 900; color: ","#120006","'>",val, "</span>"),
    val = rr[,2],USE.NAMES = FALSE)

htmlTable(rr, align="llr",col.rgroup=c("none","none","#F1F0FA"),
          header = paste(c("MONTH","DAYTYPE",c("DAYS SAMPLED","TOTAL PARTY HOURS","ST. DEV. PARTY HOURS"))),align.header="llr",
          tspanner = c("MAY","JUNE","JULY","AUGUST","SEPTEMBER","TOTAL"),
          n.tspanner =rep(3,6),
          caption=txtMergeLines(paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR  -",paste(LOC),"05/21/14-09/30/14: MONTHLY EFFORT SUMMARY (INCLUDES NON-FISHING EFFORT)")),file="Table 3.html")

effort=merge(effort,counts, by=c('MONTH','DAYTYPE'))
effort$PHOURS=effort$COUNT*effort$PROP;          #PARTY HOURS FOR THIS STRATA;
effort$VPHOURS=effort$VCOUNT*effort$PROP**2;
effort$TRIPS=effort$PHOURS/effort$MTRIP;         #TOTAL TRIPS FOR THIS STRATA;
effort$VTRIPS=effort$VPHOURS/effort$MTRIP**2;
effort$INDHRS=effort$PHOURS*effort$PARTY;        #TOTAL INDIVIDUAL HOURS FOR THIS STRATA;
effort$VINDHRS=effort$VPHOURS*effort$PARTY**2;
effort=effort[order(match(effort$MONTH,month.name),effort$DAYTYPE,effort$WATERS,effort$FISHERY),]
effort[sapply(effort, is.numeric)] <- lapply(effort[sapply(effort, is.numeric)], round, digits = 3)
rownames(effort) <- NULL
effort$WATERS=factor(effort$WATERS)

################ Table 4 & htmlTable  MONTHLY ESTIMATES OF FISHING EFFORT BY FISHERY TYPE

effort_t=tabular((WATERS)*(FISHERY)*(MONTH+1)*(DAYTYPE+1)~(N+HOURS+VHOURS)*sum*Format(digits=5)*Justify(r)+(TRIPS+VTRIPS+INDHRS+VINDHRS+PHOURS+VPHOURS)*sum*Format(digits=6)*Justify(r), data=effort)

effort_s=as.data.frame(as.matrix(effort_t, format=as.numeric))
colnames(effort_s)=c('N','HOURS','VHOURS','TRIPS','VTRIPS','INDHRS','VINDHRS','PHOURS','VPHOURS')
effort_s$PARTY=round(effort_s$INDHRS/effort_s$PHOURS,2)
effort_s$MTRIP=round(effort_s$PHOURS/effort_s$TRIPS,2)
effort_s$SDINDHRS=round(sqrt(effort_s$VINDHRS),2)
effort_s$SDPHOURS=round(sqrt(effort_s$VPHOURS),2)
effort_s$SDTRIPS=round(sqrt(effort_s$VTRIPS),2)

effort_s[sapply(effort_s, is.numeric)] <- lapply(effort_s[sapply(effort_s, is.numeric)], round, digits = 2)
effort_s[c('TRIPS','INDHRS','PHOURS')]=round(effort_s[c('TRIPS','INDHRS','PHOURS')],0)
effort_a=as.matrix(effort_t)

tt=as.matrix(cbind("",effort_a[3:nrow(effort_a),2:4],effort_s[1:nrow(effort_s),c('PHOURS','SDPHOURS','PARTY','INDHRS','SDINDHRS','MTRIP','TRIPS','SDTRIPS')]))

tt=ifelse(tt=="All","TOTAL",tt)
tt[,3]=toupper(tt[,3])
tt=ifelse(is.na(tt),"***",tt)
rownames(tt) <- NULL
tt[,2:4] <- mapply(function(val)
    paste0("<span style='font-weight: 900; color: ","#120006","'>",val, "</span>"),
    val = tt[,2:4],USE.NAMES = FALSE)

tspanner = subset(effort_a[3:nrow(effort_a),1],effort_a[3:nrow(effort_a),1]!="")
n.tspanner = diff(c(subset(c(3:nrow(effort_a)),effort_a[3:nrow(effort_a),2]!=""),nrow(effort_a)+1),1)

htmlTable(tt, align="llllr",col.rgroup = c(rep(c("none","none","#EFEFF0"),5),"none","none","#F1F0FA"),
          header = paste(c("WATERS&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;","FISHERY","MONTH","DAYTYPE","TOTAL PARTY HRS&nbsp;&nbsp;","STDEV&nbsp;&nbsp;","PERSONS/PARTY&nbsp;&nbsp;","TOTAL IND. HRS&nbsp;&nbsp;","STDEV&nbsp;&nbsp;","MEAN TRIP LENGH&nbsp;&nbsp;","NO. OF TRIPS&nbsp;&nbsp;","STDEV&nbsp;&nbsp;")),align.header="lllll",
          tspanner = c(rep(tspanner[1],length(n.tspanner)/length(tspanner)),rep(tspanner[2],length(n.tspanner)-length(n.tspanner)/length(tspanner))),
          n.tspanner = n.tspanner,
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(LOC),"05/21/14-09/30/14): MONTHLY ESTIMATES OF FISHING EFFORT BY FISHERY TYPE"),file="Table 4.html")


####### new effort summary
effort_new=subset(effort,(FISHERY!='NON-FISHING'),select=c('MONTH','DAYTYPE','WATERS','FISHERY','N','HOURS','VHOURS','INDHRS','PHOURS','VPHOURS'))
effort_new=effort_new[order(effort_new$WATERS,effort_new$FISHERY,effort_new$MONTH,effort_new$DAYTYPE),]
rownames(effort_new) <- NULL

####### FISH       USE VARIABLE OBS TO KEEP TRACK OF SEPARATE INTERVIEWS    DROP NON-FISHING INTERVIEWS
ints1=ints[, !(colnames(ints) %in% c("PERSONS","STATUS"))]
ints1$OBS=seq.int(nrow(ints1))
ints1extra=numeric(0)
SPEC=numeric(0)
for (i in 1:nrow(ints1)){
    for (j in 1:n_spp){
        if(is.na(ints1[i,j])&ints1[i,(j+2*n_spp)]!=0&!is.na(ints1[i,(j+2*n_spp)])) ints1[i,j]=40
        if(!is.na(ints1[i,j])&ints1[i,j]==99){
          for (k in 1:ints1[i,j+n_spp]){
               ints1extra=rbind(ints1extra,ints1[i,(ncol(ints1)-7):ncol(ints1)])
               time=1
               CLIP=40
               LEN=NA
               SPEC=rbind(SPEC,ints1[i,j+2*n_spp])
              }
          }
    }
}
if (length(SPEC)!=0) ints1extra=cbind(ints1extra,time,CLIP,LEN,SPEC)

ints2=subset(reshape(ints1, varying = 1:(3*n_spp), sep = "", direction = 'long'),CLIP!=99)
ints2=ints2[, !(colnames(ints2) %in% c("id"))]
ints2=rbind(ints2,ints1extra)

int2=subset(ints2,FISHERY!='NON-FISHING')
rownames(ints2) <- NULL
SPECIES=spp[1]
for(i in 1:nrow(ints2)){SPECIES[i]=spp[which(sppN2==sprintf("%03d", ints2$SPEC[i]))]}
ints2=cbind(ints2,SPECIES)
ints2=ints2[order(ints2$OBS,ints2$SPECIES),]

############

ints3=aggregate(DATE~MONTH+DAYTYPE+HOURS+STATE+FISHERY+SPECIES+OBS, data=ints2, function(x) length(x))
names(ints3)[names(ints3) == 'DATE'] <- 'HARVEST'

N=seq(1,nrow(ints3),1)
ints4=cbind(N,ints3)
ints4$HARVEST=ifelse(ints4$STATE=='WI/MN',0.5*ints4$HARVEST,ints4$HARVEST)
ints4$HOURS=ifelse(ints4$STATE=='WI/MN',0.5*ints4$HOURS,ints4$HOURS)
ints4_extra=ints4[ints4$STATE=='WI/MN',]
if(nrow(effort_extra)!=0){
  ints4_extra$WATERS='NON-WISCONSIN'
  ints4_extra$N=ints4_extra$N+0.1
  ints4$WATERS=ifelse(ints4$STATE=='MN','NON-WISCONSIN','WISCONSIN')
  ints_all=rbind(ints4,ints4_extra)
} else {
  ints4$WATERS=ifelse(ints4$STATE=='MN','NON-WISCONSIN','WISCONSIN')
  ints_all=ints4
}
ints_all=ints_all[order(ints_all$WATERS,ints_all$N),]
ints_all$COVAR=ints_all$HARVEST*ints_all$HOURS
ints_all$WATERS=factor(ints_all$WATERS)
ints_all=ints_all[, !(colnames(ints_all) %in% c("N","STATE"))]

ints_new=aggregate(cbind(HARVEST,COVAR)~SPECIES+DAYTYPE+MONTH+FISHERY+WATERS, data=ints_all, function(x) sum(x))
ints_new$VHARVEST=aggregate(cbind(HARVEST)~SPECIES+DAYTYPE+MONTH+FISHERY+WATERS, data=ints_all, function(x) c(VHARVEST=sum(x^2)))[,6]

ints_effort=merge(ints_new,effort_new,by=c('WATERS','FISHERY','MONTH','DAYTYPE'),all=TRUE)
ints_effort=ints_effort[order(ints_effort$WATERS,ints_effort$FISHERY,ints_effort$MONTH,ints_effort$DAYTYPE),]
rownames(ints_effort) <- NULL

#############
ints_effort$HOURS=ifelse(ints_effort$N>=1,ints_effort$HOURS,ifelse(ints_effort$HOURS==NA,0,ints_effort$HOURS))
ints_effort$HARVEST=ifelse(ints_effort$N>=1,ints_effort$HARVEST,ifelse(ints_effort$HARVEST==NA,0,ints_effort$HARVEST))
ints_effort$COVAR=ifelse(ints_effort$N>=1,ints_effort$COVAR,ifelse(ints_effort$COVAR==NA,0,ints_effort$COVAR))
ints_effort$VHARVEST=ifelse(ints_effort$N>=1,ints_effort$VHARVEST,ifelse(ints_effort$VHARVEST==NA,0,ints_effort$VHARVEST))
ints_effort$VHOURS=ifelse(ints_effort$N>=1,ints_effort$VHOURS,ifelse(ints_effort$VHOURS==NA,0,ints_effort$VHOURS))

ints_effort$COVAR=ifelse(ints_effort$N>=1,(ints_effort$COVAR-ints_effort$HARVEST*ints_effort$HOURS/ints_effort$N)/(ints_effort$N-1),NA)
ints_effort$VHOURS=ifelse(ints_effort$N>=1,(ints_effort$VHOURS-ints_effort$HOURS^2/ints_effort$N)/(ints_effort$N-1),NA)
ints_effort$VHARVEST=ifelse(ints_effort$N>=1,(ints_effort$VHARVEST-ints_effort$HARVEST^2/ints_effort$N)/(ints_effort$N-1),NA)

ints_effort$HRATE=ints_effort$HARVEST/ints_effort$HOURS;   #PARTY HARVEST RATE - INCLUDES NON-FISH EFFORT;
ints_effort$MHOURS=ints_effort$HOURS/ints_effort$N
ints_effort$MHARV=ints_effort$HARVEST/ints_effort$N

ints_effort$VHRATE=ifelse(ints_effort$MHARV==0&ints_effort$N>1,0,(ints_effort$VHARVEST/ints_effort$MHARV^2)+(ints_effort$VHOURS/ints_effort$MHOURS^2)-(2*ints_effort$COVAR/ints_effort$MHARV/ints_effort$MHOURS))
ints_effort$VHRATE=(ints_effort$HRATE^2)*ints_effort$VHRATE/ints_effort$N   #VARIANCE OF HARVEST RATE RATIO ESTIMATE
ints_effort$HARVEST=ints_effort$PHOURS*ints_effort$HRATE;                         #HARVEST AND VARIANCE;
ints_effort$VHARV=ints_effort$PHOURS^2*ints_effort$VHRATE + ints_effort$HRATE^2*ints_effort$VPHOURS + ints_effort$VHRATE*ints_effort$VPHOURS;
ints_effort=ints_effort[,c('MONTH','DAYTYPE','WATERS','FISHERY','SPECIES','N','VHRATE','HARVEST','VHARV','INDHRS')]
ints_effort=ints_effort[order(ints_effort$WATERS,ints_effort$SPECIES),]
ints_effort[sapply(ints_effort, is.numeric)] <- lapply(ints_effort[sapply(ints_effort, is.numeric)], round, digits = 3)
rownames(ints_effort) <- NULL

############ TABLE 5    TOTAL HARVEST AND HARVEST RATES

SUM=function(x) base::sum(x,na.rm=TRUE)
ints_t=tabular((WATERS)*(FISHERY+1)*(SPECIES)*(MONTH+1)~(DAYTYPE+1)*(HARVEST+VHARV+INDHRS)*SUM*Format(digits=5)*Justify(r), data=ints_effort)
t=subset(ints_t,ints_t[,9]!=0,na.rm=TRUE)
ttt=as.matrix(t)
tt=cbind("",ttt[5:nrow(ttt),2:ncol(ttt)])
tt[,c(5,8,11)]=round((as.matrix(t, format=as.numeric)[,c(1,4,7)]),0)
tt[,c(6,9,12)]=sprintf("%.2f",sqrt(as.matrix(t, format=as.numeric)[,c(2,5,8)]))
tt[,c(7,10,13)]=sprintf("%.5f",(as.matrix(t, format=as.numeric)[,c(1,4,7)])/(as.matrix(t, format=as.numeric)[,c(3,6,9)]))
tt=ifelse(tt=="All","TOTAL",tt)
tt[,2]=ifelse(tt[,2]=="TOTAL","COMBINED",tt[,2])
tt=ifelse(tt=="NaN","***",tt)
tt[,4]=toupper(tt[,4])

##
tt[,2:4] <- mapply(function(val)
    paste0("<span style='font-weight: 900; color: ","#120006","'>",val, "</span>"),
    val = tt[,2:4],USE.NAMES = FALSE)

tspanner = subset(ttt[5:nrow(ttt),1],ttt[5:nrow(ttt),1]!="")
n.tsp1 = diff(c(subset(c(5:nrow(ttt)),ttt[5:nrow(ttt),1]!=""),nrow(ttt)+1),1)
n.tsp2 = diff(c(subset(c(5:nrow(ttt)),ttt[5:nrow(ttt),2]!=""),nrow(ttt)+1),1)
n.tsp22=numeric(0)
if (length(tspanner)==2) n.tsp22 = diff(c(subset(c(n.tsp1[1]:nrow(ttt)),ttt[n.tsp1[1]:nrow(ttt),2]!=""),nrow(ttt)+1),1)

htmlTable(tt, align="llllr",
          header = paste(c("WATERS&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;","FISHERY","SPECIES","MONTH",rep(c("HARVEST&nbsp;&nbsp;&nbsp;&nbsp;","ST. DEV.&nbsp;&nbsp;&nbsp;&nbsp;","HARVEST/ ANGLERHR&nbsp;&nbsp;&nbsp;&nbsp;"),3))),align.header="lllll",
          tspanner = c(rep(tspanner[1],length(n.tsp2)-length(n.tsp22)),rep(tspanner[2],length(n.tsp22))),
          n.tspanner = n.tsp2,
          cgroup = rbind(c("","","DAYTYPE",rep(NA,2)),c("","",c("WEEKDAY","WEEKEND","TOTAL"))),align.cgroup=paste(rep("c",2)),
          n.cgroup = rbind(c(1,3,9,rep(NA,2)),c(1,(rep(3,4)))),
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(LOC),"05/21/14-09/30/14: TOTAL HARVEST AND HARVEST RATES BASED ON FISHERY-SPECIFIC ANGLING HOURS - DETAILED REPORT"),file="Table 5.html")

################## LENGTHSs    TABLES 6-9
lengthall=ints2[,c('FISHERY','SITE','STATE','MONTH','DATE','SPECIES','LEN','CLIP')]
lengthall$DATE=format(as.Date(lengthall$DATE),"%m/%d/%Y")
MARK=FINCLIPB[1]
for(i in 1:nrow(lengthall)){MARK[i]=FINCLIPB[which(FINCLIPA_N==lengthall$CLIP[i])]}
MARKA=FINCLIPA[1]
for(i in 1:nrow(lengthall)){MARKA[i]=FINCLIPA[which(FINCLIPA_N==lengthall$CLIP[i])]}
lengthall=cbind(lengthall,MARK,MARKA)
rownames(lengthall) <- NULL
lengths=subset(lengthall,!is.na(lengthall$LEN))

################# MARK    6
STDERR=function(x) sd(x)/sqrt(length(x))
lengths_t=tabular((SPECIES)*(MONTH+1)*(MARK+1)~(LEN)*((n=1)+(mean+STDERR+var+max+min)*Format(digits=2))*Justify(r), data=lengths)
lll=subset(lengths_t,lengths_t[,1]!=0,na.rm=TRUE)
lll[,2:6]=round((as.matrix(lll, format=as.numeric)[,2:6]),2)
lll=as.matrix(lll)
ll=cbind("",lll[3:nrow(lll),2:ncol(lll)])
ll=ifelse(ll=="All","TOTAL",ll)
ll=ifelse(ll=="    NA"|ll=="   NA","***",ll)
ll[,2]=toupper(ll[,2])
##
ll[,2:3] <- mapply(function(val)
    paste0("<span style='font-weight: 900; color: ","#120006","'>",val, "</span>"),
    val = ll[,2:3],USE.NAMES = FALSE)

htmlTable(ll, align="lllr",
          header = paste(c("SPECIES","MONTH","MARK","N","MEAN","STD. ERR.","VAR","MAX","MIN")),align.header="lllr",
          tspanner = subset(lll[3:nrow(lll),1],lll[3:nrow(lll),1]!=""),
          n.tspanner = diff(c(subset(c(3:nrow(lll)),lll[3:nrow(lll),1]!=""),nrow(lll)+1),1),
          cgroup = rbind(c("","","","LENGTH (inches)",rep(NA,5))),align.cgroup=paste(rep("c",1)),
          n.cgroup = rbind(c(1,1,1,6,rep(NA,5))),
          caption=paste("CREEL SURVEY ANALYSIS; LAKE SUPERIOR -",paste(LOC),"05/21/14-09/30/14: MEAN LENGTH OF FISH MEASURED BY SPECIES, MONTH AND MARK"),file="Table 6.html")

################## MARKA   7
lengths_t=tabular((SPECIES)*(MONTH+1)*(MARKA+1)~(LEN)*((n=1)+(mean+STDERR+var+max+min)*Format(digits=2))*Justify(r), data=lengths)
lll=subset(lengths_t,lengths_t[,1]!=0,na.rm=TRUE)
lll[,2:6]=round((as.matrix(lll, format=as.numeric)[,2:6]),2)
lll=as.matrix(lll)
ll=cbind("",lll[3:nrow(lll),2:ncol(lll)])
ll=ifelse(ll=="All","TOTAL",ll)
ll=ifelse(ll=="    NA"|ll=="   NA","***",ll)
ll[,2]=toupper(ll[,2])
##
ll[,2:3] <- mapply(function(val)
    paste0("<span style='font-weight: 900; color: ","#120006","'>",val, "</span>"),
    val = ll[,2:3],USE.NAMES = FALSE)

htmlTable(ll, align="lllr",
          header = paste(c("SPECIES","MONTH","MARK","N","MEAN","STD. ERR.","VAR","MAX","MIN")),align.header="lllr",
          tspanner = subset(lll[3:nrow(lll),1],lll[3:nrow(lll),1]!=""),
          n.tspanner = diff(c(subset(c(3:nrow(lll)),lll[3:nrow(lll),1]!=""),nrow(lll)+1),1),
          cgroup = rbind(c("","","","LENGTH (inches)",rep(NA,5))),align.cgroup=paste(rep("c",1)),
          n.cgroup = rbind(c(1,1,1,6,rep(NA,5))),
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(LOC),"05/21/14-09/30/14: MEAN LENGTH OF FISH MEASURED BY SPECIES, MONTH, AND MARK"),file="Table 7.html")

#######  8
lengths_s=tabular((SPECIES)*(MARKA+1)~(MONTH+1)*(LEN)*(n=1)*Justify(r), data=lengthall)
sss=subset(lengths_s,lengths_s[,6]!=0,na.rm=TRUE)
sss=as.matrix(sss)
ss=cbind("",sss[5:nrow(sss),2:ncol(sss)])
ss=ifelse(ss=="All","TOTAL",ss)
##
ss[,2] <- mapply(function(val)
    paste0("<span style='font-weight: 900; color: ","#120006","'>",val, "</span>"),
    val = ss[,2],USE.NAMES = FALSE)

htmlTable(ss, align="llr",
          header = paste(c("SPECIES","MARK","MAY","JUN","JUL","AUG","SEP","ALL")),align.header="llr",
          tspanner = subset(sss[5:nrow(sss),1],sss[5:nrow(sss),1]!=""),
          n.tspanner = diff(c(subset(c(5:nrow(sss)),sss[5:nrow(sss),1]!=""),nrow(sss)+1),1),
          cgroup = rbind(c("","","MONTH",rep(NA,5))),align.cgroup=paste(rep("c",1)),
          n.cgroup = rbind(c(1,1,6,rep(NA,5))),
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(LOC),"05/21/14-09/30/14: DISTRIBUTION OF FIN CLIPS AMONG FISH EXAMINED FOR MARKS"),file="Table 8.html")

####### 9
lengths_d=lengthall[order(lengthall$SPECIES,lengthall$DATE,lengthall$MARKA,lengthall$SITE,lengthall$FISHERY,lengthall$STATE),][,c('SPECIES','MARKA','DATE','SITE','FISHERY','STATE','LEN')]
ddd=as.matrix(lengths_d)
rownames(ddd) <- NULL
dd=cbind("",ddd[,2:ncol(ddd)])
##
dd[,2] <- mapply(function(val)
    paste0("<span style='font-weight: 900; color: ","#120006","'>",val, "</span>"),
    val = dd[,2],USE.NAMES = FALSE)

htmlTable(dd, align="llllllr",
          header = paste(c("SPECIES&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;","MARK","DATE","SITE","FISHERY","STATE","LENGTH (inches)")),align.header="llllllr",
          tspanner = unique(lengths_d[,1]),
          n.tspanner = data.frame(table(factor(lengths_d[,1])))[,2],
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(LOC),"05/21/14-09/30/14: DETAILED LISTING OF ALL FIN-CLIPPED FISH MEASURED"),file="Table 9.html")

           ############### //  // #############
