#**********************************************************************;
#*                                                                     ;
#*   PROGRAM TO ANALYZE "BUS ROUTE" TYPE LAKE SUPERIOR CREEL           ;
#*     SINGLE ROUTE  -   INTEGRATED EFFORT AT LANDING COUNT            ;
#*  NOTE:  THE COUNTS FOR LAKE SUPERIOR ARE AVERAGE NUMBER OF          ;
#*         PARTIES PRESENT DURING THE WAIT TIME - NOT TOTAL EFFORT     ;
#*         SEEN DURING THE WAIT TIME.                                  ;
#*                                                                     ;
#*         VERSION 1         JULY, 2016                                ;
#*                                                                     ;
#*  NOTE:  ONLY OFFICIAL HOLIDAYS ARE NEW YEARS, MEMORIAL DAY,         ;
#*  4TH OF JULY, AND LABOR DAY (THANKSGIVING AND X-MAS NOT INCLUDED)   ;
#*                                                                     ;
#  TO RUN:  FILL IN Initials for FILENAME below at Loc=                ;
#  input data: FINCLIP=99 MEANS LENGTH FIELD HAS # OF FISH HARVESTED   ;
#*                                                                     ;
#**********************************************************************;
     
library(lubridate)
library(timeDate)
library(zoo)
library(plyr)
library(tables)
library(htmlTable)
library(reshape)
library(Gmisc)

Loc="sup"      # fill in filename intial ("ash","byf","cpw","lsb","rdc","sax","sup", or "wsh")

######################   READ IN INTERVIEW DATA 
working_dir='c:/Iyob-DNR/Lake Superior Creel'
setwd(working_dir)

supints=read.csv(paste(Loc,"ints.csv",sep=""),header=T)      #NAME OF INTERVIEW SAS FILE
counts=read.csv(paste(Loc,"cnts.csv",sep=""),header=T)       #NAME OF COUNTS SAS FILE
        
n_spp=28

ints=supints[,1:(3*n_spp)]
ints=cbind(ints, supints[,c("PERSONS","DAYTYPE","FISHERY","SITE","STATE","STATUS","MONTH")])

###############   FACTORS
monthname=factor(c(1:12),levels=1:12,labels=month.name) 
x=factor(c(1:5),levels=1:5,labels=c("WI","MN","MI","WI/MN","WI/MI")) 
ints$STATE=x[ints$STATE, drop=TRUE]
x=factor(c(1:99),levels=c(1:99),labels=c("COLDWATER-OPEN","WARMWATER-OPEN","ICE-STREAM MOUTH","ICE-WARMWATER","ICE-BOBBING","BAD RIVER","NON-FISHING","SHORE","TRIBAL","COMBINED"),exclude=c(10:98))
ints$FISHERY=x[ints$FISHERY, drop=TRUE]
f_daytype=factor(c(1:9),levels=c(1:9),labels=c("WEEKDAY","WEEKEND","TOTAL"),exclude=c(3:8))

sppN1=c('W09','W00','W02','W14','I22','W06','R01','M12','I04','I24','I23','I28','L02','I21','I19','W04','I16','I14','I12','I20','J01','B01','W12','W11','I05','I18','X15','X22','098','099')
sppN2=c('002','003','004','005','006','007','011','013','033','034','035','036','043','045','046','047','048','049','051','052','060','062','066','067','070','071','076','078','098','099')
sppL=c('BLUEGILL','SUNFISH SPP.','CRAPPIE SPP.','BLACK CRAPPIE','BROOK TROUT','PUMPKINSEED','BURBOT','CARP','LAKE HERRING','SISCOWET','LAKE TROUT','SPLAKE','NORTHERN PIKE','BROWN TROUT','RAINBOW TROUT','ROCK BASS','CHINOOK','COHO SALMON','PINK SALMON','ATLANTIC SALMON','SMELT','STURGEON','LARGEMOUTH BASS','SMALLMOUTH BASS','LAKE WHITEFISH','ROUND WHITEFISH','YELLOW PERCH','WALLEYE','CATFISH','NA')
spp=factor(sppN1,levels=sppN1,labels=sppL) ###### OR
spp=factor(sppN2,levels=sppN2,labels=sppL)
spp[which(sppN2=="099")]     

FINCLIPA_N=c(0:24,40)
FINCLIPA_L=c('00: NONE','01: AD','02: AD+LV','03: AD+RV','04: AD+LP','05: AD+RP','06: LV','07: RV','08: LP','09: RP','10: LV+RV','11: RP+LV','12: AD+LV+RV','13: D','14: HATCHERY','15: LP+RV','16: D+RV','17: D+RP','18: AD+RM','19: LP+RM','20: LP+LV','21: D+AD','22: D+LV+RV','23: D+LP','24: D+LV','40: NOT EXAMINED')
FINCLIPA=factor(FINCLIPA_N,levels=FINCLIPA_N,labels=FINCLIPA_L)
FINCLIPA[which(FINCLIPA_N==40)] 

FINCLIPB<-factor(FINCLIPA_N,levels=FINCLIPA_N,labels=c("NO FINCLIP",rep("FINCLIP",24),"NO FINCLIP"))
FINCLIPB[which(FINCLIPA_N==0)]        
        
###############################                   
SDATE=as.Date("05/21/2014","%m/%d/%Y")
FDATE=as.Date("09/30/2014","%m/%d/%Y")
DATEA=as.numeric(SDATE)
DATEB=as.numeric(FDATE)

date1=as.numeric(SDATE)
date2=as.numeric(FDATE)
ndays=date2-date1+1

calendar=data.frame(matrix(NA,ndays,3))
                                    
dates = timeSequence(SDATE,FDATE)
calendar[,1]=dates
calendar[,2]=format(dates,"%B") 
calendar[,2] = factor(calendar[,2],levels=unique(calendar[,2]),ordered=TRUE)
names(calendar)[1]='DATE'
names(calendar)[2]='MONTH'  
DAY=as.numeric(format(dates,"%d"))
WDAY=format(dates,"%a")
                       
FUNdaytype <- function(dates,MON,DAY,WDAY){ 
ifelse(isWeekend(dates)|(MON=="January"&DAY==1)|(MON=="May"&DAY>=25&WDAY=="Mon")|(MON=="July"&DAY==4)|(MON=="September"&DAY<=7&WDAY=="Mon"),2,1)  
}
calendar[,3]=FUNdaytype(calendar$DATE,calendar$MONTH,DAY,WDAY)
calendar[,3]=f_daytype[calendar[,3], drop=TRUE]        
names(calendar)[3]='DAYTYPE'  
                     
calendar1=count(calendar,c("MONTH","DAYTYPE"))      
#calendar1=calendar1[order(match(calendar1$MONTH,month.abb),calendar1$DAYTYPE),]
calendar1[,4]=ifelse((calendar1$MONTH[2]=="May") | (calendar1$MONTH[2]=="June") | (calendar1$MONTH[2]=="July") | (calendar1$MONTH[2]=="August") | (calendar1$MONTH[2]=="September"), 16,0)      
names(calendar1)[3]='DAYS'   
names(calendar1)[4]='DAYLEN'  

################# Table 1 htmlTable
calendar2=tabular((Factor(MONTH) + 1) ~ (Factor(DAYTYPE) + 1)*(DAYS)*(sum)*Justify(r)+(DAYLEN)*(mean)*Justify(r), data=calendar1)  
cal=as.matrix(calendar2)[5:10,]
cal=ifelse(cal=="All","TOTAL",cal)  
cal[,1]=toupper(cal[,1]) 
cal[,1] <- mapply(function(val)
    paste0("<span style='font-weight: 900; color: ","#120006","'>",val, "</span>"), 
    val = cal[,1],USE.NAMES = FALSE)

htmlTable(cal, align="lr",           
          header = c("MONTH",rep("DAYS",3),"FISHING DAY LENGTH"),align.header="lc",
          col.rgroup=c("none","none","none","none","none","#F1F0FA"),
          cgroup = rbind(c("","DAYTYPE","",rep(NA,2)),c("","WEEKDAY","WEEKEND","TOTAL","")),
          n.cgroup = rbind(c(1,3,1,rep(NA,2)),rep(1,4)),
          align.cgroup=paste(rep("c",2)),
          caption=txtMergeLines(paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(Loc),"05/21/14-09/30/14"),"NUMBER OF DAYS BY DAYTYPE FOR SURVEY PERIOD & ASSUMED FISHING DAY LENGTH","THESE NUMBERS WILL BE USED AS EXPANSION FACTORS"),file="Table 1.html")    
                         
#################  Hours
ints$DATE=as.Date(paste(supints$MONTH,supints$DAY,supints$YEAR,sep="/"),"%m/%d/%y") 
ints$MONTH=monthname[ints$MONTH, drop=TRUE]
  wday=format(ints$DATE,"%a")   
  day=as.numeric(format(ints$DATE,"%d"))
  ints$DAYTYPE=FUNdaytype(ints$DATE,ints$MONTH,day,wday)
ints$DAYTYPE=f_daytype[ints$DAYTYPE, drop=TRUE]
ints$DATE=as.numeric(ints$DATE)

START=supints$STARTHH*60+supints$STARTMM
STOP=supints$STOPHH*60+supints$STOPMM
ints$HOURS=ifelse((DATEA<=ints$DATE&ints$DATE<=DATEB),ifelse(START<STOP,(STOP-START)/60,NA),NA)  #(STOP+24*60-START)/60? first NA
#ints$HOURS=ifelse((DATEA<=ints$DATE&ints$DATE<=DATEB),(STOP-START)/60,NA)   is in SAS file, wrong
ints=droplevels(subset(ints,!is.na(ints$HOURS)))  

################# Table 2 htmlTable effort by varios strata
w=tabular((Factor(STATE, "State") + 1)*(Factor(DAYTYPE, "Day-type") + 1)*(Factor(FISHERY, "Fishery") + 1) ~ (Factor(MONTH, "Month") + 1)*(HOURS)*((n=1)+sum*Format(digits=3))*Justify(r), data=ints) 

rrr=as.matrix(w)
rrr=ifelse(rrr=="All","TOTAL",rrr) 
rr=cbind(rep("",nrow(rrr)-4),rrr[5:nrow(rrr),2:ncol(rrr)])
 
rr[,2:3] <- mapply(function(val)
    paste0("<span style='font-weight: 900; color: ","#120006","'>",val, "</span>"), 
    val = rr[,2:3],USE.NAMES = FALSE)
#col.rgroup = c(rep(c("none","none","none","#EFEFF0"),2),"none","none","none","#F1F0FA"),
htmlTable(rr, align="lllr",col.rgroup = c(rep("none",length(levels(ints$FISHERY))),"#EFEFF0"),
          header = paste(c("STATE","DAYTYPE","FISHERY",rep(c("N","SUM"),6))),align.header="lllr",
          tspanner = subset(rrr[5:nrow(rrr),1],rrr[5:nrow(rrr),1]!=""),
          n.tspanner =diff(c(subset(c(5:nrow(rrr)),rrr[5:nrow(rrr),1]!=""),nrow(rrr)+1),1),   
          cgroup = rbind(c("","","MONTH",rep(NA,5)),c("","","MAY","JUN","JUL","AUG","SEP","TOTAL"),c("","",rep("HOURS",6))),
          n.cgroup = rbind(c(1,2,12,rep(NA,5)),c(1,(rep(2,7))),c(1,(rep(2,7)))),align.cgroup=paste(rep("c",3)),
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(Loc),"05/21/14-09/30/14: NUMBER OF INTERVIEWS & ACTUAL AMOUNT OF INTERVIEWED EFFORT BY VARIOUS STRATA"),file="Table 2.html")
             
############# *SUM TOTAL INTERVIEWED PARTY HOURS
nints=aggregate(ints['HOURS'],c(ints['MONTH'],ints['DAYTYPE']),sum,na.rm=T)
nints=nints[order(nints$MONTH,nints$DAYTYPE),]
names(nints)[3]='THOURS'

N=seq(1,nrow(ints),1) 
effort=cbind(N,ints[,c("DAYTYPE","FISHERY","MONTH","HOURS","STATE")])
effort$HOURS=ifelse(effort$STATE=='WI/MN',0.5*effort$HOURS,effort$HOURS)
effort$INDHRS=ints$PERSONS*effort$HOURS     
effort$CHOURS=ifelse(ints$STATUS==1,effort$HOURS,NA) 
effort_extra=effort[effort$STATE=='WI/MN',]
if(nrow(effort_extra)!=0){
  effort_extra$WATERS='NON-WISCONSIN'
  effort_extra$N=effort_extra$N+0.1
  effort$WATERS=ifelse(effort$STATE=='MN','NON-WISCONSIN','WISCONSIN')
  f_all=rbind(effort,effort_extra)
} else {
    effort$WATERS=ifelse(effort$STATE=='MN','NON-WISCONSIN','WISCONSIN')
    f_all=effort
}
f_all=f_all[order(f_all$N),]
rownames(f_all) <- NULL
f_all=f_all[, !(colnames(f_all) %in% c("N","STATE"))]

f_summary=aggregate(cbind(HOURS,INDHRS,CHOURS)~FISHERY+WATERS+DAYTYPE+MONTH, data=f_all, function(x) sum(x, na.rm=TRUE), na.action=na.pass)    

f_summary$MTRIP=aggregate(cbind(CHOURS)~FISHERY+WATERS+DAYTYPE+MONTH, data=f_all, mean)[,5]
f_summary=cbind(f_summary,aggregate(cbind(HOURS)~FISHERY+WATERS+DAYTYPE+MONTH, data=f_all, function(x) c(N=length(x),VHOURS=sum(x^2)))[,5])
                           
effort=merge(nints,f_summary, by=c('MONTH','DAYTYPE'))
effort$PROP=effort$HOURS/effort$THOURS     #PERCENT OF INTERVIEWED EFFORT IN THIS STRATA;
effort$PARTY=effort$INDHRS/effort$HOURS    #PERSONS PER PARTY IN THIS STRATA
effort=effort[,c('MONTH','DAYTYPE','WATERS','FISHERY','HOURS','MTRIP','N','VHOURS','PROP','PARTY')]
effort=effort[order(effort$MONTH,effort$DAYTYPE,effort$WATERS),]  #rownames(effort) <- NULL

#READ COUNT FILE, ASSUMES THAT ALL MISSING VALUES ARE REALLY ZEROS
counts$DATE=as.Date(paste(counts$MONTH,counts$DAY,counts$YEAR,sep="/"),"%m/%d/%Y")
  counts$MONTH=monthname[counts$MONTH, drop=TRUE] 
  WD=format(counts$DATE,"%a")
  counts$DAYTYPE=FUNdaytype(counts$DATE,counts$MONTH,counts$DAY,WD)
counts$DAYTYPE=f_daytype[counts$DAYTYPE, drop=TRUE]       
counts$DATE=as.numeric(counts$DATE)
counts$START=counts$STARTHH*60+counts$STARTMM
counts$STOP=counts$STOPHH*60+counts$STOPMM
counts$WAIT=ifelse((DATEA<=counts$DATE&counts$DATE<=DATEB),ifelse(counts$START<counts$STOP,(counts$STOP-counts$START)/60,(counts$STOP+24*60-counts$START)/60),NA)
counts$COUNT=counts$COUNT*counts$WAIT

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
          caption=txtMergeLines(paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR  -",paste(Loc),"05/21/14-09/30/14: MONTHLY EFFORT SUMMARY (INCLUDES NON-FISHING EFFORT)")),file="Table 3.html")
          
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
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(Loc),"05/21/14-09/30/14): MONTHLY ESTIMATES OF FISHING EFFORT BY FISHERY TYPE"),file="Table 4.html")
  
           
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
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(Loc),"05/21/14-09/30/14: TOTAL HARVEST AND HARVEST RATES BASED ON FISHERY-SPECIFIC ANGLING HOURS - DETAILED REPORT"),file="Table 5.html")      

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
          caption=paste("CREEL SURVEY ANALYSIS; LAKE SUPERIOR -",paste(Loc),"05/21/14-09/30/14: MEAN LENGTH OF FISH MEASURED BY SPECIES, MONTH AND MARK"),file="Table 6.html")      

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
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(Loc),"05/21/14-09/30/14: MEAN LENGTH OF FISH MEASURED BY SPECIES, MONTH, AND MARK"),file="Table 7.html")     
             
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
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(Loc),"05/21/14-09/30/14: DISTRIBUTION OF FIN CLIPS AMONG FISH EXAMINED FOR MARKS"),file="Table 8.html")      
 
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
          caption=paste("CREEL SURVEY ANALYSIS: LAKE SUPERIOR -",paste(Loc),"05/21/14-09/30/14: DETAILED LISTING OF ALL FIN-CLIPPED FISH MEASURED"),file="Table 9.html") 
                              
           ############### //  // #############