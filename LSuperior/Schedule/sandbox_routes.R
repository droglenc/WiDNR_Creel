library(dplyr)
here::here()
source("LSuperior/Schedule/Schedule_Helpers.R")

LSroutes <- readRoutes("LSuperior/Schedule/Schedule_Info_LS.xlsx")
str(LSroutes)

( junk <- filter(LSroutes,route=="Corny-PW",month=="April") )
busRoute(junk,"site","visit","travel","peffort",
         start="14:00",end="21:00")

( junk <- filter(LSroutes,route=="Ashland",month=="August") )
busRoute(junk,"site","visit","travel","peffort",
         start="7:00",end="15:00")
