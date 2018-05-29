
library(chillR)

sYear <- 2002
eYear <- 2003
sMth <- 5
eMth <- 4
sDay <- 1
eDay <- 30

lat <- -28.62

sJDay <- as.numeric(format(as.Date(paste(sYear,sMth,sDay,sep='-'),'%Y-%m-%d'),'%j'))

stn <- 41175

theurl<-paste(THEURL,stn,sep="")
theurl<-paste(theurl,"&ddStart=",sDay,"&mmStart=",sMth,"&yyyyStart=",sYear,"&ddFinish=",eDay,"&mmFinish=",eMth,sep="")
theurl<-paste(theurl,"&yyyyFinish=",eYear,sep="")
conn<-url(theurl)
hdr <- readLines(conn, 20, FALSE)

tab.1 <- read.table(conn,header=F,skip=20,col.names=unlist(strsplit(hdr[19], " +")))



year <- tab.1[,1]
day <- tab.1[ ,2]
maxt <- tab.1[,4]
mint <- tab.1[,5]

chillWeather<-data.frame(year,day,maxt,mint)
colnames(chillWeather)<-c('Year','JDay','Tmax','Tmin')
THourly<-make_hourly_temps(lat,chillWeather)
stack<-stack_hourly_temps(hour_file=THourly)
#get chill and heating info
ch<-chilling_hourtable(stack,sJDay)

gdh<-ch$GDH
units<-"Growing Degree Hours"
these<-1:length(ch$JDay)

maxHeat<-round(max(gdh),1)

