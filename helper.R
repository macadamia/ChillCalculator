siteInfo <- readRDS('Data/SiteInfo.rds')
gaz <- readRDS('Data/Gazetteer2010.rds')
THEURL <- readRDS('Data/extraInfo.rds')

checkDate <- function(aDate){
  if(as.Date(aDate) < Sys.Date() ){
    return(aDate)
  } else {
    return(paste(strsplit(aDate,'-')[[1]][1],'-01-01',sep=''))
  }
}

checkDateEnd <- function(aDate){
  aYear <- as.numeric(input$yearInput)
  thisYear <- as.numeric(format(Sys.Date(),'%Y'))
  if(aYear == thisYear){
    return(Sys.Date()-1)
  } else {
    return(paste(aYear,'-12-31',sep=''))
  }
}


cleanup <- which(siteInfo$Name == 'Warburton Airfield')
siteInfo <- siteInfo[-cleanup,]
siteInfo[,'NamePerc'] <- paste(siteInfo$Name,' ',formatC((siteInfo$PercMaxTObs+siteInfo$PercMinTObs)/2*100,format='f',digits=1),'%',sep='')

cols<-c('Year','JDay','Tmax','TMin')
hours<-paste('Hour',seq(1,24),sep='_')
cols<-c(cols,hours)

f1 <- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "black"
)

a <- list(
  title = "Date",
  titlefont = f1,
  tickfont = f1,
  showticklabels = TRUE,
  nticks= 20,
  tickangle = 45
)

margin <- list(l=60, r=40,b=80,t= 40,pad=0)

searchForLocation <- function(location){
  #listOfStns
  this <- grep(tolower(location),tolower(siteInfo$Name),fixed = T)
  if(length(this) == 1){
    return(list(searchedSite=siteInfo$Name[this],searchedLat=siteInfo$latitude[this],searchedLng=siteInfo$longitude[this]))
  }
  if(length(this) < 1){
    return(NULL)
  }
  if(length(this) > 1){
    return(list(N=length(this),these=this))
  }
}

searchForPlace<- function(location){
  #listOfStns
  this <- grep(tolower(location),tolower(gaz$PlaceStatePostCode),fixed = T)
  if(length(this) == 1){
    return(list(searchedSite=gaz$PlaceStatePostCode[this],searchedLat=gaz$Latitude[this],searchedLng=gaz$Longitude[this]))
  }
  if(length(this) < 1){
    return(NULL)
  }
  if(length(this) > 1){
    return(list(N=length(this),these=this))
  }
}


years <- seq(as.numeric(format(Sys.Date(), "%Y")),1968,-1)

makeHourly<-function(tmp){
  datetime<-strptime(tmp$DateTime,"%d/%m/%Y %H:%M")
  theDate<-strftime(datetime,"%d/%m/%Y")
  hours<-as.numeric(strftime(datetime, '%H'))

  #convert to mean hourly
  weather<-tapply(tmp$Tav,list(hours,theDate),mean)
  temperature<-as.vector(weather)
  hour<-rep(seq(1,24),dim(weather)[2])
  dateList<-colnames(weather)
  year<-as.numeric(strftime(as.Date(dateList,'%d/%m/%Y'),'%Y'))
  jday<-as.numeric(strftime(as.Date(dateList,'%d/%m/%Y'),'%j'))

  maxt<-apply(weather,2,max)
  mint<-apply(weather,2,min)
  THourly<-data.frame(cbind(year,jday,maxt,mint,t(weather)))
  colnames(THourly)<-cols
  return(THourly)
}


getMet<-function(stn,Year){
  theurl<-paste(THEURL,stn,sep="")
  theurl<-paste(theurl,"&ddStart=",1,"&mmStart=",1,"&yyyyStart=",Year,"&ddFinish=",31,"&mmFinish=",12,sep="")
  theurl<-paste(theurl,"&yyyyFinish=",Year,sep="")
  conn<-url(theurl)
  t.p<-try(hdr <- readLines(conn, 20, FALSE))
  if(!inherits(t.p, "try-error")){
    tab.1 <- read.table(conn,header=F,skip=20,col.names=unlist(strsplit(hdr[19], " +")))
    return (tab.1)
  } else {
    return(NA)
  }
}

getMetLT<-function(stn){ #long-term theData
  theurl<-paste(THEURL,stn,sep="")
  theurl<-paste(theurl,"&ddStart=",1,"&mmStart=",1,"&yyyyStart=",1981,"&ddFinish=",31,"&mmFinish=",12,sep="")
  theurl<-paste(theurl,"&yyyyFinish=",2010,sep="")
  conn<-url(theurl)
  t.p<-try(hdr <- readLines(conn, 20, FALSE))
  if(!inherits(t.p, "try-error")){
    tab.1 <- read.table(conn,header=F,skip=20,col.names=unlist(strsplit(hdr[19], " +")))
    return (tab.1)
  } else {
    return(NA)
  }
}

calcChill <- function(tab.1,lat,sJDay,eJDay,inputcType){
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


  if(inputcType == 1){
    chill<-ch$Chill_Portions
    units<-'Chill Portions'
    these<-1:length(ch$JDay)
  }
  if(inputcType == 2){
    chill<-ch$Chilling_Hours
    units<-'Chiling Hours < 7.2ºC'
    these<-1:length(ch$JDay)
  }
  if(inputcType == 3){
    these<-which(ch$JDay >= sJDay & ch$JDay <= eJDay)
    chill<-ch$Chill_Units[these]
    units<-'Utah Chill Units'
  }

  maxChill<-round(max(chill),1)

  return(list(chill=chill,units=units,maxChill=maxChill,jday=ch$JDay[these],hours=ch$Hour[these]))
}

calcHeat <- function(tab.1,lat,sJDay,eJDay){
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

  return(list(gdh=gdh,units=units,maxHeat=maxHeat,jday=ch$JDay[these],hours=ch$Hour[these]))
}


calcLT <- function(tab.1,lat,sJDay){ # long-Term theData
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

  return(ch)
}

#doThePlot(input$yearInput,input$cType,site$currentLoc,input$Y2DateChill,input$startDate,3)
doThePlot <- function(YEAR,CHILLTYPE,LOCATION,Y2DATE,STARTDATE,EDATE){

  # YEAR<-2010
  # CHILLTYPE<-3
  # Y2DATE<-1
  # LOCATION<-317
  # STARTDATE<-as.Date('2016-5-1')

  Year <- as.numeric(YEAR)
  sJDay<- as.numeric(format(STARTDATE,'%j'))
  eJDay<-as.numeric(format(EDATE,'%j'))

  if(CHILLTYPE == 1){
    YLAB='Chill Portions'
  }
  if(CHILLTYPE == 2){
    YLAB='Chill Hours'
  }
  if(CHILLTYPE == 3){
    YLAB='Chill Units'
  }

  stn<-siteInfo$stnID[LOCATION]
  lat<-siteInfo$latitude[LOCATION]
  stnName<-siteInfo$Name[LOCATION]

  rdata <- file.path('Data',paste(stn,'.RData',sep=''))
  load(rdata)

  tab.1<-getMet(stn,Year)
  if(!any(is.na(tab.1))){

    res <- calcChill(tab.1,lat,sJDay,eJDay,CHILLTYPE)
    chill <- res$chill
    maxChill <- res$maxChill
    jday <- res$jday
    hours <- res$hours
    hour24 <- which(hours==24)

    jday <- jday[hour24]
    chill <- chill[hour24]

  }

  if(CHILLTYPE != 3){
    if(sum(chill) <= 0){
      aTitle <- paste("No Chill Accumulation Yet at",stnName)
    }
  } else {
    if(length(chill) < 1){
      aTitle <- paste("No Chill Accumulation Yet at",stnName)
    }
  }

  if(CHILLTYPE == 1){
    LTChill <- CP
    LTHot <- CPHot
    LTCold <- CPCold
  }
  if(CHILLTYPE == 2){
    LTChill <- CH
    LTHot <- CHHot
    LTCold <- CHCold
  }
  if(CHILLTYPE == 3){
    LTChill <- CU
    LTHot <- CUHot
    LTCold <- CUCold
  }

  lastJDay <- 365
  if(Y2DATE == 1){
    lastJDay <- min(max(jday),lastJDay,eJDay)
  }
  JDays <- sJDay:lastJDay

  today <- max(chill[lastJDay])
  todayDate <- format(EDATE,'%d %b %Y')
  if(today > 0){
    chillMessage <- paste("Chill Accumulated (",as.character(todayDate),"):" ,round(today,0),YLAB)
  } else {
    chillMessage <- 'No Chill has accumulated'
  }

  if(CHILLTYPE == 1){
    day34 <- jday[head(which(chill >= 34),1)]
    day34Date <- format(as.Date(paste(Year,day34,sep='-'),'%Y-%j'),'%d %b')
    #equiv chill hours
    eqCH <- CH[day34]
  }

  labs<-as.Date(paste(Year,JDays,sep='-'),'%Y-%j')
  if(CHILLTYPE != 3){
    chill <- chill[sJDay:lastJDay]
    LTHot <- LTHot[sJDay:lastJDay]
    LTCold <- LTCold[sJDay:lastJDay]
  } else {
    nJdays <- length(seq(sJDay,lastJDay))
    chill <- chill[1:nJdays]
    LTHot <- LTHot[sJDay:lastJDay] - LTHot[sJDay]
    LTCold <- LTCold[sJDay:lastJDay] - LTCold[sJDay]
  }

  theData <- data.frame(JDays,labs, chill,LTHot,LTCold)
  b <- list(
    title = YLAB,
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 0,
    tickfont = f1
  )

  if(CHILLTYPE != 3){
    p <- plot_ly(theData, x = ~labs, y = ~LTHot,  type = "scatter", mode='lines',name='Warmest 10%',
                 line=list(color='transparent'),showlegend = F)
    p <- add_trace(p,y = ~LTCold,name='Coolest 10%',showlegend = F,fill='tonexty',fillcolor='rgba(53,118,190,0.2)')
    if(sum(chill) > 0){
      p <- add_trace(p,y = ~chill,name='This Year',showlegend = F,line=list(color='rgb(53,118,190)'))
    }
      p <- layout(p,xaxis=a,yaxis=b,margin=margin,title=chillMessage)
  } else {
    p <- plot_ly(theData, x = ~labs, y = ~LTHot,  type = "scatter", mode='lines',name='Warmest 10%',
               line=list(color='transparent'),showlegend = F)
    p <- add_trace(p,y = ~LTCold,name='Coolest 10%',showlegend = F,fill='tonexty',fillcolor='rgba(53,118,190,0.2)')
    p <- add_trace(p,y = ~chill,name='This Year',showlegend = F,line=list(color='rgb(53,118,190)'))
    p <- layout(p,xaxis=a,yaxis=b,margin=margin,title=chillMessage)
  }
}


doTheHeatPlot <- function(YEAR,GTYPE,SDATE,EDATE,LOCATION,Y2DATE){

  Year <- as.numeric(YEAR)

  sJDay<-as.numeric(format(SDATE,'%j'))
  eJDay<-as.numeric(format(EDATE,'%j'))


  stn<-siteInfo$stnID[LOCATION]
  lat<-siteInfo$latitude[LOCATION]
  stnName<-siteInfo$Name[LOCATION]

  rdata <- file.path('Data',paste(stn,'.RData',sep=''))
  load(rdata)

  #currently this is all calendar year stuff

  tab.1<-getMet(stn,Year)
  if(!any(is.na(tab.1))){
    if(GTYPE == 1) {
      res <- calcHeat(tab.1,lat,sJDay,eJDay)
      gdh <- res$gdh
      YLAB <- res$units
      maxGD <- res$maxGDH
      jday <- res$jday
      hours <- res$hours
      hour24 <- which(hours==24)

      jday <- jday[hour24]
      gdh <- gdh[hour24]
      gdh[jday < sJDay] <- NA
      gd <- gdh

    } else {
      gTmp <- ((tab.1[,'maxt'] + tab.1[,'mint']) / 2) - GDDb
      gTmp[gTmp < 0] <- 0
      gdd <- cumsum(gTmp)
      jday <- tab.1[,'day']
      gdd[ jday < sJDay] <- NA
      gd <- gdd - gdd[sJDay]
      maxGD <- max(gd,na.rm=T)
      YLAB <- paste('Growing Degree Days (base =',GDDb,'ºC)')
    }
  }

  if(GTYPE == 1) {
    LTGD <- GDH
    LTHot <- GDHHot
    LTCold <- GDHCold
  } else {
    LTGD <- GDD
    LTHot <- GDDHot
    LTCold <- GDDCold
  }


  if(sJDay !=1 ){
    LTGD[jday < sJDay] <- NA
    LTHot[jday < sJDay] <- NA
    LTCold[jday < sJDay] <- NA

    LTGD[jday >= sJDay] <- LTGD[jday >= sJDay] - LTGD[sJDay]
    LTHot[jday >= sJDay] <- LTHot[jday >= sJDay] - LTHot[sJDay]
    LTCold[jday >= sJDay] <- LTCold[jday >= sJDay] - LTCold[sJDay]
  }

  lastJDay <- 366
  if(Y2DATE == 1){
    lastJDay <- min(max(jday),lastJDay,eJDay)
  }


  today <- max(gd,na.rm=T)

  maxGD <- max(c(LTHot,gd),na.rm=T)
  YLIM <- c(0,max(LTGD,na.rm=T))
  if(Y2DATE == 1){
    dataSet <- cbind(LTGD,LTHot,LTCold)
    YLIM <- range(c(LTGD[sJDay:lastJDay],LTHot[sJDay:lastJDay],LTCold[sJDay:lastJDay],gd),na.rm=T)
  }

  JDays <- sJDay:lastJDay
  labs<-as.Date(paste(Year,JDays,sep='-'),'%Y-%j')
  theData <- data.frame(JDays=JDays,date=labs,gd=gd[sJDay:lastJDay],LTGD=LTGD[sJDay:lastJDay],
                     LTHot=LTHot[sJDay:lastJDay],LTCold=LTCold[sJDay:lastJDay])
  b <- list(
    title = YLAB,
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 0,
    tickfont = f1
  )
  p <- plot_ly(theData, x = ~date, y = ~LTHot,  type = "scatter", mode='lines',name='Warmest 10%',
               line=list(color='transparent'),showlegend = F) %>%

    add_trace(y = ~LTCold,name='Coolest 10%',showlegend = F,fill='tonexty',fillcolor='rgba(246,100,100,0.2)') %>%

    add_trace(y = ~gd,name='This Year',showlegend = F,line=list(color='rgb(246,80,80)')) %>%


    layout(xaxis=a,yaxis=b,margin=margin)

}

doTheTempPlot <- function(YEAR,SDATE,EDATE,LOCATION,Y2DATE){

  Year <- as.numeric(YEAR)

  sJDay<-as.numeric(format(SDATE,'%j'))
  eJDay<-as.numeric(format(EDATE,'%j'))


  stn<-siteInfo$stnID[LOCATION]
  lat<-siteInfo$latitude[LOCATION]
  stnName<-siteInfo$Name[LOCATION]

  rdata <- file.path('Data',paste(stn,'.RData',sep=''))
  load(rdata)

  #curretly this is all calendar year stuff

  tab.1<-getMet(stn,Year)
  maxt <- tab.1$maxt
  mint <- tab.1$mint
  jday <- tab.1$day

  if(Y2DATE == 1){
    eJDay <- min(max(jday),eJDay)
  }

  YLIM <- c(min(c(maxt,mint,minTCold,maxTHot),na.rm=T),max(c(maxt,mint,minTCold,maxTHot),na.rm=T))
  if(Y2DATE == 1){
    dataset <- cbind(maxTHot,minTCold)
    YLIM <- range(c(maxt,mint,as.vector(dataset[sJDay:eJDay,])),na.rm=T)
  }


  JDays <- sJDay:eJDay
  labs<-as.Date(paste(Year,JDays,sep='-'),'%Y-%j')
  theData <- data.frame(date=labs,maxt=maxt[JDays],mint=mint[JDays],
                     maxTlo=maxTCold[JDays],maxThi=maxTHot[JDays],
                     minTlo=minTCold[JDays],minThi=minTHot[JDays],
                     jdays=JDays)

  b <- list(
    title = "Temperature (°C)",
    titlefont = f1,
    showticklabels = TRUE,
    tickangle = 0,
    tickfont = f1
  )
  p <- plot_ly(theData, x = ~date, y = ~maxThi,  type = "scatter", mode='lines',name='Top 10% Max T',
    line=list(color='transparent'),showlegend = F) %>%

    add_trace(y = ~maxTlo,name='Low 10% Max T',showlegend = F,fill='tonexty',fillcolor='rgba(246,100,100,0.2)') %>%

    add_trace(y = ~maxt,name='Max T',showlegend = F,line=list(color='rgb(246,80,80)')) %>%

    add_trace(y = ~minThi,name='Top 10% Min T',showlegend = F,line=list(color='transparent')) %>%
    add_trace(y = ~minTlo,name='Low 10% Min T',showlegend = F,fill='tonexty',fillcolor='rgba(53,118,190,0.2)') %>%

    add_trace(y = ~mint,name='Min T',showlegend = F,line=list(color='rgb(53,118,190)')) %>%

    layout(xaxis=a,yaxis=b,margin=margin)

}

getFName <- function(LOCATION,YEAR,CTYPE,GTYPE,TABNAME){
  if(TABNAME == 'Chill'){
    cTypes <- c('CP','Hours')
    dataType <- cTypes[as.numeric(CTYPE)]
  } else if (TABNAME == 'Growing Degrees'){
    gTypes <- c('GDHours','GDDays')
    dataType <- gTypes[as.numeric(GTYPE)]
  } else {
    dataType <- TABNAME
  }
  paste(siteInfo$stnID[as.numeric(LOCATION)],YEAR,dataType,sep='_')
}

makePDF <- function(YEAR,CHILLTYPE,LOCATION,Y2DATE){
  pdf(file='myGenerated.pdf',width=12,height=8)
  doThePlot(YEAR,CHILLTYPE,LOCATION,Y2DATE)
  dev.off()
}

makeJPEG <- function(YEAR,CTYPE,GTYPE,LOCATION,Y2DATE,DATESTART,DATEEND,HEIGHT,TABS){
  WIDTH = HEIGHT * 1200 / 800
  cat(HEIGHT,WIDTH,'\n')
  jpeg(file='myGenerated.jpg',width=WIDTH,height=HEIGHT,quality=100)
  if(TABS == 'Growing Degrees') {
    doTheHeatPlot(YEAR,GTYPE,DATESTART,LOCATION,Y2DATE)
  }
  if(TABS == 'Chill'){
    startJDay <- as.numeric(format(DATESTART,'%j'))
    doThePlot(YEAR,CTYPE,LOCATION,Y2DATE,DATESTART)
  }
  if(TABS == 'Temperature'){
    doTheTempPlot(YEAR,DATESTART,DATEEND,LOCATION)
  }

  dev.off()
}

