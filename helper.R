siteInfo <- readRDS('Data/SiteInfo.rds')
gaz <- readRDS('Data/Gazetteer2010.rds')
THEURL <- readRDS('Data/extraInfo.rds')

gaz$PlaceName <- as.character(gaz$PlaceName)

starts_with <- function(vars, match, ignore.case = TRUE) {
  if (ignore.case) match <- tolower(match)
  n <- nchar(match)

  if (ignore.case) vars <- tolower(vars)
  substr(vars, 1, n) == match
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
  title = "",
  type='date',
  titlefont = f1,
  tickfont = f1,
  showticklabels = TRUE,
  nticks= 20,
  tickangle = 45,
  tickformat = "%d %b",
  hoverformat = "%d %b"
)

margin <- list(l=60, r=40,b=80,t= 40,pad=0)

searchForLocation <- function(location){
  #listOfStns
  this <- grep(tolower(location),tolower(siteInfo$Name),fixed = T)
  if(length(this) == 1){
    return(list(searchedSite=siteInfo$Name[this],searchedLat=siteInfo$latitude[this],searchedLng=siteInfo$longitude[this], these=this))
  }
  if(length(this) < 1){
    return(NULL)
  }
  if(length(this) > 1){
    return(list(N=length(this),these=this))
  }
}

searchForPlace<- function(location){
  #listOfTowns
  #print(location)
  this <- which(starts_with(gaz$PlaceName,location))
  #cat('this is',length(this),'\n')
  if(length(this) == 1){
    return(list(searchedSite=gaz$PlaceStatePostCode[this],searchedLat=gaz$Latitude[this],searchedLng=gaz$Longitude[this],these=this))
  }
  if(length(this) < 1){
    print('nothing')
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

getMetGDH<-function(stn,sYear,sMth,sDay,eYear,eMth,eDay){
  theurl<-paste(THEURL,stn,sep="")
  theurl<-paste(theurl,"&ddStart=",sDay,"&mmStart=",sMth,"&yyyyStart=",sYear,"&ddFinish=",eDay,"&mmFinish=",eMth,sep="")
  theurl<-paste(theurl,"&yyyyFinish=",eYear,sep="")
  conn<-url(theurl)
  t.p<-try(hdr <- readLines(conn, 20, FALSE))
  if(!inherits(t.p, "try-error")){
    tab.1 <- read.table(conn,header=F,skip=20,col.names=unlist(strsplit(hdr[19], " +")))
    return (tab.1)
  } else {
    return(NA)
  }
}


getMetLT<-function(stn){ #long-term data
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

getLTGDH<-function(stn,sYear,sMth,sDay,eYear,eMth,eDay,metOnly){ #long-term data for GDH
  sdate <- as.Date(paste(sYear,sMth,sDay,sep='-'),'%Y-%m-%d')
  sJDay <- as.numeric(format(sdate,'%j'))
  edate <- as.Date(paste(eYear,eMth,eDay,sep='-'),'%Y-%m-%d')
  nDays <- as.numeric(edate - sdate) + 1
  ltYears <- seq(1981,2010)
  newGDH <- matrix(0,nrow=length(ltYears), ncol = nDays)
  theurl<-paste(THEURL,stn,sep="")
  theurl<-paste(theurl,"&ddStart=",1,"&mmStart=",1,"&yyyyStart=",1981,"&ddFinish=",31,"&mmFinish=",12,sep="")
  theurl<-paste(theurl,"&yyyyFinish=",2011,sep="")
  conn<-url(theurl)
  hdr <- readLines(conn, 20, FALSE)
  tab.1 <- read.table(conn,header=F,skip=20,col.names=unlist(strsplit(hdr[19], " +")))
  if(metOnly){
    return(tab.1)
  }
  year <- tab.1[,1]
  day <- tab.1[ ,2]
  theseDates <- as.Date(paste(year,day,sep='-'),'%Y-%j')

    # fing leap years
  for(i in 1:length(ltYears)){
    yr <- ltYears[i]
    thisStart <- as.Date(paste(yr,sMth,sDay,sep='-'),'%Y-%m-%d')
    thisEnd <- as.Date(paste(yr+1,eMth,eDay,sep='-'),'%Y-%m-%d')
    these <- which(theseDates >= thisStart & theseDates <= thisEnd)
    tab.sub <- tab.1[these, ]
    results <- calcHeat(tab.sub,lat, sJDay) #return(list(gdh=gdh,units=units,maxHeat=maxHeat,jday=ch$JDay[these],hours=ch$Hour[these]))
    hours <- results$hours
    if(leap_year(yr) | leap_year(yr+1)){
      theDates <- seq.Date(as.Date(paste(yr,sMth,sDay,sep='-'),'%Y-%m-%d'),as.Date(paste(yr+1,eMth,eDay,sep='-'),'%Y-%m-%d'),'days')
      leapdayIndex <- grep(paste(yr,"02-29",sep='-'),as.character(theDates)) + grep(paste(yr+1,"02-29",sep='-'),as.character(theDates))
      if(length(leapdayIndex) > 0 ) {
        gdh <- results$gdh[hours==24][-leapdayIndex]
      }
    } else {
      gdh <- results$gdh[hours==24]
    }
    newGDH[yr-1980,1:nDays] <- gdh
  }
  return(newGDH)
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

calcHeat <- function(tab.1,lat,sJDay){
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


# calcLT <- function(tab.1,lat,sJDay){ # long-Term data
#   year <- tab.1[,1]
#   day <- tab.1[ ,2]
#   maxt <- tab.1[,4]
#   mint <- tab.1[,5]
#
#   chillWeather<-data.frame(year,day,maxt,mint)
#   colnames(chillWeather)<-c('Year','JDay','Tmax','Tmin')
#   THourly<-make_hourly_temps(lat,chillWeather)
#   stack<-stack_hourly_temps(hour_file=THourly)
#   #get chill and heating info
#   ch<-chilling_hourtable(stack,sJDay)
#
#   return(ch)
# }

#doThePlot(input$yearInput,input$cType,site$currentLoc,input$Y2DateChill,input$startDate)
doThePlot <- function(YEAR,CHILLTYPE,LOCATION,Y2DATE,STARTDATE,EDATE){

  # YEAR<-2010
  # CHILLTYPE<-3
  # Y2DATE<-1
  # LOCATION<-317
  # STARTDATE<-as.Date('2016-5-1')
  # EDATE<-as.Date('2016-9-1')

  print('hello')

  Year <- as.numeric(YEAR)
  sJDay <- as.numeric(format(STARTDATE,'%j'))
  eJDay <- as.numeric(format(EDATE,'%j'))

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

  cat('loading',stnName,'\n')

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

    LTChillStart <- LTChill[sJDay]
    LTHotStart <- LTHot[sJDay]
    LTColdStart <- LTCold[sJDay]

    chillStart <- head(chill,1)
  }

  lastJDay <- 365
  if(Y2DATE == 1){
    lastJDay <- min(max(jday),lastJDay,eJDay)
  }
  JDays <- sJDay:lastJDay


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
    chill <- chill - chillStart
    LTHot <- LTHot[sJDay:lastJDay] - LTHot[sJDay]
    LTCold <- LTCold[sJDay:lastJDay] - LTCold[sJDay]

  }

  today <- tail(chill,1)
  todayDate <- format(EDATE,'%d %b %Y')
  if(today > 0){
    chillMessage <- paste(stnName,"Chill Accumulated (",as.character(todayDate),"):" ,round(today,0),YLAB)
  } else {
    chillMessage <- paste(stnName,'No Chill has accumulated')
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
                 line=list(color='transparent'),showlegend = F) %>%
    add_trace(y = ~LTCold,name='Coolest 10%',showlegend = F,fill='tonexty',fillcolor='rgba(53,118,190,0.5)') %>%
    add_trace(y = ~chill,name='This Year',showlegend = F,line=list(color='rgb(53,118,190)')) %>%
    layout(xaxis=a,yaxis=b,margin=margin,title=chillMessage)
  } else {
    plot_ly(theData, x = ~labs, y = ~LTHot,  type = "scatter", mode='lines',name='Warmest 10%',
                 line=list(color='transparent'),showlegend = F)  %>%
    add_trace(y = ~LTCold,name='Coolest 10%',showlegend = F,fill='tonexty',fillcolor='rgba(53,118,190,0.5)') %>%
    add_trace(y = ~chill,name='This Year',showlegend = F,line=list(color='rgb(53,118,190)')) %>%
    layout(xaxis=a,yaxis=b,margin=margin,title=chillMessage)
  }
}

#doTheHeatPlot(selectedYear$Year,input$gType,input$startDate,input$endDate,site$currentLoc,input$Y2DateGDH,input$baseTemp)
doTheHeatPlot <- function(YEAR,GTYPE,SDATE,EDATE,LOCATION,Y2DATE,BASETEMP){

  YEAR <- '2014'
  GTYPE <- 1
  SDATE <- as.Date('2014-05-01')
  EDATE <- as.Date('2015-05-8')
  LOCATION <- 317
  Y2DATE <- T
  BASETEMP <- '10'

  heatDates <- seq.Date(SDATE,EDATE,'days')
  heatJDays <- as.numeric(format(heatDates,'%j'))

  Year <- as.numeric(YEAR)

  sYear <- as.numeric(format(SDATE,'%Y'))
  eYear <- as.numeric(format(EDATE,'%Y'))
  sMth <- as.numeric(format(SDATE,'%m'))
  eMth <- as.numeric(format(EDATE,'%m'))
  sDay <- as.numeric(format(SDATE,'%d'))
  eDay <- as.numeric(format(EDATE,'%d'))

  sJDay<-as.numeric(format(SDATE,'%j'))
  eJDay<-as.numeric(format(EDATE,'%j'))


  stn<-siteInfo$stnID[LOCATION]
  lat<-siteInfo$latitude[LOCATION]
  stnName<-siteInfo$Name[LOCATION]
  #cat(Year,sYear,eYear,'\n')
  #if(sYear == eYear){ # just one year so grab the ready-made data
    print("Loading std data")
    rdata <- file.path('Data',paste(stn,'.RData',sep=''))
    load(rdata)
  #}

  tab.1<-getMetGDH(stn,sYear,sMth,sDay,eYear,eMth,eDay)
  if(!any(is.na(tab.1))){
    if(GTYPE == 1) {
      res <- calcHeat(tab.1,lat,sJDay)
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
      gTmp <- ((tab.1[,'maxt'] + tab.1[,'mint']) / 2) - as.numeric(BASETEMP)
      gTmp[gTmp < 0] <- 0
      gdd <- cumsum(gTmp)
      jday <- tab.1[,'day']
      gdd[ jday < sJDay] <- NA
      gd <- gdd - gdd[1]
      maxGD <- max(gd,na.rm=T)
      YLAB <- paste('Growing Degree Days (base =',BASETEMP,'ºC)')
    }
  }

  if( (as.numeric(BASETEMP) != GDDb & GTYPE == 2) | sYear != eYear ){ # GDD
    tab.LT <- getLTGDH(stn,sYear,sMth,sDay,eYear,eMth,eDay,T) # this is the long term met data only
    #GDD
    gdd <- (tab.LT[,'maxt'] + tab.LT[,'mint'])/2 - as.numeric(BASETEMP)
    gdd[gdd < 0] <- 0
    gddTab <- tapply(gdd,list(tab.LT[,'year'],tab.LT[,'day']),max)

    gdd <- t(apply(gddTab,1,cumsum))
    GDD <- colMeans(gdd)
    GDDHot <- apply(gdd,2,quantile,probs=0.9,na.rm=T)
    GDDCold <- apply(gdd,2,quantile,probs=0.1,na.rm=T)
  }

  if(GTYPE == 1 & sYear != eYear){ # recalculate the GDH
    newLT <- getLTGDH(stn,sYear,sMth,sDay,eYear,eMth,eDay,F) # get the long term GDH data for this range of days
    GDH <- colMeans(newLT)
    GDHHot <- apply(newLT,2,quantile,probs=0.9,na.rm=T)
    GDHCold <- apply(newLT,2,quantile,probs=0.1,na.rm=T)
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

  ##########  Needs Work ####################
  JDays <- heatJDays
  #labs<-as.Date(paste(Year,JDays,sep='-'),'%Y-%j')
  theData <- data.frame(JDays=JDays,date=heatDates,gd=gd,LTGD=LTGD[sJDay:lastJDay],
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

    add_trace(y = ~LTCold,name='Coolest 10%',showlegend = F,fill='tonexty',fillcolor='rgba(246,100,100,0.5)') %>%

    add_trace(y = ~gd,name='This Year',showlegend = F,line=list(color='rgb(246,80,80)')) %>%


    layout(xaxis=a,yaxis=b,margin=margin,title=stnName) #,plot_bgcolor="rgb(126,126,126)"

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

    add_trace(y = ~maxTlo,name='Low 10% Max T',showlegend = F,fill='tonexty',fillcolor='rgba(246,100,100,0.5)') %>%

    add_trace(y = ~maxt,name='Max T',showlegend = F,line=list(color='rgb(246,80,80)')) %>%

    add_trace(y = ~minThi,name='Top 10% Min T',showlegend = F,line=list(color='transparent')) %>%
    add_trace(y = ~minTlo,name='Low 10% Min T',showlegend = F,fill='tonexty',fillcolor='rgba(53,118,190,0.5)') %>%

    add_trace(y = ~mint,name='Min T',showlegend = F,line=list(color='rgb(53,118,190)')) %>%

    layout(xaxis=a,yaxis=b,margin=margin, hovermode = 'closest',title=stnName)

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

makePDF <- function(YEAR,CHILLTYPE,LOCATION,Y2DATE,HQ){
  pdf(file='myGenerated.pdf',width=12,height=8)
  doThePlot(YEAR,CHILLTYPE,LOCATION,Y2DATE,HQ)
  dev.off()
}

makeJPEG <- function(YEAR,CTYPE,GTYPE,LOCATION,Y2DATE,DATESTART,DATEEND,HEIGHT,TABS,HQ){
  WIDTH = HEIGHT * 1200 / 800
  cat(HEIGHT,WIDTH,'\n')
  jpeg(file='myGenerated.jpg',width=WIDTH,height=HEIGHT,quality=100)
  if(TABS == 'Growing Degrees') {
    doTheHeatPlot(YEAR,GTYPE,DATESTART,LOCATION,Y2DATE,HQ)
  }
  if(TABS == 'Chill'){
    startJDay <- as.numeric(format(DATESTART,'%j'))
    doThePlot(YEAR,CTYPE,LOCATION,Y2DATE,DATESTART,HQ)
  }
  if(TABS == 'Temperature'){
    doTheTempPlot(YEAR,DATESTART,DATEEND,LOCATION,1,HQ)
  }

  dev.off()
}

