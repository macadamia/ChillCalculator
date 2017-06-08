siteInfo <- readRDS('Data/SiteInfo.rds')
gaz <- readRDS('Data/Gazetteer2010.rds')
THEURL <- readRDS('Data/extraInfo.rds')
TheAPIKey <- readRDS('Data/WillyWeather.rds')

#WillyWeatherIDs <- readRDS('Data/WillyWeatherInfo.rds')


useAPSIM <- T

debug <- F

if(!useAPSIM){
  longPaddock <- readRDS('Data/LongPaddock.rds')
  library(RCurl)
}

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
  ##print(location)
  this <- which(starts_with(gaz$PlaceName,location))
  ##cat('this is',length(this),'\n')
  if(length(this) == 1){
    return(list(searchedSite=gaz$PlaceStatePostCode[this],searchedLat=gaz$Latitude[this],searchedLng=gaz$Longitude[this],these=this))
  }
  if(length(this) < 1){
    #print('nothing')
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

getMet<-function(stn,startDate,endDate){
  if(debug)
    cat('getMet',startDate,endDate,'\n')
  #check if it is already in data
  theYear <- as.numeric(format(startDate,'%Y'))
  if(startDate == as.Date('1981-01-01') & endDate == as.Date('2010-12-31')){
    fName <- file.path('Data',paste(paste(stn,1981,2010,sep='_'),'RData',sep='.'))
  } else {
    fName <- file.path('Data',paste(paste(stn,theYear,sep='_'),'RData',sep='.'))
  }
  goodToGo <- F
  fileFound <- F
  if(file.exists(fName)){
    if(debug){
      cat('Found',fName,'\n')
    }
    fileFound <- T
    load(fName)
    #does it include the end date?
    nDays <- nrow(tab.1)
    lastDate <- as.Date(paste(tab.1$year[nDays],tab.1$day[nDays],sep='-'),'%Y-%j')
    if(lastDate >= endDate){
      metDates <- as.Date(paste(tab.1$year,tab.1$day,sep='-'),'%Y-%j')
      start <- which(metDates == startDate)
      end <- which(metDates == endDate)
      nDays <- end - start + 1
      if(debug)
        cat('return',nDays,'days\n')
      result <- tab.1[start:end,]
      goodToGo <- T
    } else {
      goodToGo <- F
    }
  }
  if(!fileFound | (fileFound & !goodToGo)){
    if(useAPSIM){
      if(debug)
        print('APSIM')
      # sYear <- as.numeric(format(startDate,'%Y'))
      # sMth <- as.numeric(format(startDate,'%m'))
      # sDay <- as.numeric(format(startDate,'%d'))
      sYear <- theYear
      sMth <- 1
      sDay <- 1
      if(fileFound & !goodToGo) {
        nextDate <- lastDate + 1
        cat('update the tab.1 already loaded',as.character(lastDate),as.character(nextDate),'\n')
        sMth <- as.numeric(format(nextDate,'%m'))
        sDay <- as.numeric(format(nextDate,'%d'))
      }
      eYear <- as.numeric(format(endDate,'%Y'))
      eMth <- as.numeric(format(endDate,'%m'))
      eDay <- as.numeric(format(endDate,'%d'))
      theurl<-paste(THEURL,stn,sep="")
      theurl<-paste(theurl,"&ddStart=",sDay,"&mmStart=",sMth,"&yyyyStart=",sYear,"&ddFinish=",eDay,"&mmFinish=",eMth,sep="")
      theurl<-paste(theurl,"&yyyyFinish=",eYear,sep="")
      conn<-url(theurl)
      if(debug)
        print(theurl)
      t.p<-try(hdr <- readLines(conn, 20, FALSE))
      if(!inherits(t.p, "try-error")){
        tab.2 <- read.table(conn,header=F,skip=20,col.names=unlist(strsplit(hdr[19], " +")))
        if(fileFound & !goodToGo) {
          cat('Updating tab1 with tab2',nrow(tab.1),nrow(tab.2),'\n')
          tab.1 <- rbind(tab.1,tab.2)
        } else {
          tab.1 <- tab.2
        }
        result <- tab.1
      } else {
        result <- NULL
      }
    } else {
      sDate <- format(startDate,'%Y%m%d')
      eDate <- format(endDate,'%Y%m%d')
      theurl <- paste('https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=apsim&station=',stn,'&start=',sDate,'&finish=',eDate,'&username=',longPaddock$Username,'&password=',longPaddock$Password,sep='')
      #print(theurl)
      res <- try(d <- getURL(theurl),silent = T)
      if(inherits(res,'try-error')){
        result <- NULL
      } else {

        info <- strsplit(d,'\n')[[1]]
        hdr <- strsplit(info[22],' +')[[1]]
        data <-info[24:length(info)]

        tab.2 <- read.table(textConnection(data))
        if(fileFound & !goodToGo) {
          tab.1 <- rbind(tab.1,tab.2)
        } else {
          tab.1 <- tab.2
        }
        result <- tab.1
      }
    }
    save(tab.1,file=fName)
  } #!fileFound | (fileFound & !goodToGo))
  return(result)

}

getMetHeat<-function(stn,startDate,endDate){
  if(debug)
    cat('getMetHeat',startDate,endDate,'\n')
  if(useAPSIM){
    if(debug)
      print('APSIM')
    sYear <- as.numeric(format(startDate,'%Y'))
    sMth <- as.numeric(format(startDate,'%m'))
    sDay <- as.numeric(format(startDate,'%d'))

    eYear <- as.numeric(format(endDate,'%Y'))
    eMth <- as.numeric(format(endDate,'%m'))
    eDay <- as.numeric(format(endDate,'%d'))
    theurl<-paste(THEURL,stn,sep="")
    theurl<-paste(theurl,"&ddStart=",sDay,"&mmStart=",sMth,"&yyyyStart=",sYear,"&ddFinish=",eDay,"&mmFinish=",eMth,sep="")
    theurl<-paste(theurl,"&yyyyFinish=",eYear,sep="")
    conn<-url(theurl)
    if(debug)
      print(theurl)
    t.p<-try(hdr <- readLines(conn, 20, FALSE))
    if(!inherits(t.p, "try-error")){
      result <- read.table(conn,header=F,skip=20,col.names=unlist(strsplit(hdr[19], " +")))
    } else {
      result <- NULL
    }
  } else {
    sDate <- format(startDate,'%Y%m%d')
    eDate <- format(endDate,'%Y%m%d')
    theurl <- paste('https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=apsim&station=',stn,'&start=',sDate,'&finish=',eDate,'&username=',longPaddock$Username,'&password=',longPaddock$Password,sep='')
    #print(theurl)
    res <- try(d <- getURL(theurl),silent = T)
    if(inherits(res,'try-error')){
      result <- NULL
    } else {

      info <- strsplit(d,'\n')[[1]]
      hdr <- strsplit(info[22],' +')[[1]]
      data <-info[24:length(info)]

      result <- read.table(textConnection(data))
    }
  }
  return(result)

}

# getWillyWeather(stn){
#   # change BoM stn number into WillyWeather id
#
#   fcast <- getURL('https://api.willyweather.com.au/v2/',TheAPIKey,'/locations/',id,'/weather.json?forecasts=temperature&days=7')
# }

getLTCold <- function(tab.LT,sJDay,eJDay, lat, CHILLTYPE){

  if(debug)
    print('getLTCold')
  #print(dim(tab.LT))
  year <- tab.LT[,1]
  day <- tab.LT[ ,2]
  maxt <- tab.LT[,4]
  mint <- tab.LT[,5]

  chillWeather<-data.frame(year,day,maxt,mint)
  colnames(chillWeather)<-c('Year','JDay','Tmax','Tmin')
  THourly<-make_hourly_temps(lat,chillWeather)
  stack<-stack_hourly_temps(hour_file=THourly)
  #get chill and heating info
  ch<-chilling_hourtable(stack,sJDay)
  Hour <- ch$Hour
  JDay <- ch$JDay[Hour == 24]
  Year <- ch$Year[Hour == 24]
  if(CHILLTYPE == 1){
    cold <- ch$Chill_Portions[Hour == 24]

  }
  if(CHILLTYPE == 2){
    cold <- ch$Chilling_Hours[Hour == 24]
  }
  if(CHILLTYPE == 3){
    cold <- ch$Chill_Units[Hour == 24]
  }

  cold.tab <- tapply(cold,list(Year,JDay),mean)[,sJDay:eJDay]


  CU <- colMeans(cold.tab)
  CUHot <- apply(cold.tab,2,quantile,probs=0.1,na.rm=T)
  CUCold <- apply(cold.tab,2,quantile,probs=0.9,na.rm=T)

  return(list(CU=CU,CUHot=CUHot,CUCold=CUCold))

}

getLTGDH<-function(stn,lat,startDate,endDate,metOnly){ #long-term data for GDH
  if(debug)
    print('getLTGDH')
  sJDay <- as.numeric(format(startDate,'%j'))
  sYear <- as.numeric(format(startDate,'%Y'))

  eJDay <- as.numeric(format(endDate,'%j'))
  eYear <- as.numeric(format(endDate,'%Y'))

  nDays <- as.numeric(endDate - startDate) + 1
  ltYears <- seq(1981,2010)
  nYrs <- eYear - sYear  # allows for the situation where we look from one year to the next, i.e. across summer

  ltStart <- as.Date(paste(1981,sJDay,sep='-'),'%Y-%j')
  #here's  a thing
  if(leap_year(eYear)){
    if(eJDay == 366){
      eJDay <- 365
    }
  }
  ltEnd <- as.Date(paste(2010+nYrs,eJDay,sep='-'),'%Y-%j')

  tab.LT <- getMetHeat(stn,ltStart,ltEnd)

  if(is.null(tab.LT)){
    return(NULL)
  } else {
    if(metOnly){
      return(tab.LT)
    } else{
      newGDH <- matrix(NA,nrow=length(ltYears), ncol = nDays)
      year <- tab.LT[,1]
      day <- tab.LT[ ,2]
      theseDates <- as.Date(paste(year,day,sep='-'),'%Y-%j')

      # fing leap years
      for(i in 1:length(ltYears)){
        yr <- ltYears[i]
        thisStart <- as.Date(paste(yr,sJDay,sep='-'),'%Y-%j')
        thisEnd <- as.Date(paste(yr+nYrs,eJDay,sep='-'),'%Y-%j')
        these <- which(theseDates >= thisStart & theseDates <= thisEnd)
        tab.sub <- tab.LT[these, ]
        results <- calcHeat(tab.sub,lat, sJDay) #return(list(gdh=gdh,units=units,maxHeat=maxHeat,jday=ch$JDay[these],hours=ch$Hour[these]))
        hours <- results$hours
        leapYear <- F
        for(y in seq(yr,(yr+nYrs))){
          leapYear <- leap_year(y)
        }
        if(leapYear){
          theDates <- seq.Date(as.Date(paste(yr,sJDay,sep='-'),'%Y-%j'),as.Date(paste(yr+nYrs,eJDay,sep='-'),'%Y-%j'),'days')
          leapdayIndex <- grep("02-29",as.character(theDates))
          if(length(leapdayIndex) > 0 ) {
            gdh <- results$gdh[hours==24][-leapdayIndex]
          } else {
            gdh <- results$gdh[hours==24]
          }
        } else {
          gdh <- results$gdh[hours==24]
        }
        newGDH[yr-1980,1:length(gdh)] <- gdh
      }
      return(newGDH)
    }
  }
}


calcChill <- function(tab.1,lat,sJDay,eJDay,CHILLTYPE){
  if(debug)
    print('calcChill')
  if(debug){
    print(dim(tab.1))
  }
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


  if(CHILLTYPE == 1){
    chill<-ch$Chill_Portions
    units<-'Chill Portions'
    these<-1:length(ch$JDay)
  }
  if(CHILLTYPE == 2){
    chill<-ch$Chilling_Hours
    units<-'Chiling Hours < 7.2ºC'
    these<-1:length(ch$JDay)
  }
  if(CHILLTYPE == 3){
    these<-which(ch$JDay >= sJDay & ch$JDay <= eJDay)
    chill<-ch$Chill_Units[these]
    units<-'Utah Chill Units'
  }

  maxChill<-round(max(chill),1)

  return(list(chill=chill,units=units,maxChill=maxChill,jday=ch$JDay[these],hours=ch$Hour[these]))
}

calcHeat <- function(tab.met,lat,sJDay){
  if(debug)
    print('calcHeat')
  #cat('just arrived calcHeat lat',lat,'\n')
  year <- tab.met[,1]
  day <- tab.met[ ,2]
  maxt <- tab.met[,4]
  mint <- tab.met[,5]

  chillWeather<-data.frame(year,day,maxt,mint)
  colnames(chillWeather)<-c('Year','JDay','Tmax','Tmin')
  #cat('calcHeat lat',lat,'\n')
  THourly<-make_hourly_temps(lat,chillWeather)
  stack<-stack_hourly_temps(hour_file=THourly)
  #get chill and heating info
  ch<-chilling_hourtable(stack,sJDay)

  gdh<-ch$GDH
  units<-"Growing Degree Hours"
  these<-1:length(ch$JDay)

  maxHeat<-round(max(gdh),1)
  #cat('leaving calcHeat lat',lat,'\n')
  return(list(gdh=gdh,units=units,maxHeat=maxHeat,jday=ch$JDay[these],hours=ch$Hour[these]))
}


#doThePlot(input$yearInput,input$cType,site$currentLoc,input$startDate)
doThePlot <- function(CHILLTYPE,LOCATION,STARTDATE,EDATE){

  # YEAR<-2017
  # CHILLTYPE<-1
  # LOCATION<-317
  # STARTDATE<-as.Date('2017-3-1')
  # EDATE<-as.Date('2017-05-21')

  if(debug)
    print('doThePlot')

  #need to check the year in case we did a GD with across the years
  sYear <- as.numeric(format(STARTDATE,'%Y'))
  eYear <- as.numeric(format(EDATE,'%Y'))
  if(eYear != sYear){
    #print('Update the end year from previous multiyear')
    eJDay <- as.numeric(format(EDATE,'%j'))
    EDATE <- as.Date(paste(sYear,eJDay,'sep=-'),'%Y-%j')
    #now update the display

  }

  #cat(as.character(STARTDATE),' ',as.character(EDATE),'\n')

  Year <- as.numeric(format(STARTDATE,'%Y'))
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


  tab.1<-getMet(stn,STARTDATE,EDATE)
  if(!is.null(tab.1)){
    if(debug)
      cat('Got met, now calc chill for',Year,'\n')
    res <- calcChill(tab.1,lat,sJDay,eJDay,CHILLTYPE)
    chill <- res$chill
    maxChill <- res$maxChill
    jday <- res$jday
    hours <- res$hours
    hour24 <- which(hours==24)

    jday <- jday[hour24]
    chill <- chill[hour24]

    #does eJDay == tail(jday,1)
    todayDate <- format(EDATE,'%d %b %Y')
    if( eJDay != tail(jday,1) ){
      eJDay <- tail(jday,1)
      actualEndDate <- as.Date(paste(eYear,eJDay,sep='-'),'%Y-%j') # not sure what to do with that
      todayDate <- format(actualEndDate,'%d %b %Y')
    }
    chill <- chill[which(jday == sJDay):which(jday == eJDay)]

  }
  #cat('Get LT Data\n')
  tab.LT <- getMet(stn,as.Date('1981-01-01'),as.Date('2010-12-31'))
  if(!is.null(tab.LT)){
    #cat('Calc LT Chill\n')
    LTData <- getLTCold(tab.LT,sJDay, eJDay,lat, CHILLTYPE)

    LTChill <- LTData$CU
    LTHot <- LTData$CUHot
    LTCold <- LTData$CUCold

    JDays <- sJDay:eJDay

    labs<-as.Date(paste(Year,JDays,sep='-'),'%Y-%j')
    nJdays <- length(JDays)

    notNA <- which(!is.na(chill))

    chill <- chill[notNA]

    labs<-as.Date(paste(Year,JDays,sep='-'),'%Y-%j')

    today <- tail(chill,1)

    if(today > 0){
      chillMessage <- paste(stnName,"Chill Accumulated (",as.character(todayDate),"):" ,round(today,0),YLAB)
    } else {
      chillMessage <- paste(stnName,'No Chill has accumulated')
    }

    #cat('create data frame theData\n')
    #cat('JDays',length(JDays),'labs',length(labs),'chill',length(chill),'LTHot',length(LTHot),'\n')
    theData <- data.frame(JDays,labs, chill,LTHot,LTCold)
    #return(list(theData =data.frame(JDays,labs, chill,LTHot,LTCold),YLAB=YLAB,chillMessage=chillMessage))
    b <- list(
      title = YLAB,
      titlefont = f1,
      showticklabels = TRUE,
      tickangle = 0,
      tickfont = f1
    )

    #cat('do the plot\n')
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
}

#doTheHeatPlot(selectedYear$Year,input$gType,input$startDate,input$endDate,site$currentLoc,input$baseTemp)
doTheHeatPlot <- function(YEAR,GTYPE,SDATE,EDATE,LOCATION,BASETEMP){
#
  # YEAR <- '2017'
  # YEAR2 <- '2017'
  # GTYPE <- 2
  # SDATE <- as.Date(paste(YEAR,'05-01',sep='-'))
  # EDATE <- as.Date(paste(YEAR2,'06-07',sep='-'))
  # LOCATION <- 317
  # BASETEMP <- '10'

  if(debug)
    print('doTheHeatPlot')

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

  tab.1 <- getMetHeat(stn,SDATE,EDATE)
  if(!is.null(tab.1)){
    if(GTYPE == 1) {
      #cat('Latitude',lat,'\n')
      res <- calcHeat(tab.1,lat,sJDay)
      gdh <- res$gdh
      YLAB <- res$units
      maxGD <- res$maxGDH
      jday <- res$jday
      hours <- res$hours
      hour24 <- which(hours==24)

      jday <- jday[hour24]
      gdh <- gdh[hour24]
      #gdh[jday < sJDay] <- NA
      gd <- gdh

    } else {
      gTmp <- ((tab.1[,4] + tab.1[,5]) / 2) - as.numeric(BASETEMP)
      gTmp[gTmp < 0] <- 0
      gd <- cumsum(gTmp)
      # jday <- tab.1[,'day']
      # gdd[ jday < sJDay] <- NA
      #gd <- gdd - gdd[1]
      maxGD <- max(gd,na.rm=T)
      YLAB <- paste('Growing Degree Days (base =',BASETEMP,'ºC)')
    }
  }

  #if( (as.numeric(BASETEMP) != GDDb & GTYPE == 2) | sYear != eYear ){ # GDD
  if(GTYPE == 2){
    #print('this is the long term met data only')
    metOnly <- T
    tab.LT <- getLTGDH(stn,lat,SDATE,EDATE,metOnly) # this is the long term met data only
    #GDD
    gdd <- (tab.LT[,4] + tab.LT[,5])/2 - as.numeric(BASETEMP)
    gdd[gdd < 0] <- 0
    gddTab <- tapply(gdd,list(tab.LT[,1],tab.LT[,2]),max,na.rm=T)

    #gdd <- t(apply(gddTab,1,sum,na.rm=T))
    GDD <- colMeans(gddTab,na.rm=T)
    GDD <- cumsum(GDD[heatJDays])
    GDDHot <- apply(gddTab,2,quantile,probs=0.9,na.rm=T)
    GDDHot <- cumsum(GDDHot[heatJDays])
    GDDCold <- apply(gddTab,2,quantile,probs=0.1,na.rm=T)
    GDDCold <- cumsum(GDDCold[heatJDays])
  }

  if(GTYPE == 1){ #} & sYear != eYear){ # recalculate the GDH
    #print("recalculate the GDH")
    metOnly <- F
    newLT <- getLTGDH(stn,lat,SDATE,EDATE,metOnly) # get the long term GDH data for this range of days
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



  today <- max(gd,na.rm=T)

  maxGD <- max(c(LTHot,gd),na.rm=T)
  YLIM <- c(0,max(LTGD,na.rm=T))

  theData <- data.frame(date=heatDates,gd=gd,LTGD=LTGD,LTHot=LTHot,LTCold=LTCold)



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

doTheTempPlot <- function(YEAR,SDATE,EDATE,LOCATION){

  if(debug)
    print('doTheTempPlot')

  Year <- as.numeric(YEAR)

  sJDay<-as.numeric(format(SDATE,'%j'))
  eJDay<-as.numeric(format(EDATE,'%j'))


  stn<-siteInfo$stnID[LOCATION]
  lat<-siteInfo$latitude[LOCATION]
  stnName<-siteInfo$Name[LOCATION]

  rdata <- file.path('Data',paste(stn,'.RData',sep=''))
  load(rdata)

  #curretly this is all calendar year stuff

  tab.1<-getMet(stn,SDATE,EDATE)
  if(debug)
    cat('returned data has',nrow(tab.1),'from day',sJDay,'to',eJDay,'Total Days:',eJDay - sJDay + 1,'\n')
  maxt <- tab.1$maxt
  mint <- tab.1$mint
  jday <- tab.1$day

  YLIM <- c(min(c(maxt,mint,minTCold,maxTHot),na.rm=T),max(c(maxt,mint,minTCold,maxTHot),na.rm=T))

  JDays <- sJDay:eJDay
  labs<-as.Date(paste(Year,JDays,sep='-'),'%Y-%j')
  theData <- data.frame(date=labs,maxt=maxt,mint=mint,
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


