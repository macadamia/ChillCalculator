siteInfo <- readRDS('Data/SiteInfo.rds')
THEURL <- readRDS('Data/extraInfo.rds')

cols<-c('Year','JDay','Tmax','TMin')
hours<-paste('Hour',seq(1,24),sep='_')
cols<-c(cols,hours)


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
  units<-"Growing Degree Hours ('000s)"
  these<-1:length(ch$JDay)

  maxHeat<-round(max(gdh),1)

  return(list(gdh=gdh,units=units,maxHeat=maxHeat,jday=ch$JDay[these],hours=ch$Hour[these]))
}


calcLT <- function(tab.1,lat,sJDay){ # long-Term data
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

doThePlot <- function(YEAR,CHILLTYPE,LOCATION,Y2DATE,STARTJDAY,HQ){
  cat('doThePlot from helper.R',YEAR,CHILLTYPE,LOCATION,Y2DATE,HQ,'\n')
  if(HQ == 3){
    #Screen plot
    CEX <- 1
    CEX.AXIS <- 1
    CEX.LAB <- 1
    CEX.MAIN <- 1
    LWD.AXIS <- 1
    par(mar=c(5.1,4.1,4.1,2.1))
  } else {
    CEX <- 1.5
    CEX.AXIS <- 2
    CEX.LAB <- 2
    CEX.MAIN <- 2
    LWD.AXIS <- 2
    par(mar=c(5.1,5.1,4.1,2.1))
  }

  Year <- as.numeric(YEAR)
  sJDay<-STARTJDAY
  if(CHILLTYPE == 1){
    YLAB='Chill Portions'
    eJDay<-366
  }
  if(CHILLTYPE == 2){
    YLAB='Chill Hours'
    eJDay<-366
  }
  if(CHILLTYPE == 3){
    YLAB='Chill Units'
    eJDay<- 366 #as.numeric(format(as.Date(paste('31-08',YEAR,sep='-'),'%d-%m-%Y'),'%j'))
    print(eJDay)
  }


  stn<-siteInfo$stnID[LOCATION]
  lat<-siteInfo$latitude[LOCATION]
  stnName<-siteInfo$Name[LOCATION]
  cat('chillplot',stnName,stn,lat,'\n')

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
      plot(0,0,type='n',axes=F,xlab='',ylab='',main=aTitle)
      return(NULL)
    }
  } else {
    if(length(chill) < 1){
      aTitle <- paste("No Chill Accumulation Yet at",stnName)
      plot(0,0,type='n',axes=F,xlab='',ylab='',main=aTitle)
      return(NULL)
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
    lastJDay <- min(max(jday),lastJDay)
  }


  today <- max(chill)

  #YLIM <- c(0,max(LTCold,na.rm=T))

  if(CHILLTYPE == 3){ #Units
    LTChillStart <- LTChill[STARTJDAY]
    LTHotStart <- LTHot[STARTJDAY]
    LTColdStart <- LTCold[STARTJDAY]
    YLIM <- c(min(c(chill,LTHot[STARTJDAY:lastJDay] - LTHotStart),na.rm=T),max(c(chill,LTCold[STARTJDAY:lastJDay] - LTColdStart),na.rm=T))
  } else {
    YLIM <- c(0,max(c(chill,LTCold),na.rm=T))
  }

  par(family = "Helvetica")

  plot(c(1,lastJDay),c(0,1),xlab='Date',type='n',ylab=YLAB,axes=F,ylim=YLIM,
       cex.lab=CEX.LAB,main=stnName,cex.main=CEX.MAIN)

  Y1 <- 0.7 * YLIM[2] # first text
  Y2 <- 0.6 * YLIM[2] # etc


  currentYear <- as.numeric(format(Sys.Date(),'%Y'))
  POS <- 4 # to the right of X, Y
  X <- 30
  ffont <- 2
  if(CHILLTYPE == 1){

    if(Y2DATE == 2){
      axis(2,las=1,at=seq(20,max(LTHot),20),seq(20,max(LTHot),20),cex.axis=CEX.AXIS,lwd=LWD.AXIS,cex=CEX)
      abline(h=seq(20,max(LTHot),20),col='grey')
    } else{

      axis(2,las=1,at=pretty(YLIM),cex.axis=CEX.AXIS,lwd=LWD.AXIS,cex=CEX)
    }


    day34 <- jday[head(which(chill >= 34),1)]
    day34Date <- format(as.Date(paste(Year,day34,sep='-'),'%Y-%j'),'%d %b')

    if(YEAR == currentYear){
      text(X,Y1,paste('Today:',formatC(today,width=1),'Chill Portions'),cex=CEX,pos=POS, font = ffont)
    } else {
      text(X,Y1,paste('Max:',formatC(today,width=1),'Chill Portions'),cex=CEX,pos=POS, font = ffont)
    }
    #equiv chill hours
    eqCH <- CH[day34]
    text(X,Y2,paste('34 CP reached:',day34Date,'\nwhich is equivalent to:',round(eqCH,0),'Chill Hours'),cex=CEX,pos=POS, font = ffont)

  } else {
    axis(2,las=1,cex.axis=CEX.AXIS,lwd=LWD.AXIS,cex=CEX)

    if(YEAR == currentYear){
      text(X,Y1,paste('Today:',formatC(today,width=1)),cex=CEX,pos=POS, font = ffont)
    } else {
      text(X,Y1,paste('Max:',formatC(today,width=1)),cex=CEX,pos=POS, font = ffont)
    }
  }


  lines(jday,chill,lwd=3)

  ats<-pretty(1:lastJDay,20)
  labs<-format(as.Date(paste(Year,ats,sep='-'),'%Y-%j'),'%d %b')
  axis(1,at=ats,labs,las=1,cex.axis=CEX.AXIS,lwd=LWD.AXIS,cex=CEX)
  if(CHILLTYPE != 3){
    lines(1:lastJDay,LTChill[1:lastJDay],col='green',lwd=3,lty=2)
    lines(1:lastJDay,LTHot[1:lastJDay],col='red',lwd=3,lty=4)
    lines(1:lastJDay,LTCold[1:lastJDay],col='blue',lwd=3,lty=3)
  } else {
    #for Units we need to recalibrate from the start day of the year
    lines(STARTJDAY:lastJDay,LTChill[STARTJDAY:lastJDay] - LTChillStart,col='green',lwd=3,lty=2)
    lines(STARTJDAY:lastJDay,LTHot[STARTJDAY:lastJDay] - LTHotStart,col='red',lwd=3,lty=4)
    lines(STARTJDAY:lastJDay,LTCold[STARTJDAY:lastJDay] - LTColdStart,col='blue',lwd=3,lty=3)
  }
  legend('top',legend=c(Year,'Average (1981 - 2010)','Warmest 10%','Coolest 10%'),lwd=3,lty=c(1,2,4,3),col=c('black','green','red','blue'),bty='n',ncol=4,cex=CEX)
}

doTheHeatPlot <- function(YEAR,SDATE,LOCATION,Y2DATE,HQ){

  if(HQ == 3){
    #Screen plot
    CEX <- 1
    CEX.AXIS <- 1
    CEX.LAB <- 1
    CEX.MAIN <- 1
    LWD.AXIS <- 1
    par(mar=c(5.1,4.1,4.1,2.1))
  } else {
    CEX <- 1.5
    CEX.AXIS <- 2
    CEX.LAB <- 2
    CEX.MAIN <- 2
    LWD.AXIS <- 2
    par(mar=c(5.1,5.1,4.1,2.1))
  }
  Year <- as.numeric(YEAR)

  sJDay<-as.numeric(format(SDATE,'%j'))
  eJDay<-366


  stn<-siteInfo$stnID[LOCATION]
  lat<-siteInfo$latitude[LOCATION]
  stnName<-siteInfo$Name[LOCATION]
  cat('heatplot',stnName,stn,lat,'\n')

  rdata <- file.path('Data',paste(stn,'.RData',sep=''))
  load(rdata)

  #curretly this is all calendar year stuff

  tab.1<-getMet(stn,Year)
  if(!any(is.na(tab.1))){

    res <- calcHeat(tab.1,lat,sJDay,eJDay)
    gdh <- res$gdh
    YLAB=res$units
    maxGDH <- res$maxGDH
    jday <- res$jday
    hours <- res$hours
    hour24 <- which(hours==24)

    jday <- jday[hour24]
    gdh <- gdh[hour24]
    gdh[jday < sJDay] <- NA

  }


  LTGDH <- GDH
  LTHot <- GDHHot
  LTCold <- GDHCold


  if(sJDay !=1 ){
    LTGDH[jday < sJDay] <- NA
    LTHot[jday < sJDay] <- NA
    LTCold[jday < sJDay] <- NA

    LTGDH[jday >= sJDay] <- LTGDH[jday >= sJDay] - LTGDH[sJDay]
    LTHot[jday >= sJDay] <- LTHot[jday >= sJDay] - LTHot[sJDay]
    LTCold[jday >= sJDay] <- LTCold[jday >= sJDay] - LTCold[sJDay]
  }

  lastJDay <- 366
  if(Y2DATE == 1){
    lastJDay <- min(max(jday),lastJDay)
  }


  today <- max(gdh,na.rm=T)

  maxGDH <- max(c(LTHot,gdh),na.rm=T)
  YLIM <- c(0,max(LTGDH,na.rm=T))
  if(Y2DATE == 1){
    dataSet <- cbind(LTGDH,LTHot,LTCold)
    YLIM <- range(c(LTGDH[sJDay:lastJDay],LTHot[sJDay:lastJDay],LTCold[sJDay:lastJDay],gdh),na.rm=T)
  }

  print(YLIM)
  plot(c(1,lastJDay),c(0,1),xlab='Date',type='n',ylab=YLAB,axes=F,ylim=YLIM,
       cex.lab=CEX.LAB,main=stnName,cex.main=CEX.MAIN)

  if(Y2DATE == 2){
    minLTCold <- min(LTCold,na.rm=T)
    YLabels <- pretty(seq(minLTCold,maxGDH)/1000)
    axis(2,las=1,at=YLabels*1000,labels=YLabels,cex.axis=CEX.AXIS,lwd=LWD.AXIS,cex=CEX)
    abline(h=seq(minLTCold,maxGDH,length.out = 10),col='grey')
  } else {
    YLabels <- pretty(YLIM)/1000
    axis(2,las=1,at=YLabels*1000,labels=YLabels,cex.axis=CEX.AXIS,lwd=LWD.AXIS,cex=CEX)
  }

  lines(jday,gdh,lwd=3)

  ats<-pretty(1:lastJDay,20)
  labs<-format(as.Date(paste(Year,ats,sep='-'),'%Y-%j'),'%d %b')
  axis(1,at=ats,labs,las=1,cex.axis=CEX.AXIS,lwd=LWD.AXIS,cex=CEX)

  lines(1:lastJDay,LTGDH[1:lastJDay],col='green',lwd=3,lty=2)
  lines(1:lastJDay,LTHot[1:lastJDay],col='red',lwd=3,lty=4)
  lines(1:lastJDay,LTCold[1:lastJDay],col='blue',lwd=3,lty=3)
  legend('top',legend=c(Year,'Average (1981 - 2010)','Warmest 10%','Coolest 10%'),lwd=3,lty=c(1,2,4,3),col=c('black','green','red','blue'),bty='n',ncol=4,cex=CEX)
}

doTheTempPlot <- function(YEAR,SDATE,LOCATION,Y2DATE,HQ){

  if(HQ == 3){
    #Screen plot
    CEX <- 1
    CEX.AXIS <- 1
    CEX.LAB <- 1
    CEX.MAIN <- 1
    LWD.AXIS <- 1
    par(mar=c(5.1,4.1,4.1,2.1))
  } else {
    CEX <- 1.5
    CEX.AXIS <- 2
    CEX.LAB <- 2
    CEX.MAIN <- 2
    LWD.AXIS <- 2
    par(mar=c(5.1,5.1,4.1,2.1))
  }
  Year <- as.numeric(YEAR)

  sJDay<-as.numeric(format(SDATE,'%j'))
  eJDay<-366


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

  lastJDay <- 366
  if(Y2DATE == 1){
    lastJDay <- min(max(jday),lastJDay)
  }

  YLIM <- c(min(c(maxt,mint,minTCold,maxTHot),na.rm=T),max(c(maxt,mint,minTCold,maxTHot),na.rm=T))
  if(Y2DATE == 1){
    dataset <- cbind(maxTHot,minTCold)
    print(dim(dataset))
    print(c(sJDay,lastJDay))
    YLIM <- range(c(maxt,mint,as.vector(dataset[sJDay:lastJDay,])),na.rm=T)
  }

  print(YLIM)
  plot(c(1,lastJDay),c(0,1),xlab='Date',type='n',ylab='Temperature (ºC)',axes=F,ylim=YLIM,
       cex.lab=CEX.LAB,main=stnName,cex.main=CEX.MAIN)


  lines(jday,maxt,lwd=3)

  lines(jday,mint,lwd=3)

  ats<-pretty(1:lastJDay,20)
  labs<-format(as.Date(paste(Year,ats,sep='-'),'%Y-%j'),'%d %b')
  axis(1,at=ats,labs,las=1,cex.axis=CEX.AXIS,lwd=LWD.AXIS,cex=CEX)


  lines(1:lastJDay,maxTHot[1:lastJDay],col='red',lwd=3,lty=4)
  lines(1:lastJDay,minTHot[1:lastJDay],col='red',lwd=3,lty=4)
  lines(1:lastJDay,maxTCold[1:lastJDay],col='blue',lwd=3,lty=3)
  lines(1:lastJDay,minTCold[1:lastJDay],col='blue',lwd=3,lty=3)

  lines(1:lastJDay,maxTAve[1:lastJDay],col='green',lwd=3,lty=2)
  lines(1:lastJDay,minTAve[1:lastJDay],col='green',lwd=3,lty=2)

  legend('top',legend=c(Year,'Average (1981 - 2010)','Warmest 10%','Coolest 10%'),lwd=3,lty=c(1,2,4,3),col=c('black','green','red','blue'),bty='n',ncol=4,cex=CEX)
}

getFName <- function(LOCATION,YEAR,CTYPE,TABNAME){
  if(TABNAME == 'Chill'){
    cTypes <- c('CP','Hours')
    dataType <- cTypes[as.numeric(CTYPE)]
  } else {
    dataType <- TABNAME
  }
  stnNums<-c(stnQld,stnVic,stnWA,stnTas,stnNSW,stnSA)
  years <- seq(2012,2016)
  paste(stnNums[as.numeric(LOCATION)],YEAR,dataType,sep='_')
}

makePDF <- function(YEAR,CHILLTYPE,LOCATION,Y2DATE,HQ){
  pdf(file='myGenerated.pdf',width=12,height=8)
  doThePlot(YEAR,CHILLTYPE,LOCATION,Y2DATE,HQ)
  dev.off()
}

#makeJPEG(input$yearInput,input$cType,input$Location,input$Y2Date,input$dateInput,input$height,input$tabs,2)
makeJPEG <- function(YEAR,CHILLTYPE,LOCATION,Y2DATE,DATEINPUT,HEIGHT,TABS,HQ){
  WIDTH = HEIGHT * 1200 / 800
  jpeg(file='myGenerated.jpg',width=WIDTH,height=HEIGHT,quality=100)
  if(TABS == 'GDH') {
    doTheHeatPlot(YEAR,DATEINPUT,LOCATION,Y2DATE,HQ)
  } else {
    doThePlot(YEAR,CHILLTYPE,LOCATION,Y2DATE,HQ)
  }

  dev.off()
}

