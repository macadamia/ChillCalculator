
inFormat <- "%d/%m/%Y %H:%M"

#localData <- read.csv('~/Downloads/ARS1_2014_HOURLY.csv',header=T,stringsAsFactors = F)

localData <- read.csv('/Users/uqnwhite/Dropbox/ApplePear/Tas/2014/Met/PLENTY_2014_HOURLY.csv',header=T,stringsAsFactors = F)

if(any(is.na(localData))){
  #assume it's dangling rows
  NAs <- which(is.na(localData[,2]))
  if(all(diff(NAs) == 1)){
    #they are sequential
    localData <- localData[-NAs,]
    cat(paste('Removed',length(NAs),'of dangling rows'),'\n')
  } else {
    cat(('I suspect you have missing data\n'))
    localData <- localData[-NAs,]
  }
}

growerData <- data.frame(dateTime=as.datetime(localData[,1],inFormat),theTemp=localData[,2])


testFmt <- '%Y'

testValue <- as.numeric(format(growerData[1,1],testFmt))
cat(as.character(growerData[1,1]),'Using testFmt', testFmt,'Result is',testValue,'\n')
if(is.na(testValue)){
  cat('showing notification\n')
} else {
  if( testValue < 1000){
    cat('showing notification\n')

  } else {
    cat('removing notification\n')
  }
}

hoursInData <- as.numeric(format(growerData[,1],'%H'))
maxHourInData <- max(hoursInData)
if(tail(hoursInData,1) != maxHourInData){
  #need to search back for last maxHourInData
  lastMaxHour <- max(which(hoursInData == maxHourInData))
  dropThese <- seq((lastMaxHour+1),length(hoursInData))
  growerData <- growerData[-dropThese,]
}

print(tail(growerData$dateTime,1))