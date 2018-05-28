
debug <- T

if(debug)
  print('hello from global')

library(shiny)
library(chillR)
library(plotly)
library(shinysky)
library(V8)
library(leaflet)
library(shinyjs)

# arm online #4E7707
#franchise blue #007EB1
Sys.setenv(TZ="Australia/Brisbane")

userLocationSet <- F


siteInfo <- readRDS('Data/SiteInfo.rds')
gaz <- readRDS('Data/Gazetteer2010.rds')
THEURL <- readRDS('Data/extraInfo.rds')
TheAPIKey <- readRDS('Data/WillyWeather.rds')

#WillyWeatherIDs <- readRDS('Data/WillyWeatherInfo.rds')

#Dropbox
token <- readRDS('Data/droptokenchillcalc.rds')
#drop_acc(dtoken=token)

useAPSIM <- T

if(!useAPSIM){
  longPaddock <- readRDS('Data/LongPaddock.rds')
  library(RCurl)
}

startStnRowID <- which(siteInfo$Name == 'Applethorpe')
startTown <- which(gaz$PlaceName == 'Applethorpe')

source('helper.R')
