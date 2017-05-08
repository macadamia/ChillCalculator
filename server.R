#server.R

library(chillR)
library(shinysky)
library(leaflet)
library(plotly)

source('helper.R')


startStn <- which(siteInfo$Name == 'Applethorpe')
startTown <- which(gaz$PlaceName == 'Applethorpe')



shinyServer(function(input, output,session) {

  stns <- reactiveValues()
  stns$df <- data.frame(row=numeric(0),stn=character(0))

  site <- reactiveValues(currentlLoc=startStn,name='Applethorpe')

  currentYear <- as.numeric(format(Sys.Date(), "%Y"))

  selectedYear <- reactiveValues(Year=as.numeric(format(Sys.Date(), "%Y")))

  results <- reactive({
    searchForLocation(input$Location)
  })

  towns <- reactive({
    searchForPlace(input$Town)
  })




  currentFN <- function() { getFName(site$currentLoc,input$yearInput,input$cType,input$gType,input$tabs) }

  output$SelectedLocation <- renderUI({
    if(is.null(site$currentLoc)){
      site$currentLoc <- startStn
    }
    ## replace the UI with a table or something using stns$df
    if(is.na(siteInfo$Name[site$currentLoc])){
      HTML(paste("<br/>Select a Location"))
    } else {
     HTML(paste("<br/><h3>",siteInfo$Name[site$currentLoc],"</h3>"))
    }
  })

  output$yearOutput <- renderUI({
    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      selectInput('yearInput',h4('Year'),as.character(seq(currentYear,1968,-1)), selectedYear$Year)
    }
  })

  checkDate <- function(aDate){
    if(as.Date(aDate) < Sys.Date() ){
      return(aDate)
    } else {
      return(paste(strsplit(aDate,'-')[[1]][1],'-01-01',sep=''))
    }
  }

  checkDateEnd <- function(aDate){  # this function is not quite right
    aYear <- as.numeric(input$yearInput)
    thisYear <- as.numeric(format(Sys.Date(),'%Y'))
    if(aYear == thisYear){
      return(Sys.Date()-1)
    } else {
      return(paste(aYear,'-12-31',sep=''))
    }
  }


  output$dateStart <- renderUI({
    if (is.null(input$yearInput)) {
      return(NULL)
    }
    selectedYear$Year <- input$yearInput

    if(input$tabs == 'Chill'){
      if(input$cType == 3){
        value1 <- checkDate(paste(selectedYear$Year,"-05-01",sep=''))
      } else {
        value1 = checkDate(paste(selectedYear$Year,"-03-01",sep=''))
      }
    }

    if(input$tabs == 'Growing Degrees' ){
      value1 = checkDate(paste(selectedYear$Year,"-05-01",sep=''))
    }

    if(input$tabs == 'Temperature'){
      value1 = checkDate(paste(selectedYear$Year,"-01-01",sep=''))
    }

    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      dateInput("startDate", label = h4("Start Date"), value = value1, min = paste(selectedYear$Year,"-01-01",sep=''), max =  Sys.Date() - 1)
    }
  })

  output$dateEnd <- renderUI({

    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      if (is.null(input$yearInput)) {
        return(NULL) #dropbox not ready
      }
      dateInput("endDate", label = h4("End Date"), value = checkDateEnd(as.character(Sys.Date()-1)),  min = paste(selectedYear$Year,"-01-01",sep=''), max =  checkDateEnd(as.character(Sys.Date()-1)))
    }
  })

  output$baseTemp <- renderUI({
    if(input$tabs == 'Growing Degrees'){
      textInput('baseTemp',h4("Base Temperature (ºC)"),"10",'150px')
    }
  })



  output$sliderForHeight <-renderUI({
    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      sliderInput("JPEGHeight", "Plot Height", min = 400, max = 1200,step = 100, value = 600)
    }
  })

  loadTheData <- function() {
    if(is.null(site$currentLoc)){
      site$currentLoc <- startStn
    }
    stn<-siteInfo$stnID[site$currentLoc]
    rdata <- file.path('Data',paste(stn,'.RData',sep=''))
    print(rdata)
    load(rdata)
  }


  ### Chill Plot ###
  observe({
    output$chillPlot <- renderPlotly({
      if (is.null(input$yearInput) | is.null(input$startDate) | is.null(site$currentLoc)) {
        return(NULL) #slider not ready
      }
      loadTheData() # stns$df[1,1] as the location
      startJDay <- as.numeric(format(input$startDate,'%j'))
      doThePlot(selectedYear$Year,input$cType,site$currentLoc,input$Y2DateChill,input$startDate,input$endDate)
    }) #renderPlot
  })#observe

  #need a second chill plot
  observe({
    output$chillPlot2 <- renderPlotly({
      if (is.null(input$yearInput) | is.null(input$startDate | is.null(site$currentLoc))) {
        return(NULL) #slider not ready
      }
      loadTheData() # stns$df[1,1] as the location
      startJDay <- as.numeric(format(input$startDate,'%j'))
      doThePlot(selectedYear$Year,input$cType,site$currentLoc,input$Y2DateChill,input$startDate,input$endDate)
    }) #renderPlot
  })#observe

  ### GDH Plot ###
  observe({
    output$GDHPlot <- renderPlotly({
      if (is.null(input$yearInput) | is.null(input$startDate) | is.null(site$currentLoc)) {
        return(NULL) #sliders not ready
      }
      if( is.null(input$gType) | is.null(input$startDate) | is.null(input$endDate) | is.null(site$currentLoc) | is.null(input$Y2DateGDH) | is.null(input$baseTemp )){
        return(NULL)
      }
      loadTheData()
      doTheHeatPlot(selectedYear$Year,input$gType,input$startDate,input$endDate,site$currentLoc,input$Y2DateGDH,input$baseTemp)

    }) #renderPlot
  })


  output$TempPlot <- renderPlotly({
    if (is.null(selectedYear$Year) | is.null(input$startDate) | is.null(input$endDate) | is.null(site$currentLoc)) {
      return(NULL) #sliders not ready
    }
    loadTheData()
    doTheTempPlot(selectedYear$Year,input$startDate,input$endDate,site$currentLoc,1)
  })



  ### Leaflet Map ###########


  output$map <- renderLeaflet({
    zoom <- 9
    if(input$Region == "1"){
      #granite belt
      mtch <- match('Applethorpe',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
    }
    if(input$Region == "2"){
      #NSW
      mtch <- match('Orange Airport Aws',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
    }
    if(input$Region == "3"){
      #Victoria
      mtch <- match('Kilmore Gap',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
      zoom <- 8
    }
    if(input$Region == "4"){
      #TAS
      mtch <- match('Hobart Airport',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
      zoom <- 9
    }
    if(input$Region == "5"){
      #SA
      mtch <- 208
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
      zoom <- 10
    }
    if(input$Region == "6"){
      #WA
      mtch <- match('Manjimup',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
    }

    searchResults <- results()

    if(!is.null(searchResults)){

        x <- siteInfo$Name[searchResults$these]
        bunchOfInts <- seq(1,length(x))

        stnList <- list()
        for(i in seq(1,length(searchResults$these))){
          stnList[x[i]] = searchResults$these[i]
        }
        if(!is.null(stnList)){
          output$BuildStnLocations <- renderUI({
            selectInput("stnFound", label = h4("Select Station"),choices = stnList, size = 10, selectize = F,selected = startStn)
          })
        }
    }

    searchTowns <- towns()
    if(!is.null(searchTowns) & length(searchTowns$these) < 200){
      x <- gaz$PlaceStatePostCode[searchTowns$these]
      bunchOfInts <- seq(1,length(x))

      theTownList <- list()
      for(i in seq(1,length(searchTowns$these))){
        theTownList[x[i]] = searchTowns$these[i]
      }
      if(!is.null(theTownList)){
        output$NTowns <- renderUI({
          selectInput("townFound", label = h4("Select Town"),choices = theTownList, size = 10, selectize = F,selected=startTown)
        })
      }
    }
    leaflet(data = siteInfo) %>%
      addTiles() %>%
      addMarkers(~longitude,~latitude, layerId = ~stnID, popup = ~NamePerc) %>%
      setView(lng = rlng, lat = rlat, zoom = zoom)
  })

  observeEvent ( input$stnFound, {
    this <- as.numeric(input$stnFound)
    site$currentLoc <- this
    rlat <- siteInfo$latitude[this]
    rlng <- siteInfo$longitude[this]
    proxy <- leafletProxy("map")
    proxy %>% setView(lng=rlng, lat=rlat, zoom = 9)

  })

  observeEvent (input$townFound, {
    if (is.null(input$townFound)) {
      return(NULL)
    }
    this <- as.numeric(input$townFound)

    rlat <- gaz$Latitude[this]
    rlng <- gaz$Longitude[this]
    proxy <- leafletProxy("map")
    proxy %>% setView(lng=rlng, lat=rlat, zoom = 9)

  })
  observeEvent(input$map_marker_click,{
    click<-input$map_marker_click
    if(is.null(click)){
      return()
    }
    text<-paste("Latitude ", click$lat, "Longtitude ", click$lng)
    rowNumber <- which(siteInfo$stnID == click$id)
    text2<-paste("You've selected point", click$id,'at row #',rowNumber)
    perc <- (siteInfo$PercMaxTObs[rowNumber]+siteInfo$PercMinTObs[rowNumber])/2*100
    warning <- ''
    if(perc < 0.5) {
      warning <- '<b>Warning:</b>'
      output$StationInfo <- renderUI({
        HTML(paste(warning,siteInfo$Name[rowNumber],'recorded temperature',formatC(perc,format='f',digits=1),'% of the time<br/>',sep=' '))
      })
    }
    site$currentLoc <- rowNumber
    if(nrow(stns$df) == 0){
      stns$df <- data.frame(row=rowNumber, stn=siteInfo$Name[rowNumber])
    }
    else if(nrow(stns$df) == 1){
      #add to 2nd row
      tmp <- data.frame(row=rowNumber, stn=siteInfo$Name[rowNumber])
      stns$df <- rbind(stns$df, tmp)
    }
    else {
      #move 2nd row to first row and add to 2nd row
      if(stns$df$row[2] == rowNumber){
        #already have it
        stns$df <- stns$df[2,]
      } else { # append
        tmp <- data.frame(row=rowNumber, stn=siteInfo$Name[rowNumber])
        stns$df <- stns$df[2,]
        stns$df <- rbind(stns$df, tmp)
      }
    }
    print(stns$df)
  })



})

# aList <- list()
# for(i in 1:nrow(siteInfo)){
#   aList[siteInfo$Name[i]] = i
# }
