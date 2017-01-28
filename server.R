#server.R

library(chillR)
library(shinysky)
library(leaflet)

source('helper.R')

startTown <- which(siteInfo$Name == 'Applethorpe')

shinyServer(function(input, output) {

  site <- reactiveValues(currentlLoc=startTown,name='Applethorpe')

  currentYear <- as.numeric(format(Sys.Date(), "%Y"))

  #selectedYear <- reactiveValues(Year=as.numeric(format(Sys.Date(), "%Y")))

  results <- reactive({
    searchForLocation(input$Location)
  })

  towns <- reactive({
    cat('towns()',input$Town,'\n')
    searchForPlace(input$Town)
  })


  currentFN <- function() { getFName(site$currentLoc,input$yearInput,input$cType,input$gType,input$tabs) }

  output$yearOutput <- renderUI({
    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      selectInput('yearInput',h4('Select Year for Plots'),as.character(seq(currentYear,1968,-1)), format(Sys.Date(), "%Y"))
    }
  })

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


  output$dateStart <- renderUI({
    if (is.null(input$yearInput)) {
      yearInput <- 2017
    } else {
      yearInput <- input$yearInput
    }

    if(input$tabs == 'Chill'){
      print(input$tabs)
      if(input$cType == 3){
        value1 <- checkDate(paste(yearInput,"-05-01",sep=''))
      } else {
        value1 = checkDate(paste(yearInput,"-03-01",sep=''))
      }
    }

    if(input$tabs == 'Growing Degrees' ){
      value1 = checkDate(paste(yearInput,"-05-01",sep=''))
    }

    if(input$tabs == 'Temperature'){
      value1 = checkDate(paste(yearInput,"-01-01",sep=''))
    }

    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      dateInput("startDate", label = h4("Start Date"), value = value1, min = paste(yearInput,"-01-01",sep=''), max =  Sys.Date() - 1)
    }
  })

  output$dateEnd <- renderUI({

    if(input$tabs == 'Temperature'){
      if (is.null(input$yearInput)) {
        return(NULL) #dropbox not ready
      }
      dateInput("endDate", label = h4("End Date"), value = checkDateEnd(as.character(Sys.Date()-1)),  min = paste(input$yearInput,"-01-01",sep=''), max =  checkDateEnd(as.character(Sys.Date()-1)))

    }
  })


  output$downloadJPEG <- renderUI({
    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      downloadButton("outputJPEG", "Download JPEG")
    }
  })

  output$sliderForHeight <-renderUI({
    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      sliderInput("JPEGHeight", "Plot Height", min = 400, max = 1200,step = 100, value = 600)
    }
  })


  # output$dateToStartChill <- renderUI({
  #   if (is.null(input$yearInput)) {
  #     return(NULL) #selector not ready
  #   }
  #   if(input$cType == 3){
  #     dateInput("startJDay", label = h4("Start Date"), value = checkDate(paste(input$yearInput,"-05-01",sep='')), min = paste(input$yearInput,"-01-01",sep=''), max =  Sys.Date() - 1)
  #   } else {
  #     dateInput("startJDay", label = h4("Start Date"), value = checkDate(paste(input$yearInput,"-03-01",sep='')), min = paste(input$yearInput,"-01-01",sep=''), max =  Sys.Date() - 1)
  #   }
  # })
  #
  # output$dateForGDHOutput <- renderUI({
  #   if (is.null(input$yearInput)) {
  #     return(NULL) #dropbox not ready
  #   }
  #   dateInput("dateInputGDH", label = h4("Start Date"), value = checkDate(paste(input$yearInput,"-05-01",sep='')),  min = paste(input$yearInput,"-01-01",sep=''), max =  Sys.Date() - 1)
  # })
  #
  # output$dateForTempStart <- renderUI({
  #   if (is.null(input$yearInput)) {
  #     return(NULL) #dropbox not ready
  #   }
  #   dateInput("dateInputStart", label = h4("Start Date"), value = checkDate(paste(input$yearInput,"-01-01",sep='')),  min = paste(input$yearInput,"-01-01",sep=''), max =  Sys.Date() - 1)
  # })

  # output$dateForTempEnd <- renderUI({
  #   if (is.null(input$yearInput)) {
  #     return(NULL) #dropbox not ready
  #   }
  #   dateInput("dateInputEnd", label = h4("End Date"), value = checkDateEnd(as.character(Sys.Date()-1)),  min = paste(input$yearInput,"-01-01",sep=''), max =  checkDateEnd(as.character(Sys.Date()-1)))
  # })


  loadTheData <- function() {
    ##cat('Start loadTheData\n')
    if(is.null(site$currentLoc)){
      site$currentLoc <- startTown
    }
    stn<-siteInfo$stnID[site$currentLoc]
    rdata <- file.path('Data',paste(stn,'.RData',sep=''))
    load(rdata)
  }


  ### Chill Plot ###
  observe({
    output$chillPlot <- renderPlot({
      if (is.null(input$yearInput) | is.null(input$startDate)) {
        return(NULL) #slider not ready
      }
      loadTheData()
      startJDay <- as.numeric(format(input$startDate,'%j'))
      doThePlot(input$yearInput,input$cType,site$currentLoc,input$Y2DateChill,input$startDate,3)
    },height=input$JPEGHeight) #renderPlot
  })#observe

  ### GDH Plot ###
  observe({
    output$GDHPlot <- renderPlot({
      if (is.null(input$yearInput) | is.null(input$startDate)) {
        return(NULL) #sliders not ready
      }
      loadTheData()
      doTheHeatPlot(input$yearInput,input$gType,input$startDate,site$currentLoc,input$Y2DateGDH,3)
    },height=input$JPEGHeight) #renderPlot
  })

  ### Temperature Plot ###
  observe({
    output$TempPlot <- renderPlot({
      if (is.null(input$yearInput) | is.null(input$startDate) | is.null(input$endDate)) {
        return(NULL) #sliders not ready
      }
      #print(input$tabs)
      loadTheData()
      doTheTempPlot(input$yearInput,input$startDate,input$endDate,site$currentLoc,1,3) #input$Y2DateTemp,3)
    },height=input$JPEGHeight) #renderPlot
  })

  ### Leaflet Map ###########


  output$map <- renderLeaflet({
    # if (is.null(input$yearInput)) {
    #   return(NULL) #slider not ready
    # }
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
      #NSW
      mtch <- match('Kilmore Gap',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
      zoom <- 8
    }
    if(input$Region == "4"){
      #NSW
      mtch <- match('Hobart Airport',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
      zoom <- 9
    }
    if(input$Region == "5"){
      #NSW
      mtch <- match('Mount Barker',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
      zoom <- 10
    }
    if(input$Region == "6"){
      #NSW
      mtch <- match('Manjimup',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
    }

    searchResults <- results()
    if(!is.null(searchResults)){
      if(length(searchResults) == 2) {
        #just the count of matches
        textIs <- HTML(paste('There <b>',searchResults$N,'</b>possible Locations'))
        if(searchResults$N <= 10){
          first <- T
          textIs <- HTML(paste('There <b>',searchResults$N,'</b>possible Locations<br/>'))
          for(i in searchResults$these){
            if(first){
              first <- F
              textIs <- HTML(paste(textIs,siteInfo$Name[i],sep='<br/>'))
            } else {
              textIs <-HTML(paste(textIs,siteInfo$Name[i],sep='<br/>'))
            }
            #print(textIs)
          }
        }
        output$NMatches <- renderUI({
          textIs
        })
      }
      if(length(searchResults) == 3) {
        output$NMatches <- renderUI({
          HTML(paste('Found',searchResults$searchedSite,'<br/>Click Marker to Select it'))
        })
        rlng <- searchResults$searchedLng
        rlat <- searchResults$searchedLat
        zoom <- 12
      }
    }

    searchTowns <- towns()
    cat('searchTowns',length(searchTowns),'\n')
    if(!is.null(searchTowns)){
      if(length(searchTowns) == 2) {
        #just the count of matches
        textIs2 <- HTML(paste('There <b>',searchTowns$N,'</b>possible Towns'))
        print(searchTowns$N)
        if(searchTowns$N <= 10){
          first <- T
          textIs2 <- HTML(paste('There <b>',searchTowns$N,'</b>possible Towns<br/>'))
          for(i in searchTowns$these){
            if(first){
              first <- F
              textIs2 <- HTML(paste(textIs2,gaz$PlaceStatePostCode[i],sep='<br/>'))
            } else {
              textIs2 <-HTML(paste(textIs2,gaz$PlaceStatePostCode[i],sep='<br/>'))
            }
          }
        }
        output$NTowns <- renderUI({
          textIs2
        })
      }
      if(length(searchTowns) == 3) {
        output$NTowns <- renderUI({
          HTML(paste('Found',searchTowns$searchedSite,'<br/>Select nearby location to Select it'))
        })
        rlng <- searchTowns$searchedLng
        rlat <- searchTowns$searchedLat
        zoom <- 12
      }
    }
    #cat(rlng,rlat,'\n')
    leaflet(data = siteInfo) %>%
      addTiles() %>%
      addMarkers(~longitude,~latitude, layerId = ~stnID, popup = ~NamePerc) %>%
      setView(lng = rlng, lat = rlat, zoom = zoom)
  })

  observe({
    click<-input$map_marker_click
    if(is.null(click)){
      return()
    }
    text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
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
    #print(text2)
    site$currentLoc <- rowNumber
    # output$Click_text<-renderText({
    #   text2
    # })
  })

  # output$outputPDF <- downloadHandler(
  #   filename = function() {
  #     paste(currentFN(),'pdf',sep='.')
  #   },
  #   content=function(file) {
  #     makePDF(input$yearInput,input$cType,site$currentLoc,input$Y2Date,2)
  #     file.copy(from = "myGenerated.pdf", to = file)
  #   }
  # )

  output$outputJPEG <- downloadHandler(
    filename = function() {
      print(paste(currentFN(),'jpg',sep='.'))
      paste(currentFN(),'jpg',sep='.')
      },
    content=function(file) {
      makeJPEG(input$yearInput,input$cType,input$gType,input$Location,input$Y2DateChill,input$startDate,input$endDate,input$JPEGHeight,input$tabs,2)
      file.copy(from = "myGenerated.jpg", to = file)
    }
  )


})

