#server.R

library(chillR)
library(rdrop2)
library(shinysky)
library(leaflet)

source('helper.R')

shinyServer(function(input, output) {

  site <- reactiveValues(currentlLoc=318,name='Applethorpe')

  currentYear <- as.numeric(format(Sys.Date(), "%Y"))

  #selectedYear <- reactiveValues(Year=as.numeric(format(Sys.Date(), "%Y")))

  results <- reactive({
    searchForLocation(input$Location)
  })


  currentFNChill <- function() { getFName(site$currentLoc,input$yearInputChill,input$cType,input$tabs) }
  currentFNGDH <- function() { getFName(site$currentLoc,input$yearInputGDH,input$cType,input$tabs) }
  currentFNTemp <- function() { getFName(site$currentLoc,input$yearInputTemp,input$cType,input$tabs) }

  output$yearOutput <- renderUI({
    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      selectInput('yearInput',h4('Select Year for Plots'),as.character(seq(currentYear,1968,-1)), format(Sys.Date(), "%Y"))
    }
  })

  # output$yearOutputChill <- renderUI({
  #   selectInput('yearInputChill',h4('Select Year'),as.character(seq(currentYear,1968,-1)), format(Sys.Date(), "%Y"))
  # })
  #
  # output$yearOutputGDH <- renderUI({
  #   selectInput('yearInputGDH',h4('Select Year'),as.character(seq(currentYear,1968,-1)), format(Sys.Date(), "%Y"))
  # })
  #
  # output$yearOutputTemp <- renderUI({
  #   selectInput('yearInputTemp',h4('Select Year'),as.character(seq(currentYear,1968,-1)), format(Sys.Date(), "%Y"))
  # })

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

  output$dateToStartChill <- renderUI({
    if (is.null(input$yearInput)) {
      return(NULL) #selector not ready
    }
    if(input$cType == 3){
      dateInput("startJDay", label = h4("Start Date"), value = checkDate(paste(input$yearInput,"-05-01",sep='')), min = paste(input$yearInput,"-01-01",sep=''), max =  Sys.Date() - 1)
    } else {
      dateInput("startJDay", label = h4("Start Date"), value = checkDate(paste(input$yearInput,"-03-01",sep='')), min = paste(input$yearInput,"-01-01",sep=''), max =  Sys.Date() - 1)
    }
  })

  output$dateForGDHOutput <- renderUI({
    if (is.null(input$yearInput)) {
      return(NULL) #dropbox not ready
    }
    dateInput("dateInputGDH", label = h4("Start Date"), value = checkDate(paste(input$yearInput,"-05-01",sep='')),  min = paste(input$yearInput,"-01-01",sep=''), max =  Sys.Date() - 1)
  })

  output$dateForTempStart <- renderUI({
    if (is.null(input$yearInput)) {
      return(NULL) #dropbox not ready
    }
    dateInput("dateInputStart", label = h4("Start Date"), value = checkDate(paste(input$yearInput,"-01-01",sep='')),  min = paste(input$yearInput,"-01-01",sep=''), max =  Sys.Date() - 1)
  })

  output$dateForTempEnd <- renderUI({
    if (is.null(input$yearInput)) {
      return(NULL) #dropbox not ready
    }
    dateInput("dateInputEnd", label = h4("End Date"), value = checkDateEnd(as.character(Sys.Date()-1)),  min = paste(input$yearInput,"-01-01",sep=''), max =  checkDateEnd(as.character(Sys.Date()-1)))
  })


  loadTheData <- function() {
    ##cat('Start loadTheData\n')
    if(is.null(site$currentLoc)){
      site$currentLoc <- 318
    }
    stn<-siteInfo$stnID[site$currentLoc]
    rdata <- file.path('Data',paste(stn,'.RData',sep=''))
    load(rdata)
  }
  #   lat<-siteInfo$latitude[site$currentLoc]
  #   if(input$tabs == 'Chill'){
  #     sJDay<- as.numeric(format(input$startJDay,'%j'))
  #     #print(sJDay)
  #     if(input$cType == 1){
  #       eJDay<-365
  #     }
  #     if(input$cType == 2){
  #       eJDay<-365
  #     }
  #     if(input$cType == 3){
  #       eJDay<- 365 #as.numeric(format(as.Date(paste('31-08',YEAR,sep='-'),'%d-%m-%Y'),'%j'))
  #     }
  #   }
  #   if(input$tabs == 'Growing Degrees'){
  #     sJDay<- as.numeric(format(input$dateInputGDH,'%j'))
  #     eJDay<- 365
  #   }
  #   if(input$tabs == 'Temperature'){
  #     #print('hello from temperature tab')
  #     sJDay<- as.numeric(format(input$dateInputStart,'%j'))
  #     eJDay<- as.numeric(format(input$dateInputEnd,'%j'))
  #   }
  #
  #
  #
  #   #cat('try and retrieve previously created data\n')
  #   rdata <- file.path('Data',paste(stn,'.RData',sep=''))
  #   withProgress(message = 'Getting Data',value = 0, {
  #     load(rdata)
  #     incProgress(.9,detail='Create Plot')
  #     incProgress(1)
  #   }) #progress
  # }

  ### Chill Plot ###
  observe({
    output$chillPlot <- renderPlot({
      if (is.null(input$yearInput) | is.null(input$startJDay)) {
        return(NULL) #slider not ready
      }
      loadTheData()
      startJDay <- as.numeric(format(input$startJDay,'%j'))
      doThePlot(input$yearInput,input$cType,site$currentLoc,input$Y2DateChill,startJDay,3)
    },height=input$heightChill) #renderPlot
  })#observe

  ### GDH Plot ###
  observe({
    output$GDHPlot <- renderPlot({
      if (is.null(input$yearInput) | is.null(input$dateInputGDH)) {
        return(NULL) #sliders not ready
      }
      loadTheData()
      doTheHeatPlot(input$yearInput,input$gType,input$dateInputGDH,site$currentLoc,input$Y2DateGDH,3)
    },height=input$heightGDH) #renderPlot
  })

  ### Temperature Plot ###
  observe({
    output$TempPlot <- renderPlot({
      if (is.null(input$yearInput) | is.null(input$dateInputStart) | is.null(input$dateInputEnd)) {
        return(NULL) #sliders not ready
      }
      #print(input$tabs)
      loadTheData()
      doTheTempPlot(input$yearInput,input$dateInputStart,input$dateInputEnd,site$currentLoc,1,3) #input$Y2DateTemp,3)
    },height=input$heightTemp) #renderPlot
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
      mtch <- match('Shepparton Airport',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
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
    #cat(rlng,rlat,'\n')
    leaflet(data = siteInfo) %>%
      addTiles() %>%
      addMarkers(~longitude,~latitude, layerId = ~stnID, popup = ~as.character(Name)) %>%
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

  output$outputJPEGChill <- downloadHandler(
    filename = function() {
      paste(currentFNChill(),'jpg',sep='.')
      },
    content=function(file) {
      makeJPEG(input$yearInputChill,input$cType,input$Location,input$Y2DateChill,input$dateInput,input$heightChill,input$tabs,2)
      file.copy(from = "myGenerated.jpg", to = file)
    }
  )

  output$outputJPEGGDH <- downloadHandler(
    filename = function() {
      paste(currentFNGDH(),'jpg',sep='.')
    },
    content=function(file) {
      makeJPEG(input$yearInputGDH,input$cType,input$Location,input$Y2DateGDH,input$dateInput,input$heightGDH,input$tabs,2)
      file.copy(from = "myGenerated.jpg", to = file)
    }
  )

  output$outputJPEGTemp <- downloadHandler(
    filename = function() {
      paste(currentFNTemp(),'jpg',sep='.')
    },
    content=function(file) {
      makeJPEG(input$yearInputTemp,input$cType,input$Location,input$Y2DateGDH,input$heightTemp,input$tabs,2)
      file.copy(from = "myGenerated.jpg", to = file)
    }
  )

})

