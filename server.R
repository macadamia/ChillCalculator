#server.R

library(chillR)
library(shinysky)
library(leaflet)
library(plotly)

startTown <- which(siteInfo$Name == 'Applethorpe')

shinyServer(function(input, output,session) {

  site <- reactiveValues(currentlLoc=startTown,name='Applethorpe')

  currentYear <- as.numeric(format(Sys.Date(), "%Y"))

  selectedYear <- reactiveValues(Year=as.numeric(format(Sys.Date(), "%Y")))

  results <- reactive({
    searchForLocation(input$Location)
  })

  towns <- reactive({
    searchForPlace(input$Town)
  })


  currentFN <- function() { getFName(site$currentLoc,input$yearInput,input$cType,input$gType,input$tabs) }

  callModule(fluidRowServer,"chillControls",selectedYear,currentYear,site,siteInfo)
  callModule(fluidRowServer,"gdhControls",selectedYear,currentYear,site,siteInfo)
  callModule(fluidRowServer,"tempControls",selectedYear,currentYear,site,siteInfo)

  # output$SelectedLocation <- renderUI({
  #   if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
  #     HTML(paste("<br/><b>",siteInfo$Name[site$currentLoc],"</b>"))
  #   }
  # })
  #
  # output$yearOutput <- renderUI({
  #   if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
  #     selectInput('yearInput',h4('Year'),as.character(seq(currentYear,1968,-1)), selectedYear$Year)
  #   }
  # })
  # output$dateStart <- renderUI({
  #   if (is.null(input$yearInput)) {
  #     return(NULL)
  #   }
  #   selectedYear$Year <- input$yearInput
  #
  #   if(input$tabs == 'Chill'){
  #     if(input$cType == 3){
  #       value1 <- checkDate(paste(selectedYear$Year,"-05-01",sep=''))
  #     } else {
  #       value1 = checkDate(paste(selectedYear$Year,"-03-01",sep=''))
  #     }
  #   }
  #
  #   if(input$tabs == 'Growing Degrees' ){
  #     value1 = checkDate(paste(selectedYear$Year,"-05-01",sep=''))
  #   }
  #
  #   if(input$tabs == 'Temperature'){
  #     value1 = checkDate(paste(selectedYear$Year,"-01-01",sep=''))
  #   }
  #
  #   if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
  #     dateInput("startDate", label = h4("Start Date"), value = value1, min = paste(selectedYear$Year,"-01-01",sep=''), max =  Sys.Date() - 1)
  #   }
  # })
  #
  #
  # output$dateEnd <- renderUI({
  #
  #   if( input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
  #     if (is.null(input$yearInput)) {
  #       return(NULL) #dropbox not ready
  #     }
  #     dateInput("endDate", label = h4("End Date"), value = checkDateEnd(as.character(Sys.Date()-1)),  min = paste(selectedYear$Year,"-01-01",sep=''), max =  checkDateEnd(as.character(Sys.Date()-1)))
  #
  #   }
  # })


  # output$downloadJPEG <- renderUI({
  #   if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
  #     downloadButton("outputJPEG", "Download JPEG")
  #   }
  # })

  # output$sliderForHeight <-renderUI({
  #   if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
  #     sliderInput("JPEGHeight", "Plot Height", min = 400, max = 1200,step = 100, value = 600)
  #   }
  # })

  loadTheData <- function() {
    if(is.null(site$currentLoc)){
      site$currentLoc <- startTown
    }
    stn<-siteInfo$stnID[site$currentLoc]
    rdata <- file.path('Data',paste(stn,'.RData',sep=''))
    load(rdata)
  }


  ### Chill Plot ###
  observe({
    output$chillPlot <- renderPlotly({
       if (is.null(input$startDate) | is.null(input$startDate)) {
         return(NULL)
       }
      loadTheData()
      startJDay <- as.numeric(format(input$startDate,'%j'))
      cat(selectedYear$Year,input$cType,site$currentLoc,input$Y2DateChill,'startDate',input$startDate,'enddate',input$endDate,':\n')
      doThePlot(selectedYear$Year,input$cType,site$currentLoc,input$Y2DateChill,input$startDate,input$endDate)
    }) #renderPlotly
  })#observe

  ### GDH Plot ###
  observe({
    output$GDHPlot <- renderPlotly({
      if (is.null(input$yearInput) | is.null(input$startDate)) {
        return(NULL) #sliders not ready
      }
      loadTheData()
      doTheHeatPlot(selectedYear$Year,input$gType,input$startDate,input$endDate,site$currentLoc,input$Y2DateGDH)
    }) #renderPlotly
  })

  ### Temperature Plot ###

  output$TempPlot <- renderPlotly({
    if (is.null(selectedYear$Year) | is.null(input$startDate) | is.null(input$endDate)) {
      return(NULL) #sliders not ready
    }
    loadTheData()
    doTheTempPlot(selectedYear$Year,input$startDate,input$endDate,site$currentLoc,1) #input$Y2DateTemp,3)
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
    if(!is.null(searchTowns)){
      if(length(searchTowns) == 2) {
        #just the count of matches
        textIs2 <- HTML(paste('There <b>',searchTowns$N,'</b>possible Towns'))
        if(searchTowns$N <= 25){
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
    site$currentLoc <- rowNumber
    text2<-paste("You've selected point", click$id,'at row #',rowNumber)
    perc <- (siteInfo$PercMaxTObs[rowNumber]+siteInfo$PercMinTObs[rowNumber])/2*100
    warning <- ''
    if(perc < 0.5) {
      warning <- '<b>Warning:</b>'
    }
    output$StationInfo <- renderUI({
      HTML(paste(warning,'You have selected:<b>',siteInfo$Name[rowNumber],'</b><br/> Which recorded temperature',formatC(perc,format='f',digits=1),'% of the time<br/><br/>',sep=' '))
    })
    # output$Click_text<-renderText({
    #   text2
    # })
  })

  # output$outputPDF <- downloadHandler(
  #   filename = function() {
  #     paste(currentFN(),'pdf',sep='.')
  #   },
  #   content=function(file) {
  #     makePDF(input$yearInput,input$cType,site$currentLoc,input$Y2Date)
  #     file.copy(from = "myGenerated.pdf", to = file)
  #   }
  # )

  output$outputJPEG <- downloadHandler(
    filename = function() {
      paste(currentFN(),'jpg',sep='.')
      },
    content=function(file) {
      makeJPEG(selectedYear$Year,input$cType,input$gType,site$currentLoc,input$Y2DateChill,input$startDate,input$endDate,input$JPEGHeight,input$tabs)
      file.copy(from = "myGenerated.jpg", to = file)
    }
  )


})

