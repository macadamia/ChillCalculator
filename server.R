#server.R

library(chillR)
library(rdrop2)
library(shinysky)
library(leaflet)

source('helper.R')

shinyServer(function(input, output) {

  site <- reactiveValues(currentlLoc=247,name='Applethorpe')

  currentFN <- function() { getFName(site$currentLoc,input$yearInput,input$cType,input$tabs) }

  output$yearOutput <- renderUI({
    selectInput('yearInput',h4('Select Year'),as.character(seq(as.numeric(format(Sys.Date(), "%Y")),1968,-1)), format(Sys.Date(), "%Y"))
  })

  output$dateForGDHOutput <- renderUI({
    if (is.null(input$yearInput)) {
      return(NULL) #slider not ready
    }
    dateInput("dateInput", label = h4("Start GDH"), value = paste(input$yearInput,"-01-01",sep=''))
  })

  checkFordata <- function() {
    cat('Start checkForData\n')
    #locations<-c('Applethorpe','Shepparton','Manjimup','Huonville','Orange','Mount Barker')
    #loc<-as.numeric(input$Location)
    #stnNums<-c(stnQld,stnVic,stnWA,stnTas,stnNSW,stnSA)
    stn<-siteInfo$stnID[site$currentLoc]
    lat<-siteInfo$latitude[site$currentLoc]
    cat('server:',stn,lat,'\n')

    if(input$cType == 1){
      sJDay<-1
      eJDay<-366
    }
    if(input$cType == 2){
      sJDay<-1
      eJDay<-366
    }
    if(input$cType == 3){
      sJDay<-as.numeric(format(as.Date(paste('01-05',Year,sep='-'),'%d-%m-%Y'),'%j'))
      eJDay<-as.numeric(format(as.Date(paste('31-08',Year,sep='-'),'%d-%m-%Y'),'%j'))
      print(eJDay)
    }

    #try and retrieve previously created data
    rdata <- file.path('Data',paste(stn,'.RData',sep=''))
    withProgress(message = 'Getting Data',value = 0, {
      print(getwd())
      print(rdata)
      load(rdata)
      incProgress(.9,detail='Create Plot')

      incProgress(1)
      print('Completed Check For data')
    }) #progress
  }

  ### Chill Plot ###
  observe({
    output$chillPlot <- renderPlot({
      if (is.null(input$yearInput)) {
        return(NULL) #slider not ready
      }
      checkFordata()
      cat('call doThePlot from server.R with',input$yearInput,'\n')
      doThePlot(input$yearInput,input$cType,site$currentLoc,input$Y2Date,3)
    },height=input$height) #renderPlot
  })#observe

  ### GDH Plot ###
  observe({
    output$GDHPlot <- renderPlot({
      if (is.null(input$yearInput) | is.null(input$dateInput)) {
        return(NULL) #sliders not ready
      }
      checkFordata()
      doTheHeatPlot(input$yearInput,input$dateInput,site$currentLoc,input$Y2Date,3)
    },height=input$height) #renderPlot
  })

  ### Leaflet Map ###########
  output$map <- renderLeaflet({
    if (is.null(input$yearInput)) {
      return(NULL) #slider not ready
    }
    zoom <- 8
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
    cat(rlng,rlat,'\n')
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
    print(text2)
    site$currentLoc <- rowNumber
    # output$Click_text<-renderText({
    #   text2
    # })
  })

  output$outputPDF <- downloadHandler(
    filename = function() {
      paste(currentFN(),'pdf',sep='.')
    },
    content=function(file) {
      makePDF(input$yearInput,input$cType,site$currentLoc,input$Y2Date,2)
      file.copy(from = "myGenerated.pdf", to = file)
    }
  )

  output$outputJPEG <- downloadHandler(
    filename = function() {
      paste(currentFN(),'jpg',sep='.')
      },
    content=function(file) {
      makeJPEG(input$yearInput,input$cType,input$Location,input$Y2Date,input$dateInput,input$height,input$tabs,2)
      file.copy(from = "myGenerated.jpg", to = file)
    }
  )

})

