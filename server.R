# server.R
rm(list=ls())
library(chillR)
library(shinysky)
library(leaflet)
library(plotly)
library(rdrop2)

source('helper.R')

shinyServer(function(input, output, session) {

  observe({input$jscookie
    if(debug)
      print('running getcookie from observe')
  js$getcookie()
  if(debug)
    cat('cookie length',length(input$jscookie),':',input$jscookie,':\n')
  if(length(input$jscookie) < 1){
    if(debug)
      print("No cookie (length 0)")
    startStn <- which(siteInfo$Name == 'Applethorpe')
    startTown <- which(gaz$PlaceName == 'Applethorpe')
  } else {
    if (!is.null(input$jscookie) & input$jscookie != '') {
      if(debug)
        cat("Found cookie", input$jscookie,length(input$jscookie), '\n')
      startStn <- as.numeric(input$jscookie)
      startTown <- which(gaz$PlaceName ==  siteInfo[as.numeric(input$jscookie),'Name'])
      #update the label in the
      stns$df[1,1] <- startStn
      if(debug)
        cat('startStn',startStn,'startTown',startTown,'\n')
    } else {
      if(debug)
        print("No cookie")
      startStn <- which(siteInfo$Name == 'Applethorpe')
      startTown <- which(gaz$PlaceName == 'Applethorpe')
    }
  }
})

  stns <- reactiveValues()
  stns$df <- data.frame(row=numeric(0),stn=character(0))

  currentYear <- as.numeric(format(Sys.Date(), "%Y"))# just used to update the Year drop down

  #list(Year=as.numeric(format(Sys.Date(), "%Y")))
  selectedYear <-  reactiveValues(Year=as.numeric(format(Sys.Date(), "%Y")))

  results <- reactive({
    searchForLocation(input$Location)
  })

  towns <- reactive({
    #print('Search for Towns')
    searchForPlace(input$Town)
  })

  output$Logos <- renderUI(
    HTML(
        '<style>
          table#t01 {
          width: 100%;
          }
        </style>
        <table id="t01">
          <td>
            <a href="//www.daf.qld.gov.au"> <img src="qg_logo_mar.png">  </a>
          </td>
          <td style="float:right;">
            <a href="//www.horticulture.com.au"> <img src="HortInnovation-rgb-web.jpg">  </a>
          </td>
        </table>'
    )
  )

  output$Share <- renderUI(
    HTML(
      '<a href="https://twitter.com/share" class="twitter-share-button" data-size="large" data-show-count="false">Tweet</a><script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>
      <iframe src="https://www.facebook.com/plugins/share_button.php?href=https%3A%2F%2Fhort-science.shinyapps.io%2FChillCalculator%2F&layout=button_count&size=large&mobile_iframe=true&width=84&height=28&appId" width="84" height="28" style="border:none;overflow:hidden" scrolling="no" frameborder="0" allowTransparency="true"></iframe>'
    )
  )



  output$SelectedLocation <- renderUI({
    if(is.null(stns$df[1,1])){
      stns$df[1,1] <- startStn
    }
    ## replace the UI with a table or something using stns$df
    if(is.na(siteInfo$Name[stns$df[1,1]])){
      HTML(paste("<br/><h3>Applethorpe</h3>"))
    } else {
      if(is.na(stns$df[2,1])){
        HTML(paste("<br/><h3>",siteInfo$Name[stns$df[1,1]],"</h3>"))
      } else {
        HTML(paste("<br/><h3>",siteInfo$Name[stns$df[1,1]],"<br/>",siteInfo$Name[stns$df[2,1]],"</h3>"))
      }
    }
  })


  output$yearOutput <- renderUI({
    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      selectInput('yearInput',h4('Year'),as.character(seq(currentYear,1968,-1)), selectedYear$Year) #currentYear
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
    thisYear <- as.numeric(format(Sys.Date(),tz='Australia/Brisbane','%Y'))
    if(aYear == thisYear){
      nDaysToSubstract <- 1
      options(warn=-1) # to ignore tz warning
      if(as.numeric(format(Sys.time(),tz='Australia/Brisbane','%H')) < 12){ # silo probably not updated
        nDaysToSubstract <- 1
      }
      options(warn=0)
      return(Sys.Date() - nDaysToSubstract)
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
      value1 = isolate(checkDate(paste(selectedYear$Year,"-05-01",sep='')))
    }

    if(input$tabs == 'Temperature'){
      value1 = checkDate(paste(selectedYear$Year,"-01-01",sep=''))
    }

    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      dateInput("startDate", label = h4("Start Date"), value = value1, min = paste(selectedYear$Year,"-01-01",sep=''), max =  Sys.Date() - 1)
    }
  })

  output$dateEnd <- renderUI({
    if(is.null(input$yearInput)){
      return(NULL)
    }
    selectedYear$Year <- input$yearInput

    maxValue <- checkDateEnd(as.character(Sys.Date()-1))
    if( input$tabs == 'Growing Degrees'){
      maxValue = NULL
    }

    if(input$tabs == 'Chill' | input$tabs == 'Growing Degrees' | input$tabs == 'Temperature'){
      dateInput("endDate", label = h4("End Date"), value = checkDateEnd(as.character(Sys.Date())),
                min = paste(selectedYear$Year,"-01-01",sep=''), max = maxValue)
    }
  })

  output$baseTemp <- renderUI({
    if(input$tabs == 'Growing Degrees'){
      textInput('baseTemp',h4("Base Temperature (ºC)"),"10",'300px')
    }
  })


  ### Chill Plot ###
 #observe( {
    output$chillPlot <- renderPlotly({
      if ( is.null(input$startDate) ) {
        return(NULL) #slider not ready
      }
      if(is.na(stns$df[1,1])){
        stns$df[1,1] <- startStn
        #print(stns$df)
      }
      input$TriggerButton
      if(debug)
        cat('\n\n### Chill Plot ###\n')
      #startJDay <- isolate(as.numeric(format(input$startDate,'%j')))
      #doThePlot(isolate(selectedYear$Year),isolate(input$cType),stns$df[1,1],isolate(input$startDate),isolate(input$endDate))
      #doThePlot(selectedYear$Year,input$cType,stns$df[1,1],input$startDate,input$endDate)
      doThePlot(input$cType,stns$df[1,1],input$startDate,input$endDate)
    }) #renderPlot
  #})#observe

  #need a second chill plot
  #observe({
    output$chillPlot2 <- renderPlotly({
      if ( is.null(input$startDate) ){
        return(NULL) #slider not ready
      }
      if(is.na(stns$df[2,1])){
        return(NULL)
      }
      doThePlot(input$cType,stns$df[2,1],input$startDate,input$endDate)
    }) #renderPlot
  #})#observe

  ### GDH Plot ###
  #observe({
    output$GDHPlot <- renderPlotly({

      #print("gType and startDate...")
      if(  is.null(input$gType) | is.null(input$startDate) | is.null(input$endDate) | is.null(input$baseTemp )){
        return(NULL)
      }
      if(debug)
        print('### GDH Plot ###')
      if(is.na(stns$df[1,1])){
        stns$df[1,1] <- startStn
      }
      doTheHeatPlot(selectedYear$Year,input$gType,input$startDate,input$endDate,stns$df[1,1],input$baseTemp) #selectedYear$Year

    }) #renderPlot
  #})

  #observe({
    output$GDHPlot2 <- renderPlotly({
      if( is.null(input$gType) | is.null(input$startDate) | is.null(input$endDate) | is.null(input$baseTemp )){
        return(NULL)
      }
      if(is.na(stns$df[2,1])){
        return(NULL)
      }
      #loadTheData(stns$df[2,1])
      doTheHeatPlot(selectedYear$Year,input$gType,input$startDate,input$endDate,stns$df[2,1],input$baseTemp) # selectedYear$Year

    }) #renderPlot
  #})

  #observe({
    output$TempPlot <- renderPlotly({
      if (is.null(input$startDate) | is.null(input$endDate) ) {
        return(NULL) #sliders not ready
      }
      if(debug)
        print('### Temperature Plot ###')
      if(is.na(stns$df[1,1])){
        stns$df[1,1] <- startStn
      }
      #loadTheData(stns$df[1,1])
      doTheTempPlot(selectedYear$Year,input$startDate,input$endDate,stns$df[1,1]) # selectedYear$Year
    })
  #})

  #observe({
    output$TempPlot2 <- renderPlotly({
      if (is.null(input$startDate) | is.null(input$endDate) ) {
        return(NULL) #sliders not ready
      }
      if(is.na(stns$df[2,1])){
        return(NULL)
      }
      #loadTheData(stns$df[2,1])
      doTheTempPlot(selectedYear$Year,input$startDate,input$endDate,stns$df[2,1]) #selectedYear$Year
    })
  #})



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
      zoom <- 9
    }
    if(input$Region == "4"){
      #TAS
      mtch <- match('Hobart Airport',siteInfo$Name)
      rlng <- siteInfo$longitude[mtch]
      rlat <- siteInfo$latitude[mtch]
      zoom <- 10
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
    if(debug)
      cat('About to run leaflet\n')
    leaflet(data = siteInfo) %>%
      addTiles() %>%
      addMarkers(~longitude,~latitude, layerId = ~stnID, popup = ~NamePerc) %>%
      setView(lng = rlng, lat = rlat, zoom = zoom)
  })

  observeEvent ( input$stnFound, {
    this <- as.numeric(input$stnFound)
    ####site$currentLoc <- this
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

  observeEvent(input$recentre, {
    if (is.null(input$recentre)) {
      return(NULL)
    }
    this <- stns$df$row[1]
    rlng <- siteInfo$longitude[this]
    rlat <- siteInfo$latitude[this]
    cat(startStn,rlat,rlng,'\n')
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
    ####site$currentLoc <- rowNumber
    if(nrow(stns$df) == 0){
      stns$df <- data.frame(row=rowNumber, stn=siteInfo$Name[rowNumber])
    } else if(nrow(stns$df) == 1){
      #print("add to 2nd row")
      tmp <- data.frame(row=rowNumber, stn=siteInfo$Name[rowNumber])
      stns$df <- rbind(stns$df, tmp)
    } else {
      #move 2nd row to first row and add to 2nd row, or replace 2 depending on FixTopStn
      if(stns$df$row[2] == rowNumber){
        #print("already have it, replace the df with just this stn")
        stns$df <- stns$df[2,]
      } else { # replace or shift
        tmp <- data.frame(row=rowNumber, stn=siteInfo$Name[rowNumber])
        if(input$FixTopStn){
          #print("keep top")
          stns$df <- stns$df[1,]
          stns$df <- rbind(stns$df, tmp)
        } else {
          #print("replace top")
          stns$df <- stns$df[2,]
          stns$df <- rbind(stns$df, tmp)
        }
      }
    }
    if(nrow(stns$df) == 2){
      if(stns$df[1,1] == stns$df[2,1]){
        #print("Two the same")
        stns$df <- stns$df[1,]
      }
    }
    if(input$KeepLocation){
      if(debug)
        print("store Cookie")
      js$setcookie(rowNumber)
      js$getcookie()
      if(debug)
        print(input$jscookie)
    }

  })
})
