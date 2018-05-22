# server.R
rm(list=ls())
library(chillR)
library(shinysky)
library(leaflet)
library(plotly)
library(rdrop2)

shinyServer(function(input, output, session) {

  stns <- reactiveValues()
  stns$df <- data.frame(row=numeric(0),stn=character(0))



  observe({input$jscookie
    if(debug)
      print('running getcookie from observe')
  js$getcookie()
  if(debug)
    cat('cookie length',length(input$jscookie),':',input$jscookie,':\n')
  if(length(input$jscookie) >= 1){
    if (!is.null(input$jscookie) & input$jscookie != '') {
      if(debug)
        cat("Found cookie", input$jscookie,length(input$jscookie), '\n')
      startStnRowID <- as.numeric(input$jscookie)
      startTown <- which(gaz$PlaceName ==  siteInfo[startStnRowID,'Name'])
      #update the label in the
      #stns$df[1,1] <- startStn
      #stns <- startStn
    } else {
      if(debug)
        print("No cookie")
      startStnRowID <- which(siteInfo$Name == 'Applethorpe')
      startTown <- which(gaz$PlaceName == 'Applethorpe')
    }
  }
  if(debug)
    cat('startStn',startStnRowID, siteInfo[startStnRowID,'Name'], 'startTown',startTown,'\n')
  stns$row <- startStnRowID
  stns$stn <- siteInfo$stnID
}) ## cookie stuff




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
    if(is.null(stns)){
      return(NULL)
    } else {
      if(debug)
        cat(stns$row,"<br/><h3>",siteInfo$Name[stns$row],"</h3>\n")
      HTML(paste("<br/><h3>",siteInfo$Name[stns$row],"</h3>"))
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

    output$chillPlot <- renderPlotly({
      if(is.null(input$isMobile )){
        print('mobile is null')
      }
      if(input$isMobile){
        print('this is a mobile device')
      } else {
        print('not a mobile device')
      }
      if ( is.null(input$startDate) | input$isMobile ) {
        return(NULL)
      }
      input$TriggerButton
      if(debug)
        cat('\n\n### Chill Plot ###\n')

      doThePlot(input$cType,stns$row,input$startDate,input$endDate)
    }) #renderPlot


    output$itsAMobile <- renderText({
      if(is.null(input$isMobile )){
        print('mobile is null')
      }
      if(input$isMobile){
        print('this is a mobile device')
      } else {
        print('not a mobile device')
      }
      if ( is.null(input$startDate) | !input$isMobile ) {
        return(NULL)
      }
      "You are on a mobile device"
    })


  ### GDH Plot ###
    output$GDHPlot <- renderPlotly({

      #print("gType and startDate...")
      if(  is.null(input$gType) | is.null(input$startDate) | is.null(input$endDate) | is.null(input$baseTemp )){
        return(NULL)
      }
      if(debug)
        print('### GDH Plot ###')
      # if(is.na(stns)){
      #   startStnRowID <- startStn
      # }
      doTheHeatPlot(selectedYear$Year,input$gType,input$startDate,input$endDate,stns$row,input$baseTemp) #selectedYear$Year

    }) #renderPlot



  #observe({
    output$TempPlot <- renderPlotly({
      if (is.null(input$startDate) | is.null(input$endDate) ) {
        return(NULL) #sliders not ready
      }
      if(debug)
        print('### Temperature Plot ###')
      # if(is.na(stns)){
      #   startStnRowID <- startStn
      # }
      #loadTheData(stns$df[1,1])
      doTheTempPlot(selectedYear$Year,input$startDate,input$endDate,stns$row) # selectedYear$Year
    })
  #})

  #observe({
    output$RainPlot <- renderPlotly({
      if (is.null(input$startDate) | is.null(input$endDate) ) {
        return(NULL) #sliders not ready
      }
      # if(is.na(stns)){
      #   return(NULL)
      # }
      doTheRainPlot(selectedYear$Year,input$startDate,input$endDate,stns$row) #selectedYear$Year
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
            selectInput("stnFound", label = h4("Select Station"),choices = stnList, size = 10, selectize = F,selected = stns$row)
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
    this <- startStn
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
    if(debug)
      print(paste("You've selected point", click$id,'at row #',rowNumber))
    perc <- (siteInfo$PercMaxTObs[rowNumber]+siteInfo$PercMinTObs[rowNumber])/2*100
    warning <- ''
    if(perc < 0.5) {
      warning <- '<b>Warning:</b>'
      output$StationInfo <- renderUI({
        HTML(paste(warning,siteInfo$Name[rowNumber],'recorded temperature',formatC(perc,format='f',digits=1),'% of the time<br/>',sep=' '))
      })
    }
    ###site$currentLoc <- rowNumber
    stns$row <- rowNumber

    if(input$KeepLocation){
      if(debug)
        cat("store Cookie", rowNumber, siteInfo$Name[rowNumber],'\n')
      js$setcookie(rowNumber)
      js$getcookie()
      if(debug)
        print(input$jscookie)
    }

  })
})


