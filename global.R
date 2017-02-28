library(shiny)

source('helper.R')

fluidRowUI <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
        column(width=2, align = 'center',uiOutput(ns("SelectedLocation"))),
        column(width=2, align = 'center',uiOutput(ns("yearOutput"))),
        column(width=2, align = 'center',uiOutput(ns("dateStart"))),
        column(width=2, align = 'center',uiOutput(ns("dateEnd")))
      )
  )
}


fluidRowServer <- function(input, output, session,selectedYear,currentYear,site,siteInfo){
  output$SelectedLocation <- renderUI({
    HTML(paste("<br/><b>",siteInfo$Name[site$currentLoc],"</b>"))
  })

  output$yearOutput <- renderUI({
    ns <- session$ns
    selectInput(ns('yearInput'),h4('Year'),as.character(seq(currentYear,1968,-1)), selectedYear$Year)
  })

  output$dateStart <- renderUI({
    ns <- session$ns
    if (is.null(input$yearInput)) {
      return(NULL)
    }
    selectedYear$Year <- input$yearInput
    # if(input$tabs == 'Chill'){
    #   if(input$cType == 3){
    #     value1 <- checkDate(paste(selectedYear$Year,"-05-01",sep=''))
    #   } else {
    #     value1 = checkDate(paste(selectedYear$Year,"-03-01",sep=''))
    #   }
    # }
    #
    # if(input$tabs == 'Growing Degrees' ){
    #   value1 = checkDate(paste(selectedYear$Year,"-05-01",sep=''))
    # }
    #
    # if(input$tabs == 'Temperature'){
    #   value1 = checkDate(paste(selectedYear$Year,"-01-01",sep=''))
    # }
    value1 = checkDate(paste(selectedYear$Year,"-03-01",sep=''))
    dateInput(ns("startDate"), label = h4("Start Date"), value = value1, min = paste(selectedYear$Year,"-01-01",sep=''), max =  Sys.Date() - 1)
  })


  output$dateEnd <- renderUI({
    ns <- session$ns
      if (is.null(input$yearInput)) {
        return(NULL) #year not read
      }
    dateInput(ns("endDate"), label = h4("End Date"), value = checkDateEnd(input$yearInput),  min = paste(selectedYear$Year,"-01-01",sep=''), max =  Sys.Date()-1)

  })
  return(list(startDate = value1))
}