#Chill Units Calculator
#ui.R
library(shiny)
library(chillR)
library(plotly)
library(shinysky)
library(leaflet)

#

shinyUI(

  fluidPage(

    tags$head(includeScript("google_analytics.js")),

    titlePanel(""),

          fluidRow(
            column(width=2, align = 'left',uiOutput("SelectedLocation")),
            #column(width=2, align = 'center',uiOutput("FixTop")),
            #column(width=2, align = 'left',checkboxInput("FixTopStn",h4("Fix Stn"),F)),
            column(width=2, align = 'center',uiOutput("yearOutput")),
            column(width=2, align = 'center',uiOutput("dateStart")),
            column(width=2, align = 'center',uiOutput("dateEnd")),
            column(width=2, align = 'center',uiOutput("baseTemp"))
          ),
          tabsetPanel(id='tabs',
          tabPanel("How To Use This Site",
            includeHTML('HowToUse.html')
          ),
          tabPanel("Locations", value='Locations',busyIndicator("Calculation In progress",wait = 0),
                   fluidPage(
                     fluidRow(
                       column(width=2,
                          selectInput("Region", label = h4("Select Region"),choices = list("Granite Belt" = 1, "NSW" = 2, "Yarra Valley" = 3, 'Tas' = 4, 'SA' = 5, 'southern WA' = 6), selected = 1),
                              textInput("Location", label = h4("Search For Station"),value=''),
                              #htmlOutput("NMatches"),
                              uiOutput("BuildStnLocations"),
                              #selectInput("stnFound",label = h4("Select Station"),choices ="Applethorpe",size=10,selectize=F),
                              textInput("Town", label = h4("Town Starts With..."),value=''),
                              htmlOutput("StationInfo"),
                              uiOutput("NTowns")
                       ),
                       column(width=2, align = 'left',
                        checkboxInput("FixTopStn","Keep upper station",F,width='100%')
                       ),
                       column(width=9,
                              leafletOutput("map", width='100%',height='600px' )
                      )
                    )
                  )
          ),
          tabPanel("Chill", value='Chill',busyIndicator("Calculation In progress",wait = 0),
            fluidPage(
              fluidRow(
                column(width=2,
                       radioButtons("cType", label = h4("Chill"), choices = list("Portions" = 1, "Hours" = 2, "Units" = 3),selected = 1)
                )#,
                # column(width=3,
                #        radioButtons("Y2DateChill", label = h4("Year To Date"),choices = list("Yes" = 1, "No" = 2),selected = 1)
                #)
              ),#fluidRow
              fluidRow(
                plotlyOutput("chillPlot")
              ),#fluidRow
              fluidRow(
                plotlyOutput("chillPlot2")
              )

            )#fluidPage
        ),
          tabPanel("Growing Degrees", value ='Growing Degrees', busyIndicator("Calculation In progress",wait = 0),
              fluidPage(
                fluidRow(
                  # column(width=2,
                  #        radioButtons("Y2DateGDH", label = h4("Year To Date"), choices = list("Yes" = 1, "No" = 2),selected = 1)
                  # ),
                   column(width=3,
                          radioButtons("gType", label = h4("Growing Degree"), choices = list("Hours" = 1, "Days" = 2),inline = T,selected = 2)
                  )
                ),
                fluidRow(
                  plotlyOutput("GDHPlot")
                ),
                fluidRow(
                  plotlyOutput("GDHPlot2")
                )
              )#fluidPage
          ),
          tabPanel("Temperature", value ='Temperature', busyIndicator("Calculation In progress",wait = 0),
                   fluidPage(
                     fluidRow(
                       plotlyOutput("TempPlot")
                     ),
                     fluidRow(
                       plotlyOutput("TempPlot2")
                     )
                )
          ),
          tabPanel('Additional Information',
                   fluidRow(
                     column(width=12,
                      includeHTML('Additional.html'),
                      h4("References"),
                      helpText("Anderson, J., Richardson, E., & Kesner, C. (1986). Validation of chill unit and flower bud phenology models for 'Montmorency' sour cherry. Acta Horticulturae, 184, 74-78."),
                      helpText("Bennett JP (1949) Temperature and bud rest period. Calif Agric 3 (11), 9-12"),
                      helpText('Darbyshire, R., K. Pope and I. Goodwin (2016). An evaluation of the chill overlap model to predict flowering time in apple tree. Scientia Horticulturae 198: 142-149.'),
                      helpText("Erez A, Fishman S, Linsley-Noakes GC, Allan P (1990). The dynamic model for rest completion in peach buds. Acta Hortic 276, 165-174"),
                      helpText('Ghariani, K. and R. L. Stebbins (1994). Chilling requirements of apple and pear cultivars. Fruit Varieties Journal 48: 215.'),
                      helpText("Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology 57,679-689."),
                      helpText("Luedeling, E., 2017. chillR: Statistical methods for phenology analysis in temperate fruit trees. R package version 0.66, URL http://cran.r-project.org/web/packages/chillR/."),
                      helpText("Richardson EA, Seeley SD, Walker DR (1974) A model for estimating the completion of rest for Redhaven and Elberta peach trees. HortScience 9(4), 331-332"),
                      helpText("Weinberger JH (1950) Chilling requirements of peach varieties. Proc Am Soc Hortic Sci 56, 122-128")
                     )
                   )
          ),
          tabPanel("About & Legal",
                   h4("About This Site"),
                   helpText("This site is being developed to deliver up-to-date and historical information on the accumulation of chill and growing degree days."),
                   helpText("This work was undertaken for the project AP12029 Understanding apple and pear production systems in a changing climate funded by Horticulture Innovation Australia Limited using the Apple and Pear Industry levy and funds from the Australian Government. Additional financial support was contributed by Department of Agriculture and Fisheries (Qld), Department of Economic Development, Jobs, Transport and Resources (Vic), Department of Agriculture and Food Western Australia and Pomewest (WA)."),
                   helpText('Based on or contains data provided by the State of Queensland (Department of Science, Information Technology and Innovation) [2016]. In consideration of the State permitting use of this data you acknowledge and agree that the State gives no warranty in relation to the data (including accuracy, reliability, completeness, currency or suitability) and accepts no liability (including without limitation, liability in negligence) for any loss, damage or costs (including consequential damage) relating to any use of the data. Data must not be used in breach of the privacy laws.'),
                   hr(),
                   helpText("If you would like further information please contact: "),
                   HTML("<a href=mailto:heidi.parkes@daf.qld.gov.au?subject=Phenology%20Calculator>Dr Heidi Parkes, Qld Dept. of Agriculture and Fisheries</a>"),
                   helpText("For Technical issues: "),
                   HTML("<a href=mailto:Neil.White@daf.qld.gov.au?subject=Phenology%20Calculator>Dr Neil White, Qld Dept. of Agriculture and Fisheries</a> <br/><br/>"),
                   helpText("© State of Queensland, Department of Agriculture and Fisheries and Horticulture Innovation Australia Ltd, 2017.")
          )
        ) #tabset
  )#fluidPage
)#shinyUI
