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
            column(width=4, align = 'center',uiOutput("SelectedLocation")),
            column(width=2, align = 'center',uiOutput("yearOutput")),
            column(width=2, align = 'center',uiOutput("dateStart")),
            column(width=2, align = 'center',uiOutput("dateEnd")),
            column(width=2, align = 'center',uiOutput("baseTemp"))
          ),
          tabsetPanel(id='tabs',
          tabPanel("How To Use This Site",
                   h3('Introduction'),
                   helpText('This site accesses daily weather station data for the period 1968 to the current day for 600 stations. This is used to make calculations for chilling requirements and heating.
                            Options are selected from the sidebar and the calculation is redone. Chilling can be calculated as either Chill Portions, Chill Units or Chill Hours.
                            All calculations are performed using the chillR package of Luedeling et al. (2013).'),

                   h3('Where and When'),
                   helpText('There are 600 locations in the database which can be selected from the Locations tab. The map is zoomable and pannable in the normal way. Select a station by clicking the marker and selecting with Chill or GDH to see the data. Initially the map is zoomed to the Granite Belt.'),
                   helpText('Years from 1968 to the present day can be chosen. The latest weather day is
                            available after about midday (AEST) and is current then up to yesterday. The Year To Date option is useful during the current year when one wants to concentrate on
                            the progress of the chill or heat accumulation for the most recent period. Data are displayed with the long-term average, and the warmest and coolest years.
                            data from 1981 to 2010 are used for the long-term averages etc.'),

                   h3('Chill Accumulation'),
                   helpText('Chilling can be calculated in either Chill Portions (Erez et al. 1990), Chill Hours (Bennet 1949, Weinberger 1950) or Chill Units (Richardson et al. 1974).'),

                   h3('Growing Degrees'),
                   helpText('Growing Degree Hours (GDH) (Anderson et al 1974) or Growing Degree Days (GDD) are calculated when the user selects the "Growing Degrees" tab. GDH are calculated using the methodology of Anderson et al. (1986) that uses four thresholds to calculate the accumulation of yield.
                            GDD are calculated by subtracting 10ºC (or which ever base temperature is used) from the average daily temperature. Only positive values are accumulated, i.e. temperatures greater than 10ºC.
                            These can be accumulated from 1 January (default) or set at a later date using the date selector box. To close the calendar simply click outside of it.')#,

                   # h3('Other Options'),
                   # helpText("The plot height can be controlled using the slider. It moves in 100-pixel steps and can set between 400 and 1200 pixels. The width is set as the width of the main panel.
                   #          A jpeg of the image is created and downloaded to the user's computer when the download button is selected."),
                   #  hr()
          ),
          tabPanel("Locations", value='Locations',busyIndicator("Calculation In progress",wait = 0),
                   fluidPage(
                     fluidRow(
                       column(width=4,
                          selectInput("Region", label = h4("Select Region"),choices = list("Granite Belt" = 1, "NSW" = 2, "Victoria" = 3, 'Tas' = 4, 'SA' = 5, 'WA' = 6), selected = 1),
                              textInput("Location", label = h4("Search For Station"),value=''),
                              htmlOutput("NMatches"),
                              textInput("Town", label = h4("Search For Town"),value=''),
                              htmlOutput("StationInfo"),
                              htmlOutput("NTowns")
                       ),
                       column(width=8,
                              leafletOutput("map", width='600px',height='600px' )
                      )
                    )
                  )
          ),
          tabPanel("Chill", value='Chill',busyIndicator("Calculation In progress",wait = 0),
            fluidPage(
              fluidRow(
                column(width=2,
                       radioButtons("cType", label = h4("Chill"), choices = list("Portions" = 1, "Hours" = 2, "Units" = 3),selected = 1)
                ),
                column(width=3,
                       radioButtons("Y2DateChill", label = h4("Year To Date"),choices = list("Yes" = 1, "No" = 2),selected = 1)
                )
              ),#fluidRow
              fluidRow(
                plotlyOutput("chillPlot")
              )
            )#fluidPage
        ),
          tabPanel("Growing Degrees", value ='Growing Degrees', busyIndicator("Calculation In progress",wait = 0),
              fluidPage(
                fluidRow(
                  column(width=2,
                         radioButtons("Y2DateGDH", label = h4("Year To Date"), choices = list("Yes" = 1, "No" = 2),selected = 1)
                  ),
                  column(width=3,
                         radioButtons("gType", label = h4("Growing Degree"), choices = list("Hours" = 1, "Days" = 2),selected = 2)
                  )
                ),
                fluidRow(
                  plotlyOutput("GDHPlot")
                )
              )#fluidPage
          ),
          tabPanel("Temperature", value ='Temperature', busyIndicator("Calculation In progress",wait = 0),
                   fluidPage(
                     fluidRow(
                       plotlyOutput("TempPlot")
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
                      helpText("Richardson EA, Seeley SD, Walker DR (1974) A model for estimating the completion of rest for Redhaven and Elberta peach trees. HortScience 9(4), 331-332"),
                      helpText("Weinberger JH (1950) Chilling requirements of peach varieties. Proc Am Soc Hortic Sci 56, 122-128")
                     )
                   )
          ),
          tabPanel("About & Legal",
                   h4("About This Site"),
                   helpText("This site is being developed to deliver up-to-date and historical information on the accumulation of chill and growing degree days."),
                   helpText('Based on or contains data provided by the State of Queensland (Department of Science, Information Technology and Innovation) [2016]. In consideration of the State permitting use of this data you acknowledge and agree that the State gives no warranty in relation to the data (including accuracy, reliability, completeness, currency or suitability) and accepts no liability (including without limitation, liability in negligence) for any loss, damage or costs (including consequential damage) relating to any use of the data. Data must not be used in breach of the privacy laws.'),
                   hr(),
                   helpText("If you would like further information please contact: "),
                   HTML("<a href=mailto:heidi.parkes@daf.qld.gov.au?subject=Phenology%20Calculator>Dr Heidi Parkes, Qld Dept. of Agriculture and Fisheries</a>"),
                   helpText("For Technical issues: "),
                   HTML("<a href=mailto:Neil.White@daf.qld.gov.au?subject=Phenology%20Calculator>Dr Neil White, Qld Dept. of Agriculture and Fisheries</a> <br/><br/>")
          )
        ) #tabset
  )#fluidPage
)#shinyUI
