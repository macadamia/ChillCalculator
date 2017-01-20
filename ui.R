#Chill Units Calculator
#ui.R
library(shiny)
library(chillR)
library(shinysky)
library(leaflet)
#
# lastYr <- length(years)
# yearChoices <- as.list(setNames(seq(1,lastYr),as.character(years)))

shinyUI(

  fluidPage(

    tags$head(includeScript("google_analytics.js")),

    titlePanel(""),

    # Generate a row with a sidebar
    sidebarLayout(

      # Define the sidebar with one input
      sidebarPanel(width=2,
        h3("Select:"),
        selectInput("Region", label = h4("Select Region"),
                    choices = list("Granite Belt" = 1, "NSW" = 2,
                                   "Victoria" = 3, 'Tas' = 4, 'SA' = 5, 'WA' = 6), selected = 1),
        # selectInput("Location", label = h4("Select Site"),
        #             choices = list("Applethorpe" = 1, "Shepparton" = 2,
        #                            "Manjimup" = 3, 'Huonville' = 4, 'Orange' = 5, 'Mt Barker' = 6), selected = 1),
        uiOutput("yearOutput"),
        radioButtons("Y2Date", label = h4("Year To Date"),
                     choices = list("Yes" = 1, "No" = 2),selected = 1),
        radioButtons("cType", label = h4("Chill Type"),
                     choices = list("Portions" = 1, "Hours" = 2),selected = 1),
        uiOutput("dateForGDHOutput"),
        sliderInput("height", "Plot Height (px)", min = 400, max = 1200,step = 100, value = 600),
        #downloadButton("outputPDF", "Download PDF"),
        downloadButton("outputJPEG", "Download JPEG")
      ),

      mainPanel(
          tabsetPanel(id='tabs',
          tabPanel("Help",
                   h3('Introduction'),
                   helpText('This site accesses daily weather station data for the period 1968 to the current day. This used to make calculations for chilling requirements and heating.
                            Options are selected from the sidebar and the calculation is redone. Chilling can be calculated as either Chill Portions or Chill Hours.
                            All calculations are performed using the chillR package of Luedeling et al. (2013).'),

                   h3('Where and When'),
                   helpText('There are 597 locations in the database which can be selected from the Locations tab. The map is zoomable and pannable in the normal way. Select a station by clicking the marker and selecting with Chill or GDH to see the data. Initially the map is zoomed to the Granite Belt.'),
                   helpText('Years from 1968 to the present day can be chosen. The latest weather day is
                            available after about midday (AEST) and is current then up to yesterday. The Year To Date option is useful during the current year when one wants to concentrate on
                            the progress of the chill or heat accumulation for the most recent period. Data are displayed with the long-term average, and the warmest and coolest years.
                            data from 1981 to 2010 are used for the long-term averages etc.'),

                   h3('Chill Accumulation'),
                   helpText('Chilling can be calculated in either Chill Portions (Fishman et al. 1987) or Chill Hours (Richardson et al. 1974).'),
                   helpText("Chill is calculated as either chill portions or chill hours. Chill units and the ability to search for locations will be added in the future."),

                   h3('Growing Degree Days'),
                   helpText('Growing Degree Hours (GDH) are calculated when the user selects the GDH tab. GDH are calculated using the methodology of Anderson et al. (1986) that uses four thresholds to calculate the accumulation of yield.
                            These can be accumulated from 1 January (default) or set at a later date using the date selector box. To close the calendar simply click outside of it.'),

                   h3('Other Options'),
                   helpText("The plot height can be controlled using the slider. It moves in 100-pixel steps and can set between 400 and 1200 pixels. The width is set as the width of the main panel.
                            A jpeg of the image is created and downloaded to the user's computer when the download button is selected."),
                   helpText("If you would like further information please contact: "),
                   HTML("<a href=mailto:heidi.parkes@daf.qld.gov.au?subject=Phenology%20Calculator>Dr Heidi Parkes, Qld Dept. of Agriculture and Fisheries</a>"),
                   helpText("For Technical issues: "),
                   HTML("<a href=mailto:Neil.White@daf.qld.gov.au?subject=Phenology%20Calculator>Dr Neil White, Qld Dept. of Agriculture and Fisheries</a>"),
                   h4("References"),
                   helpText("Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat requirements of cherry trees - a statistical approach. International Journal of Biometeorology 57,679-689."),
                   helpText("Fishman S, Erez A, Couvillon GA. 1987. The temperature dependence of dormancy breaking in plants: Mathematical analysis of a two-step model involving a cooperative transition. Journal of Theoretical Biology, 124: 473-483."),
                   helpText("Weinberger JH (1950) Chilling requirements of peach varieties. Proc Am Soc Hortic Sci 56, 122-128"),
                   helpText("Bennett JP (1949) Temperature and bud rest period. Calif Agric 3 (11), 9+12"),
                   helpText("Richardson EA, Seeley SD, Walker DR (1974) A model for estimating the completion of rest for Redhaven and Elberta peach trees. HortScience 9(4), 331-332"),
                   helpText("Anderson, J., Richardson, E., & Kesner, C. (1986). Validation of chill unit and flower bud phenology models for 'Montmorency' sour cherry. Acta Horticulturae, 184, 74-78."),
                   hr()
          ),
          tabPanel("Locations", value='Select',busyIndicator("Calculation In progress",wait = 500),
                   leafletOutput("map", width='800px',height='600px' )
          ),
          tabPanel("Chill", value='Chill',busyIndicator("Calculation In progress",wait = 10), plotOutput("chillPlot")),
          tabPanel("GDH", value ='GDH', busyIndicator("Calculation In progress",wait = 10), plotOutput("GDHPlot")),
          tabPanel("About & Legal",
                   h4("About This Site"),
                   helpText("This site is being developed to deliver up-to-date and historical information on the accumulation of chill and growing degree days."),
                   hr(),
                   helpText('Based on or contains data provided by the State of Queensland (Department of Science, Information Technology and Innovation) [2016]. In consideration of the State permitting use of this data you acknowledge and agree that the State gives no warranty in relation to the data (including accuracy, reliability, completeness, currency or suitability) and accepts no liability (including without limitation, liability in negligence) for any loss, damage or costs (including consequential damage) relating to any use of the data. Data must not be used in breach of the privacy laws.')

          )
        ) #tabset
      )#mainPanel
    )#sidebarLayout
  )#fluidPage
)#shinyUI
