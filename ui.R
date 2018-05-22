#Chill Units Calculator
#ui.R



mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}


shinyUI(

  fluidPage(
    includeCSS( "www/assets/v3/css/qg-main.css"),
    tags$head(
      tags$script(src = "js.cookie.js")
    ),
    useShinyjs(),
    extendShinyjs('www/myJSCode.js'),
    tags$head(includeScript("google_analytics.js"),
    tags$head(tags$style('.headerRow{background-color: #4E7707;}')),
    tags$div(id="fb-root"),

      #tags$meta(http-equiv="Content-Type", content="text/html; charset=utf-8"),
      tags$meta(name="description", content="calculates the chill and heating degrees for plant growth"),
      tags$meta(name="keywords", content="chill, chill portions, chilling units, chill hours, growing degree days, growing degree hours, temperature, Australia, fruit, nut, trees "),

      tags$link(rel="schema.DCTERMS", href="http://purl.org/dc/terms/"),
      tags$link(rel="schema.AGLSTERMS", href="http://www.agls.gov.au/agls/terms/"),

      tags$meta(name="DCTERMS.creator", scheme="AGLSTERMS.GOLD", content="c=AU; o=The State of Queensland; ou=DAF; ou=UNIT H&FS"),
      tags$meta(name="DCTERMS.publisher", scheme="AGLSTERMS.AglsAgent", content="corporateName=The State of Queensland; jurisdiction=Queensland"),
      tags$meta(name="DCTERMS.created", content="2017-7-12"),
      tags$meta(name="DCTERMS.modified", content="2017-7-12"),
      tags$meta(name="DCTERMS.title", content="Chill and Heat Calculator"),
      tags$meta(name="DCTERMS.alternative", content="Add your heading"),
      tags$meta(name="DCTERMS.description", content="DESCRIPTION"),
      tags$meta(name="DCTERMS.subject", scheme="AGLSTERMS.APAIS", content="SUBJECT"),
      tags$meta(name="AGLSTERMS.function", scheme="AGLSTERMS.AGIFT", content="FUNCTION"),
      tags$meta(name="DCTERMS.type", scheme="DCTERMS.DCMIType", content="Text"),
      tags$meta(name="AGLSTERMS.documentType", scheme="AGLSTERMS.agls-document", content="guidelines"),
      tags$meta(name="DCTERMS.audience", scheme="AGLSTERMS.agls-audience", content=""),
      tags$meta(name="DCTERMS.jurisdiction", scheme="AGLSTERMS.AglsJuri", content="Queensland"),

      tags$meta(name="DCTERMS.license", scheme="DCTERMS.URI", content="https://creativecommons.org/licenses/by/4.0/"),

      #tags$meta(http-equiv="X-UA-Compatible", content="IE=edge"),
      tags$meta(name="viewport", content="width=device-width, initial-scale=1"),

      tags$link(rel="shortcut icon", href="assets/v3/images/favicon.ico"),

      tags$script("https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"),

      tags$link(href="//fonts.googleapis.com/css?family=Lato:100,300,400,700,900,100italic,300italic,400italic,700italic,900italic", rel="stylesheet", type="text/css"),
      tags$noscript(
        tags$link(href="assets/v3/css/qg-noscript.css", rel="stylesheet", type="text/css", media="all")
      ),
      tags$link(href="assets/v3/css/qg-documentation.css", rel="stylesheet", type="text/css", media="all"),
      tags$script("https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"),
      tags$script("assets/v3/lib/ext/butterfly/jquery.resize-events.js"),
      tags$script("assets/v3/lib/ext/butterfly/jquery.history.js"),
      tags$script("assets/v3/lib/ext/butterfly/jquery.butterfly.js"),
      tags$script("assets/v3/js/qg-main.js")
    ),


    fluidRow(
      column(width=12,uiOutput("Logos"))
    ),
    fluidRow(class = 'headerRow',
             column(width=12,uiOutput("Share"))
    ),

    fluidRow(class = 'headerRow',
        column(width=3, align = 'left', uiOutput("SelectedLocation")),
        column(width=2, align = 'center',uiOutput("yearOutput")),
        column(width=2, align = 'center',uiOutput("dateStart")),
        column(width=2, align = 'center',uiOutput("dateEnd")),
        column(width=3, align = 'center',uiOutput("baseTemp"))
    ),
      tabsetPanel(id='tabs',
      tabPanel("Introduction",
        includeHTML('Introduction.html')
      ),
      tabPanel("How To Use This Site",
        includeHTML('HowToUse.html')
      ),
      tabPanel("Locations", value='Locations',busyIndicator("Calculation In progress",wait = 0),
         fluidPage(
           fluidRow(
             column(width=2,
                selectInput("Region", label = h4("Select Region"),choices = list("Granite Belt" = 1, "NSW" = 2, "Victoria" = 3, 'Tasmania' = 4, 'SA' = 5, 'southern WA' = 6), selected = 1),
                    textInput("Location", label = h4("Search For Station"),value=''),
                    uiOutput("BuildStnLocations"),
                    textInput("Town", label = h4("Town Starts With..."),value=''),
                    htmlOutput("StationInfo"),
                    uiOutput("NTowns")
             ),
             column(width=4, align = 'left',
                    checkboxInput("KeepLocation","Remember this station",F,width='100%'),
                    actionButton("recentre","Recentre")
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
            column(width=6,
                   radioButtons("cType", inline = T, label = h4("Chill"), choices = list("Portions" = 1, "Hours" = 2, "Units" = 3),selected = 1)
            )
          ),#fluidRow

          fluidRow(
            mobileDetect('isMobile'),
            textOutput('itsAMobile'),
            plotlyOutput("chillPlot")
          )

        ) #fluidPage
      ),
    tabPanel("Growing Degrees", value ='Growing Degrees', busyIndicator("Calculation In Progress",wait = 0),
        fluidPage(
          fluidRow(
             column(width=3,
                    radioButtons("gType", label = h4("Growing Degree"), choices = list("Hours" = 1, "Days" = 2),inline = T,selected = 2)
            )
          ),
          fluidRow(
            plotlyOutput("GDHPlot")
          )
          # ,
          # fluidRow(
          #   plotlyOutput("GDHPlot2")
          # )
        )#fluidPage
    ),
    tabPanel("Temperature/Rainfall", value ='Temperature', busyIndicator("Calculation In Progress",wait = 0),
             fluidPage(
               fluidRow(
                 plotlyOutput("TempPlot")
               ),
               fluidRow(
                 plotlyOutput("RainPlot")
               )
          )
    ),
    tabPanel('Details',
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
      ),
  tabPanel("News",
           includeHTML('News.html')
  )
    ) #tabset

  )#fluidPage
)
