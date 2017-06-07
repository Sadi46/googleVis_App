
###### googleVis does not work outside shiny for security settings in 'flash'###
#(can be changed)

library(googleVis)
tabPanel(
  title = "googleVis",
  wellPanel(
           #br(),
           # change style to fit with dark shiny themes (classnames in server)
           tags$style(type="text/css",
                      HTML("#view1 td{color:white;} .headrow {color:black;} 
                           .headrow {background-color:orange;} 
                           .tablerow {background-color:slategrey;}
                           table { border-collapse: collapse; border-spacing: 0;}")),
           h1(htmlOutput("view1")) 
                      ),
  # Options for selecting variables
  fluidRow(       
    column(2,
           h3("Histogram"),
           selectInput("variableA",
                       label= "Numeric Variables",
                       selectize=TRUE,
                       choices= "")
           ),
    column(4, 
           h3("Columcharts and Barcharts"),
           checkboxGroupInput("variableB",
                              label= "X-Variables",
                              inline= TRUE,
                              choices= list(),
                              selected= ""),
           checkboxGroupInput("variableC",
                              label= "Y-Variables",
                              inline= TRUE,
                              choices= list(),
                              selected= "")),
  column(4,
         h3("eg Scattercharts and Motioncharts need a dataframe: the first variable will be the x-variable"),
         selectInput("dataframe",
                     label= "Choose Variables for a dataframe",
                     selectize=TRUE,
                     multiple=TRUE,
                     choices= "")
  )),
   

    fluidRow(
    h3("Choose Charts"),
    actionButton("histogram", "histogram/bar"),
    actionButton("scatter", "scatter"),
    actionButton("motion", "motion"),
    actionButton("line", "line"),
    actionButton("area", "area"),
    actionButton("combo", "combo"),
    actionButton("candlestick", "candlestick"),
    actionButton("pie", "pie"),
    actionButton("geo", "geo"),
    actionButton("intensity map", "intensity map"),
    actionButton("org", "org"),
    actionButton("tree", "tree"),
    actionButton("annotation", "annotation"),
    actionButton("sankey", "sankey"),
    actionButton("calendar", "calendar"),
    actionButton("timeline", "timeline"),
    actionButton("bubble", "bubble"),
    actionButton("gauge", "gauge")),
    
  fluidRow(
   tabsetPanel(
        id = 'chart1', # should be unique!
        tabPanel('Histogram/Bar', 
                 inline(htmlOutput("view2")),
                 inline(htmlOutput("view3")),
                 inline(htmlOutput("view4"))),
        tabPanel('Scatter', 
                 h4("Scatter chart"),
                 h6("The first variable chosen will be the x-variable,
                    the rest will be y-variables"),
                 htmlOutput("view5")),
        tabPanel('Motion', 
                 h4("Motion chart"),
                 #h6()
                 htmlOutput("view6")),
        tabPanel('Line', 
                 h4("Line chart"),
                 htmlOutput("view7"))
        
             
   )),
                 
                 
  
  
  # fluidRow(column(12,
  #                 div(style = "background-color: turqoise;",
  #                     h3("Coursera example using googleVis"),
  #                     column(9,
  #                     wellPanel(includeMarkdown("googleVis.Rmd"),
  #                               width="50%",
  #                               style = "overflow-y:scroll; max-height: 600px;background-color: honeydew;color:black")
  #                 )) # end tabpanel 
  #                 )),
  fluidRow(
    h6("Portions of this page are reproduced from work created  
       and shared by",
    a("Google ", href="https://developers.google.com/terms/",
      target="_blank"),
    "and used according to terms described in the",
    a("creative commons Attribution 3.0 License",
      href="http://creativecommons.org/licenses/by/3.0/",
      target="_blank"))
    )
)
