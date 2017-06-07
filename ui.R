
############### main app frame #################################################

# icloud?

# shorthand for inline outlook
inline <- function(x){
  tags$div(style="display:inline-block;", x)
}

# make sure the debugging tools for javascript work on Safari 
# (for Windows already integrated in RStudio)
# turn them on using this command in Terminal and restart RStudio:
# defaults write org.rstudio.RStudio WebKitDeveloperExtras -bool true
library(shiny)
library(shinythemes)
library(grDevices)
library(rio)
library(tidyverse) #incl dplyr, ggplot2, lubridate..
library(plotly)
library(DT)
library(markdown)
#install.packages(c("shiny",), lib= ) => make lib in www?


shinyUI(
  navbarPage(
    title = "Visual Data",
    position = "fixed-top",
    collapsible = TRUE,
    shinythemes::themeSelector(),
    theme = shinytheme("superhero"),
    tags$header(tags$img(src = 'Tibet_small.jpg', width = '100%',
                         tags$footer(tags$small('Tibet 2012')))),
    # give validation messages more contrast
    tags$head(
      tags$style(HTML(".shiny-output-error-validation {color: blue;}"))),
    
    tabsetPanel(
      id = "Tabset1",
      
      ###### tab 1 ###################################################
      tabPanel(
        "Load, Read, Convert and Subset", 
        sidebarLayout( 
          #option: position = "right",
          sidebarPanel(
            width=3,
            # show input options on condition
            h3("Load data"),
            radioButtons(inputId="mode",
                         label="Origin",
                         inline=TRUE,
                         choices=list("Existing Dataset", "Browse", "Download"),
                         selected="Existing Dataset"), 
            conditionalPanel(
              condition = "input.mode == 'Existing Dataset'",
              h4("Included datasets"),
              actionButton("data1", "mtcars", icon("download"),
                           style = "border-color: cadetblue"),
              actionButton("data2", "ozone", icon("download"),
                           style = "border-color: cadetblue"), 
              actionButton("data3", "steps", icon("download"),
                           style = "border-color: cadetblue"),
              actionButton("data4", "weather", icon("download"),
                           style = "border-color: cadetblue"), 
              hr()), 
            conditionalPanel(
              condition = "input.mode == 'Browse'",
              h4("Load files"),
              actionButton("browse1", "Browse", icon("search"), 
                           style = "border-color: darkgrey"),
              br(),
              br(),
              textInput("path1", "File:"),
                 #style = "background-color:slategrey"),
                        #style = "background-color:slategrey"),
              h5("uncheck if header=FALSE"),
              checkboxInput('header1', 'Header', TRUE),
              actionButton("import1", "Load Data", icon("download"),
                           style = "border-color: cadetblue"),
              hr()),
            conditionalPanel(
              condition = "input.mode == 'Download'",
              h4("Download files"),
              # example of data without header
              # https://datahub.io/dataset/82954561-ba95-4984-82de-1121c1217c9b/resource/6a7281b9-1aaf-488b-bd9f-4e6a9a604745/download/openpubs.csv
              textInput("path2", "Url:"),
              h5("Peak at 30 rows of external webdata"),
              actionButton("peakUrl", "Peak", icon("binoculars"),
                           style = "border-color: darkgrey" ),
              br(),
              br(),
              h5("ColClasses"),
              h6("Select columns by picking classes (order from 'peak')",br(),
                 "choosing 'NULL' for those columns that should be left out"),
              selectInput('select', 'Select columns', 
                          choices= list(), 
                          multiple = TRUE,
                          selectize= TRUE),
              sliderInput("row2", "How many observations?",
                          min=1,
                          max=1000,
                          value=100,
                          step=1, 
                          round=TRUE),
              h5("uncheck if header=FALSE"),
              checkboxInput('header2', 'Header', TRUE),
              actionButton("import2", "Import nrows", icon("cloud-download"),
                           style = "border-color: cadetblue"),
              actionButton("import3", "Import all", icon("cloud-download"),
                           style = "border-color: cadetblue"),
              br(),
              h5("If necessary,",br(),"delete rows that contain table title"),
              actionButton("minus_row", "Delete first row",
                           style = "border-color:orange"),
              hr()),
            h3("Tidy data"),
            h5("Choose variables and choose conversion"), 
            # Show input options under condition
            radioButtons(inputId="mode2",
                         label="",
                         inline=TRUE,
                         choices=list("Unite", "Separate", "Reshape"),
                         selected=""), 
            conditionalPanel(
              condition = "input.mode2 == 'Unite'",
              h5("Choose two or more variables from the main bar", br(),
                 "write down a new columnname and choose a separator"),
              inline(textInput("name1","New factor-column name")),
              radioButtons("sep1",
                           label="Separator",
                           inline=TRUE,
                           choices= c("None"="",
                                      '-',
                                      '.',
                                      '/',
                                      ','),
                           selected="/"),
              br()),
            conditionalPanel(
              condition = "input.mode2 == 'Reshape'",
              h5("Choose two or more variables from the main bar"), br(),
              h5("Write down key and value name"), br(),
              h5("Convert into longer format"),
              inline(textInput("name1b","New key-column name")),
              inline(textInput("name1c","New value-column name")),
              br()),
            conditionalPanel(
              condition = "input.mode2 == 'Separate'",
              h5("Choose a variable",br(),"and write down new names",br(),
                 "(comma separated)"),
              inline(textInput("name2","New column names")),
              br()),
            actionButton("convert3", "Make new variable(s)",
                         style = "border-color:orange"),
            hr(),
            h3("Classes"),
            h5("Click on table columns or",br(), 
               "Choose variables from the checkboxgroup when dataset is large"), 
            h5("Only select 'as.Date',",
              br(),
              "if structure = year-month-day",
              br(), 
              "or year/month/day"),
            selectInput("class1", 
                        label="Classes",
                        choices=c("as.character",
                                  "as.numeric",
                                  "as.integer",
                                  "as.factor",
                                  "as.ts",
                                  "as.Date"),
                        selected="as.factor",
                        width="50%"),
            actionButton("convert1", "Convert",
                         style = "border-color:orange"),
            hr(),
            h3("Other Dates"), 
            h5("works only with checkboxgroup"),
            h5("Select Date origin if it is a count (1904 for Mac Excel,
                1899 for Windows Excel, 1970 minus 719529 days for Matlab)",
               br(),
               "If not, choose Date structure 'as is'"),
            selectInput("class2",
                        label="Dates",
                        choices=c("days since 1900-01-01",
                                  "Mac Excel",
                                  "Windows Excel",
                                  "Matlab",
                                  "ydm",
                                  "ymd",
                                  "mdy",
                                  "dmy"),
                        selected=""),

            actionButton("convert2", "Convert Dates",
                         style = "border-color:orange"),
            hr(),
            h3("Force as.Date"),
            h5("works only with checkboxgroup"),
            radioButtons(inputId="mode3",
                         label="",
                         inline=TRUE,
                         choices=list("add day 1",
                                      "add year 2000", 
                                      "add day and year"),
                         selected=""), 
            actionButton("convert4", "Convert",
                         style = "border-color:orange"),
            hr(),
            h3("Aggregate"),
            selectInput("group",
                        label= "Group by",
                        selectize=TRUE,
                        choices= "",
                        width = "50%"),
            selectInput("summary1",
                        label= "Summary",
                        selectize=TRUE,
                        multiple=TRUE,
                        choices= "",
                        width = "50%"),
            selectInput("cols",
                        label= "Columns",
                        selectize=TRUE,
                        multiple=TRUE,
                        choices= "",
                        width = "50%"),
            actionButton("summary2","Summary",
                         style = "border-color:orange"),
            actionButton("join","Join Tables",
                         style = "border-color:orange"),
            hr(),
            h3("Move column"),
            h5("negative number moves column position to end,
               positive number moves column position to beginning"),
            sliderInput("number", label = "",
                        min = -10,
                        max = 10,
                        step = 1,
                        value = 0),
            actionButton("reorder", "Move column",
                         style = "border-color:orange"),
            hr(),
            actionButton("add","Add Column for row",
                         style = "border-color:orange"),
            hr(),
            h3("Names"),
            inline(textInput("names1", "Alternative column name")),
            actionButton("data8", "Change name",
                         style = "border-color:orange"),
            hr(),
            h3("Convert alternative missing values into NA"),
            h5("(e.g. 99)"), 
            h5("Select a column and",
               br(),"type the value to be changed change to NA",
               br(),"(only one at a time)"), 
            #div(style="display:inline-block",
            inline(textInput("NA2", "Alternative missing values")),
            actionButton("NA3", "Replace with NA",
                         style = "border-color:orange"),
            hr(),
            h4("Add a column with (in)complete cases"),
            actionButton("NA4", "(in)complete cases column",
                         style = "border-color:orange"),
            hr(),
            h5("Locale for Datetime can be set to 'C'"),
            h5("Reset will return original locale"),
            actionButton("setLocale", "Set Locale", icon("clock-o"),
                         style = "border-color:chartreuse"),
            actionButton("resetEnd", "Reset Locale", icon("clock-o"),
                         style = "border-color:chartreuse"),
            hr(),
            h4("Final subset for further analysis"),
            actionButton("subset2", "Subdata",
                         class = "btn-primary"),
            br(),
            br(),
            actionButton("link1", "googleVis", icon("hand-o-right"),
                         style = "border-color:chartreuse"),
            # actionButton("link2", "steps_markdown", icon("hand-o-right"),
            #              style = "border-color:chartreuse"),
            # actionButton("link3", "ozone_markdown", icon("hand-o-right"),
            #              style = "border-color:chartreuse"),
            actionButton("link4", "weather_markdown", icon("hand-o-right"),
                         style = "border-color:chartreuse"),
            actionButton("link5", "Links", icon("hand-o-right"),
                         style = "border-color:chartreuse"),
            hr()
            ), # end sidebarpanel
          #-------------------------------------------------
          mainPanel(
            hr(),
            h3("Variables"),
            # progress bar
            tags$style(
              type="text/css",
              "#shiny-notification-panel {left: 70%; top: 25%; height:150%}"),
            # make collapsable output panel
            absolutePanel(
              id = "controls", "Collapse or Drag",
              #class = "panel panel-danger",
              class="btn btn-link",
              fixed = F,
              #style = "bootstrap",
              draggable = TRUE,
              top = 100, left = 400, right = "auto",  bottom = "100%",
              width = "auto", height = "auto", 
              HTML('<button data-toggle= "collapse" data-target="#peak">Peak </button>'),
              tags$div(id = 'peak',  class="collapse in",
                       verbatimTextOutput('lines1'))),
            
            # show (un)checked variables under click-condition
            radioButtons(inputId="mode1",
                         label="Variablenames",
                         inline=TRUE,
                         choices=list("All", "Choose"),
                         selected="All"), 
            conditionalPanel(
              condition = "input.mode1 == 'Choose'",
              # empty checkbox to be filled
              checkboxGroupInput("variable1",
                                 label="Variables",
                                 inline= TRUE,
                                 choices= list(),
                                 selected="")),
            conditionalPanel(
              condition = "input.mode1 == 'All'",
              # empty checkbox to be filled
              checkboxGroupInput("variable1b",
                                 label="All_Variables",
                                 inline= TRUE,
                                 choices= list(),
                                 selected="")),
            actionButton("subset1", "Subset", class = "btn-primary"),
            hr(),
            h3("View data"),
            actionButton("showTable1", "Show or Update Table",
                         style = "border-color:chartreuse"),
            h6("(Selected columns can be converted in the sidebar)"),
            sliderInput("row1", "How many rows?",
                        min=1,
                        max=30,
                        value=30, 
                        step=1, 
                        round=TRUE),
            br(),
            DT::dataTableOutput("table1", width = "100%"),
            # check selection effects
            #verbatimTextOutput('info'),
            hr(),
            h3("Subset rows"),
            h5("The column filters show subsets only in the table.",
               br(), "To subset cases in the dataset, set the slider to max,  
               filter and use actionButton"),
            actionButton("subset3", "Subset cases", class = "btn-primary"),
            br(),
            hr(),
            DT::dataTableOutput("summary3"),
            uiOutput("summary4"),
            h4("Missing values"),
            actionButton("NA1", "check percentage missing",
            style = "border-color:chartreuse"),
            verbatimTextOutput("lines2"), br(),
            h4("Percentage of zeroes"),
            actionButton("null1", "check percentage zeroes",
                         style = "border-color:chartreuse"),
            verbatimTextOutput("lines3"),
            hr()
            ) # end mainpanel
          ) # end sidebarlayout
        ), # end tabpanel
      
      ###### tab 2 #####
      # run R file from the project folder
      source("googleVis_ui.R", local=TRUE)$value
      , # end tabpanel in R.file
      
      
      ###### tab 3 #####
      tabPanel(
        title = "Links",
      # run R file from the project folder
      source("links.R", local=TRUE)$value,
      hr()
      ),
                
                
                # ##############################################################
                tabPanel(
                  title = "Session info",
                  verbatimTextOutput('intel')
                  
                ) # tabpanel
                ) # tabsetpanel
                ) # end navbarPage 
    ) # shinyUI




# ##############################################################################
#           navbarMenu(title="menu", "Code", icon = NULL),

