#packageVersion('plotly')  
# > shiny::runApp(app, display.mode = "showcase")




###### stuff that needs to be processed only once ##############################
# read datasets from included files:

# airquality; source: coursera JHU course
ozone<-import("hw1_data.csv")

# steps: source: coursera JHU course
# source("steps.md", local=TRUE)
steps<-import("activity.csv")

# severe weather events; source: coursera JHU course
US_severe_weather_events<-import("weather.csv")

# TODO? get datasets from packages (mtcars is from Base R datasets, no need to get):
# get(input$datasetName, "package:datasets", inherits = FALSE) 

#source custom function for inspecting structure of the data
source("peak.R", local=TRUE)$value



############# note ###########
# inputs are numbered per content ==> adapt depending on UI lay-out
# (for simplicity and less parentheses, but modularizing is alternative)

# As the widgets return quoted variable names, the use of them has to be
# like this: dataframe[,"V"] 


shinyServer(function(input, output, session) {
  
  # visualize session info # why in console?
  output$intel<-renderText({
    str(session)
  })
  #display.mode = "showcase"
  #make sure session stops when browser is closed
  session$onSessionEnded(stopApp)
  # allow reconnect after greying out
  session$allowReconnect(TRUE)
  # prevent a fading out grey screen: 
  tags$head(tags$style(type="text/css",
                       "body.disconnected {
                       background-color: inherit;
                       opacity: 1;
}"
))
  
  
  ######################## set locale ##########################################
  # change locale for date analysis and English words in dates
  # first store actual locale in object to be able to reverse the proces later
  observeEvent(input$setLocale,{
    timeLocale<-Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
  })
  ######################### reset locale if changed before #####################
  observeEvent(input$resetEnd, {
    req(input$setLocale)
    Sys.setlocale("LC_TIME", timeLocale)
  })
  
  #### get code for loading and reading ########################################
  # run R file from the project folder
  source("load_and_read.R", local=TRUE)$value
  
  
  #### fill checkboxGroups / source ############################################
  # run R file from the project folder
  source("updateCheckboxGroupInput.R", local=TRUE)$value
  
  
  #### make smaller subset #####################################################
  observeEvent(input$subset1, {
    if (!is.null(input$variable1))
      return(include$data <- 
               include$data %>% 
               select(one_of(input$variable1)))
    if (!is.null(input$variable1b))
      return(include$data <- 
               include$data %>% 
               select(one_of(input$variable1b)))
  })
  
  ###################### convert variables #####################################
  
  #change classes on demand; updating include$data
  observeEvent(input$convert1,{
    # prevent error in case the button is clicked without reactive data
    if (is.null(include$data)) return()
    # make classes for either chosen checkboxgroup or selected columns in table
    colNumbers<- input$table1_columns_selected
    colNames<- input$variable1
    variables<- input$variable1
    if (!is.null(colNumbers))
      return (include$data <-
                include$data %>%  
                mutate_at(.cols=colNumbers, .funs=input$class1))
    if (!is.null(colNames))
      return (include$data <-
                include$data %>%  
                mutate_at(.cols=colNames, .funs=input$class1))
  })
  
  # # convert dates TODO: test (else works)
  observeEvent(input$convert2,{
    column<- include$data[,input$variable1]
    colNames<- input$variable1
    Count <- input$class2
    if (Count == "days since 1900-01-01"){
      column <- as.Date(column, origin = "1900-01-01")}
    if (Count == "Mac Excel"){
      column <- as.Date(column, origin = "1904-01-01")}
    if (Count == "Windows Excel"){
      column <- as.Date(column, origin = "1899-12-30")}
    if (Count == "Matlab"){
      column <- as.Date(column, origin = "1970-01-01") - 719529}
    # library(lubridate) can even handle: c(20090101, "2009-01-02", "2009 01 03",
    # "2009-1-4", "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
    else {include$data <- include$data %>% mutate_at(.cols=colNames, .funs=Count)} 
    })
  
  #Force Date class if year or day is missing
  observeEvent(input$convert4,{
    req(input$variable1)
    column<- include$data[,input$variable1]
    if(input$mode3=="add day 1")
      include$data[,input$variable1]<-as.Date(paste(column,"01", sep="/"))
    if(input$mode3=="add year 2000")
      include$data[,input$variable1]<-as.Date(paste("2000", column, sep="/"))
    if(input$mode3=="add day and year")
      include$data[,input$variable1]<-as.Date(paste("2000", column,"01", sep="/"))
  })
  
  
  ################## aggregate ###################################################
  # aggregate(cbind(airquality[,"Ozone"] ,airquality[,"Wind"]) ~ Month, airquality, mean)
  # works, but changes variable names in V1 etc
  # dplyr makes logical names, but needs NA removal if present:
  
  # fill input boxes 
  observe({
    updateSelectInput(session, "group",
                      label = "Group by",
                      choices = c(Choose='', colnames(include$data))) 
  })
  
  # make basic function to get correct column name for rounded mean or sd column
  roundedMean <- function(x)
    round(mean(x, na.rm = T),digits=3)
  roundedSd <- function(x)
    round(sd(x, na.rm = T),digits=3)
  
  # fill input boxes
  observe({
    updateSelectInput(session, "summary1",
                      label = "Summary",
                      choices = c("min(.,na.rm = T)",
                                  "max(.,na.rm=T)",
                                  "roundedMean(.)", 
                                  "roundedSd(.)",
                                  "sum(.,na.rm=T)",
                                  "quantile(.,probs=0.05, na.rm=T)",
                                  "quantile(.,probs=0.25, na.rm=T)",
                                  "median(.,na.rm=T)",
                                  "quantile(.,probs=0.75, na.rm=T)",
                                  "quantile(.,probs=0.95, na.rm=T)" ))
  })
  
  # fill input boxes
  observe({
    updateSelectInput(session, "cols",
                      label = "Columns",
                      choices = colnames(include$data)) 
  })
  # get tabe
  observeEvent(input$summary2,{
    include$summary <- 
      include$data %>% 
      group_by(.[,input$group]) %>% 
      summarise_at(.funs= input$summary1, .cols= input$cols) 
    output$summary3<-DT::renderDataTable(
      include$summary,
      style = 'bootstrap',
      options = list(colReorder = TRUE,
                     scrollX = T,
                     autoWidth = T)
      
    )
  })
  # join summaries with original dataframe
  # first change grouping column in same name as grouping variable 
  # (TODO: rename_at in later dplyr version)
  observeEvent(input$join,{
    colnames(include$summary)[1] <- input$group
    include$data<-full_join(include$data, include$summary, by=input$group)
  })
  # 
  # # regroup 
  #     include$summary<-
  #       include$summary %>% 
  #       # somehow it does not work with column numbers (works in console)
  #       gather_(key=summary, value=value, include$summary[,-1])
  
  
  
  ################### change names #############################################
  observeEvent(input$data8, {
    req(input$names1)
    colnames(include$data)[names(include$data) == input$variable1] <-
      input$names1
  })
  
  # delete rows with table title
  observeEvent(input$minus_row, {
    include$data<-
      include$data %>% slice(-1)
  })
  
  # unite 
  observeEvent(input$convert3,{
    columns<-input$variable1
    if(input$mode2 == "Unite"){
      include$data<-
        include$data %>% 
        unite_(col=input$name1, from=columns, sep=input$sep1)}
    if(input$mode2 == "Reshape"){
      include$data<-
        include$data %>% 
        # somehow it does not work with column numbers (works in console)
        gather_(key=input$name1b, value=input$name1c, columns)}
  })
  
  # split mixed columns
  observeEvent(input$convert4,{          
    include$data<-
      include$data %>% separate(col=input$variable1,
                                into=c(input$name2))
  })
  
  # reorder columns
  # with dplyr; negatives to end and positives to front
  observe({
    req(include$data)
    updateSliderInput(session,"number", label = "",
                     min = -length(include$data),
                     max = length(include$data),
                     step = 1,
                     value = 0)
  })
  observeEvent(input$reorder,{ 
      include$data <-
        include$data %>% select(input$number, everything())
  })
    
  
  #check percentage missing values
  observeEvent(input$NA1, { 
    output$lines2 <- renderPrint({
      if (!is.null(include$data))
        return(round(colMeans(is.na(include$data))*100, digits = 1))
    })
  })
  
  # fix alternative missing values other than NA with a 'closure'
  # note: (functions that make and return functions; 
  # Closures allow to make functions based on a template), 
  # df[]returns dataframe in stead of a list from lapply
  
  # also possible here: fix_missing <- function(x, na.value) {
  # x[x == na.value] <- NA
  # x
  # }
  observeEvent(input$NA3, {
    missingValue <-input$NA2
    fix_missing <- function(missingValue) {
      function(x) {
        x[x == missingValue] <- NA
        x }}
    include$data[input$variable1] <- 
      lapply(include$data[input$variable1], fix_missing(missingValue))
  })
  
  # add row column 
  observeEvent(input$add,{
    include$data <- rownames_to_column(include$data, "row")
  })
  
  
  # add column with incomplete cases
  observeEvent(input$NA4,{
    if (!is.null(include$data))
      return (include$data <-
                include$data %>% 
                mutate(missing = ifelse(complete.cases(.), yes="0", no="1")))
  })
  
  #check zeroes
  observeEvent(input$null1,{
    output$lines3 <- renderPrint({
      if (!is.null(include$data))
        round(colMeans(include$data==0)*100, digits=1)
    })
  })
  
 
  ######## make subset for further analysis on page 2: $subdata ################
  observeEvent(input$subset2, {
    if (!is.null(input$variable1))
      return(include$subdata <- 
               include$data %>% 
               select(one_of(input$variable1)))
    if (!is.null(input$variable1b))
      return(include$subdata<-
               include$data %>% 
               select(one_of(input$variable1b)))
    else include$subdata <- include$filter
  })
  ################## switch tabs ###############################################
  observeEvent(input$link1, {
    updateTabsetPanel(session, "Tabset1", selected="googleVis")
  })
  observeEvent(input$link2, {
    updateTabsetPanel(session, "Tabset1", selected="steps_markdown")
  })
  observeEvent(input$link3, {
    updateTabsetPanel(session, "Tabset1", selected="ozone_markdown")
  })
  observeEvent(input$link4, {
    updateTabsetPanel(session, "Tabset1", selected="weather_markdown")
  })
  observeEvent(input$link5, {
    updateTabsetPanel(session, "Tabset1", selected="Links")
  })
  # observeEvent(input$link6, {
  #   updateTabsetPanel(session, "Tabset1", selected="Load, Read, Convert and Subset")
  # })
  # observeEvent(input$link7, {
  #   updateTabsetPanel(session, "Tabset1", selected="Plotly")
  # })
  # observeEvent(input$link8, {
  #   updateTabsetPanel(session, "Tabset1", selected="Plotly2")
  # })
  # observeEvent(input$link9, {
  #   updateTabsetPanel(session, "Tabset1", selected="Load, Read, Convert and Subset")
  # })
  # observeEvent(input$link10, {
  #   updateTabsetPanel(session, "Tabset1", selected="Plotly2")
  # })
  # 
  
  ################ TABLE ##################
  # important: a dataset with rownames, has rownames = column 0 in datatable!
  # JavaScript indexes from 0 instead of 1, 
  # so the index of the n-th element is actually n - 1 
  # (so n=n when rownames are added ).
  # table will be rerendered on bigger set, because the ranges can change
  # some other changes can be done through proxy or actionbutton,
  # without rerendering immediately
  
  ################### show table from source ###################################
  # run R file from the project folder
  source("table.R", local=TRUE)$value
  
  
  ################### subset cases #############################################
  # subset table
  observeEvent(input$subset3,{
    filtered_data1 <- input$table1_rows_all
    include$data <- include$data[filtered_data1,] 
  })
  
  
 
  ############# show table and plot options from source
  # run R file from the project folder
  source("googleVis_server.R", local=TRUE)$value
  })

