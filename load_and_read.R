
##### server.R ##########

# prepare list for reactive values (class = function)
# for re-usable 'template code'
include <- reactiveValues(data = NULL)
# also for controlling progress
include$setupComplete <- FALSE ##??TODO


# put data in list of reactive values, 'include'
observeEvent(input$data1, {
  include$data <- mtcars
  # clear subdata in case of change of dataset
  include$subdata <- NULL
  # make sure names are properly formatted
  names(include$data)<-make.names(names(include$data)) 
})

observeEvent(input$data2, {
  include$data <- ozone
  # clear subdata in case of change of dataset
  include$subdata <- NULL
  # make sure names are properly formatted
  names(include$data)<-make.names(names(include$data)) 
})
observeEvent(input$data3, {
  include$data <- steps
  # clear subdata in case of change of dataset
  include$subdata <- NULL
  # make sure names are properly formatted
  names(include$data)<-make.names(names(include$data)) 
})
observeEvent(input$data4, {
  include$data <- US_severe_weather_events
  # clear subdata in case of change of dataset
  include$subdata <- NULL
  # make sure names are properly formatted
  names(include$data)<-make.names(names(include$data)) 
})

##### peak: show structure for dataset, subset and url-data ####################


# observeEvent(input$peakUrl,{
#   include$data <- NULL
# })
# show with progress bar 
output$lines1 <- renderPrint({
  withProgress(message = 'Reading data', value=1,{
    incProgress(1/10,detail = "Please wait")
    Sys.sleep(0.1)
    if (!is.null(include$data))
      return (include$data %>% peak())
    if (!is.null(include$subdata))
      return (include$subdata %>% peak())
    else{
      validate(need(input$path2, 'Get a dataset or copy and paste a url first'))
      # delay action
      req(input$peakUrl)
      Url <- input$path2
      include$peak<- peak(Url)
      include$peak
    }
  })
})
observeEvent(input$peakUrl,{
  # clear the (old) data 
  include$data <- NULL
})

############## download; choose file and update path
observeEvent(input$browse1,{
  # function to escape the file
  # thanks to Henrik Bengtsson; 
  # https://stat.ethz.ch/pipermail/r-help/2007-June/133564.html
  file.choose2 <- function(...) {
    pathname <- NULL
    tryCatch({pathname <- file.choose()},
             error = function(ex) { })
    pathname
  }
  updateTextInput(session, "path1",  value = file.choose2())
})
# put data in list of reactive values, 'include'
observeEvent(input$import1, {
  head<-input$header1 # TRUE or FALSE
  dataset<- import(input$path1, header=head)
  include$data <- dataset
  # clear subdata in case of change of dataset
  include$subdata <- NULL
  # make sure names are properly formatted
  names(include$data)<-make.names(names(include$data)) 
})



#### make datasets from a url reactive ##################################

# although import() can be very slow, 
# it is the simplest way and works for almost "all" extensions
# else:
# https://www.datacamp.com/community/tutorials/importing-data-r-part-two#gs.4Ci45kg
# example url
# source: https://datahub.io/dataset/open-pubs
# data: https://datahub.io/dataset/82954561-ba95-4984-82de-1121c1217c9b/resource/6a7281b9-1aaf-488b-bd9f-4e6a9a604745/download/openpubs.csv


# fill selectInput for column selection / duplicates not possible with selectize
observe({
  req(include$peak)
  # get classes for every column in the dataset    
  # prevent duplicates by pasting V1,V2 etc to classes
  # peak output (factor) for Url's: Datastructure[1]= accessDate, [2]= filesize,
  # [3]= filetype, [4]= miss, [5]= columns, [6:end]= classes 
  
  cols<- as.character(include$peak$Datastructure[5])
  vars<- paste0("V", 1:cols)
  classes<- c(as.character(include$peak$Datastructure[6:nrow(include$peak)]))
  none<- c(paste0("N",1:length(classes),"=","NULL"))
  variables<- paste(vars, classes, sep="=")
  # add security in case there is no header()
  updateSelectInput(session, "select",
                    label = "Select columns",
                    # list of ordered classes in the data + NULL options
                    choices = c(variables, none))
  # as class names start at fourth position ("V1=character", "N2=NULL"),
  # they are easy to extract: substr(string1, start=4, stop=nchar(string1))
})


# read some data from url
observeEvent(input$import2, {
  # prevent error if button is clicked without url
  req(input$path2) 
  # or: validate(need(input$path2, 'Copy and Paste a url first')) 
  withProgress(message = 'Reading data', value=1,{
    incProgress(1/10, detail = "Please wait")
    Sys.sleep(0.1)
    N<-input$row2
    Url <- input$path2
    head<-input$header2
    # extract classes from input$select
    colclasses <- substr(input$select, start=4, stop=nchar(input$select))
    data<-import(Url, header=head, nrows=N, colClasses= colclasses)
    include$data<-data
    # clear subdata in case of change of dataset
    include$subdata <- NULL
    # make sure names are properly formatted
    names(include$data)<-make.names(names(include$data))
  })
})
# read all data from url 
observeEvent(input$import3, {
  # prevent error if button is clicked without url
  req(input$path2) 
  # show progress
  withProgress(message = 'Reading data', value=1,{
    incProgress(1/10, detail = "Please wait")
    Sys.sleep(0.1)
    Url <- input$path2
    head<-input$header2
    # extract classes from input$select
    colclasses <- substr(input$select, start=4, stop=nchar(input$select))
    newdata<-import(Url, header=head, colClasses = colclasses)
    include$data<-data
    # clear subdata in case of change of dataset
    include$subdata <- NULL
    # make sure names are properly formatted
    names(include$data)<-make.names(names(include$data))
    # write to file to re-use through browse without downloading again
    write.csv(newdata, file = "download.csv", row.names = FALSE)
  })
})

 
  
