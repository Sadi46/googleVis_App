library(googleVis)


# fill checkboxes #############################################################
# dataframe (for scatter etc)
observe({
  # fill checkbox 
  updateSelectInput(session, "dataframe",
                    label = "Dataframe",
                    choices = colnames(include$subdata))
})
# variable names
observe({
  # get numeric variables
  numerics_integers<- as.data.frame(include$subdata) %>% select_if(is.numeric)
  list<-colnames(numerics_integers)
  # fill checkbox (prevent multiple)
  updateSelectInput(session, "variableA",
                     label = "Numeric Variables",
                     choices = c(Choose='', list))
  })
observe({
  updateCheckboxGroupInput(session, "variableB",
                           label = "X-Variables",
                           inline=TRUE,
                           choices = colnames(include$subdata),
                           selected = "")
})
observe({
  updateCheckboxGroupInput(session, "variableC",
                           label = "Y-Variables",
                           inline=TRUE,
                           choices = colnames(include$subdata),
                           selected = "")
})

# make variable input reactive for re-use in multiple charts (instead of
# putting the input in objects in every rendering block)
observe({
  req(input$variableA)
  # column
  include$numdata <- include$subdata[,input$variableA]
  # column name
  include$num <- input$variableA  
})
observe({
  # column
  include$Xdata <- include$subdata[,input$variableB]
  # column name
  include$x <- input$variableB
  # column name
  include$y <- input$variableC
})
observe({
  include$dataframe<-include$subdata[,input$dataframe]
})
                             

# get table in tabsetpanel
output$view1 <- renderGvis({
  req(include$subdata)
  gvisTable(include$subdata,
            options=list(page='enable',
                         cssClassNames = "{headerRow: 'headrow', tableRow: 'tablerow'}",
                         alternatingRowStyle = FALSE,
                         gvis.editor='enable'))  
})


# get histogram and barplots 
observeEvent(input$histogram,{
  #change tabsetpanel
  updateTabsetPanel(session, "chart1", selected="Histogram")
  output$view2 <- renderGvis({
    # control error message on missing input + give hints
    validate(
      need(!is.null(input$variableA),
           'Histogram: Pick one  numeric variable'))
    # prevent error of zero length when return to zero variables
    if(is.null(include$numdata)) return()
    gvisHistogram(as.data.frame(include$numdata),
                  options= list(
                    legend="none",
                    width=400, height=360,
                    #legend="{position: 'top', maxLines: 2 }",
                    title= paste("Histogram", include$num, sep=" "),
                    titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}"
                    )) 
  })
  output$view3 <- renderGvis({
    # control error message on missing input + give hints
    validate(
      need(!is.null(input$variableB) & !is.null(input$variableC),
           'ColumnChart: Pick one x-variable and one or more y-variables'))
    # prevent error when returned to zero variables
    if(is.null(include$x) | is.null(include$y)) return()
    gvisColumnChart(as.data.frame(include$subdata),
                    xvar= include$x,
                    yvar= include$y,
                    options= list(
                      width=400, height=360,
                      title= paste("ColumnChart", include$x, sep=" "),
                      titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}",
                      bar="{groupWidth:'300%'}"))
                    #legend="{position: 'top', maxLines: 2 }",
                                 # hAxis="{title: x_variable}"
                    #             colors="['#5C3292', '#1A8763', '#871B47']",
                                 
                   
  })
  output$view4 <- renderGvis({
    # control error message on missing input + give hints
    validate(
      need(!is.null(input$variableB) & !is.null(input$variableC),
                   'BarChart: Pick one x-variable and one or more y-variables'))
    # prevent error when returned to zero variables
    if(is.null(include$x) | is.null(include$y)) return()
    gvisBarChart(as.data.frame(include$subdata),
                 xvar= as.character(include$x) ,
                 yvar= as.character(include$y),
                 options= list(
                   width=400, height=360,
                   isStacked=TRUE,
                   title=paste("BarChart", include$x, sep=" "),
                   titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}",
                   bar="{groupWidth:'300%'}"))
  })
 })
# get scatter chart
observeEvent(input$scatter,{
  updateTabsetPanel(session, "chart1", selected="Scatter")
  output$view5 <- renderGvis({
    # prevent error message
    validate(need(input$dataframe, 'Choose variables'))
    # scatterchart
    xaxis<- names(include$dataframe[1])
    yaxis<- names(include$dataframe[2])
    gvisScatterChart(include$dataframe,
                     options=list(
                       legend="none",
                       #lineWidth=2, pointSize=0,
                       title="Women",
                       hAxis="{title:'xaxis'}", 
                       vAxis="{title:'yaxis'}",
                       width=300, height=300))
    })
  })

observeEvent(input$motion,{
  updateTabsetPanel(session, "chart1", selected="Motion")
  output$view6 <- renderGvis({
    # motion chart
    data<-include$subdata[,input$dataframe]
    id <- include$x #input$dataframe[1] 
    time <- as.numeric(include$y)
    # xvar<-input$dataframe[3]
    # yvar<-input$dataframe[4]
    # colorvar<-input$dataframe[5]
    # sizevar<-input$dataframe[6]
    # (xvar, yvar,colorvar, sizevar), will be assumed to be the next columns
    gvisMotionChart(as.data.frame(include$subdata), idvar = input$variableB, timevar = input$variableC
                    #, 
                    #xvar = xvar,  yvar = yvar,
                    #colorvar = colorvar, sizevar = sizevar
                    #, #date.format = "%Y/%m/%d",
                    #options = list(), chartid
                    )
    #gvisMotionChart(include$subdata, factor, time)
    })
  })

# observeEvent(input$line,{
#   updateTabsetPanel(session, "chart1", selected="Line")
#   
#   
#   output$view7 <- renderGvis({
#     gvisLineChart(df, xvar=, yvar=c(),
#                   options=list(
#                     series=
#                     vAxes="[{title:'val1'}, {title:'val2'}]"
#                   ))
#   })
# })

# })

