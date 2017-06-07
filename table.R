

######################### tables ################################################
# set rows on 30, until updated
observe({
  updateSliderInput(session, "row1",
                    label = "How many rows?",
                    min=1,
                    max= nrow(include$data),
                    value = 30,
                    step=1)
})

# Put table in container
# include <- reactiveValues(data = NULL) : prepaired in 'load and read'
include$table <-
  DT::renderDataTable(
    if(is.null(include$data))
      return()
    else
      # isolate inputs
      isolate({slice(include$data,1:input$row1)}),
    # allow column selction from client side
    server= FALSE,
    extensions = c('Buttons'),
    selection = list(target = 'column'),
    # bootstrap necessary for shinytheme visibility
    style = 'bootstrap',
    filter = list(position='top',
                  clear = FALSE,
                  plain = TRUE),
    options = list(dom = 'Bfrtip',
                   scrollY = 300,
                   #colReorder = TRUE,
                   scrollX = T,
                   autoWidth = T,
                   buttons = c(I('colvis'),
                               c('copy','csv','excel','pdf','print')),
                   #class = 'table-condensed',
                   pageLength = 100 # make scrolling easier,
    ))
# with data isolated and table in proxy, changes do not cause direct rerendering
proxy1 <- dataTableProxy('table1')

observeEvent(input$showTable1, {
  output$table1<-  include$table 
})

# visualize selected rows and columns 
output$info <- renderPrint({
  req(include$data)
  cat('Selected rows:\n\n')
  cat(input$table1_rows_all, sep = ', ')
  cat('\n\nSelected columns:\n\n')
  cat(input$table1_columns_selected, sep = ', ')
})

####################### tab 2 ##################################################
# 
# observeEvent(input$showTable2, {
#   output$table2<-  include$subtable
# }) 
# # Quantile coloring scheme for formatStyle 
# # start the app with one column selected (and therefore colored) (else:error)
# # clear table
# observeEvent(input$quantile1,{
#   updateCheckboxGroupInput(session, "variable15",
#                            choices = colnames(include$subdata),
#                            inline= input$inline3,
#                            selected = "")
# })
# observe({
#   req(input$variable15) # throws error without
#   req(input$cut2)
#   # get quantiles
#   q<-quantile(as.numeric(include$subdata[,input$variable15]), ######################testen
#               probs = seq(.05, .95, .05), na.rm = TRUE, names = FALSE)
#   # replace quantile-cuts from input$cut2
#   # with actual values depending on variable input
#   # vectorisation is the fastest solution for replacement; unname is necessary!
#   replace_vector<-c(
#          "5%" = q[1], "10%" = q[2],
#          "15%" = q[3], "20%" = q[4],
#          "25%" = q[5], "30%" = q[6],
#          "35%" = q[7], "40%" = q[8],
#          "45%" = q[9], "50%" = q[10],
#          "55%" = q[11], "60%" = q[12],
#          "65%" = q[13], "70%" = q[14],
#          "75%" = q[15], "80%" = q[16],
#          "85%" = q[17], "90%" = q[18],
#          "95%" = q[19])
#     include$cuts <- unname(replace_vector[input$cut2])
#     # get sequence of white to dark orange; length = 20 for all 5% quantiles
#     # (often lighter is higher value, but I prefer darker colors; more=more) 
#     include$oranges<-rgb(colorRamp(c("white", "orangered"))
#                         (seq(0, 1,length = length(include$cuts) + 1)), max = 255)
# })
# 
# #get colnumber:match("b",names(df))
# 
# 
# # Put table with data subset in container
#   include$subtable <-
#    DT::renderDataTable(
#       x<-DT::datatable(
#         # isolate inputs,
#         # so the table does not rerender on every change, until updated
#         isolate({include$subdata}),
#         extensions = c('Buttons'),
#         selection = list(target = 'column'),
#         caption = isolate({input$caption1}),
#         style = 'bootstrap',
#       filter = list(position='top',
#                     clear = FALSE,
#                     plain = TRUE),
#       options = list(dom = 'Bfrtip',
#                      scrollY = 300,
#                      #colReorder = TRUE,
#                      scrollX = T,
#                      autoWidth = T,
#                      buttons = c(I('colvis'),
#                                  c('copy','csv','excel','pdf','print')),
#                      #class = 'table-condensed',
#                      pageLength = 100))# make scrolling easier,
#      
#      
#       # proxy does not work on helperfunctions
#       # table rerenders when updated with actionButton
#       
#       %>% formatCurrency(isolate({input$variable7}), isolate({input$curr1}))
#       %>% formatRound(isolate({input$variable5}), digits= isolate({input$num1}))
#       %>% formatPercentage(isolate({input$variable6}), digits=isolate({input$num2}))
#       %>% formatDate(isolate({input$variable8}), isolate({input$method1}))
#       
#       # highlight, row or column, general or conditionally 
#       %>% formatStyle(columns=isolate({input$variable9}),
#                       valueColumns =isolate({input$variable10}),
#                       target = isolate({input$target1}),
#                       # optional highlighting:
#                       #fontWeight = isolate({input$bold1}),
#                       color = isolate({input$color1}),
#                       backgroundColor = isolate({input$color2}))
#       #vectors of cut-offs and colors - 2 breaks make 3 chunks, so color = +1
#       %>% formatStyle(columns=isolate({input$variable11}),
#                       valueColumns =isolate({input$variable12}),
#                       target = isolate({input$target2}),
#                       backgroundColor = styleInterval(cuts=isolate({input$cut1}),
#                                                       values=isolate({input$color3})))
#       %>% formatStyle(columns=isolate({input$variable13}),
#                       valueColumns =isolate({input$variable14}),
#                       target = isolate({input$target3}),
#                       backgroundColor = styleEqual(levels=isolate({input$level1}),
#                                                    values=isolate({input$color4})))
#       %>% formatStyle(columns=isolate({input$variable15}),
#                       #valueColumns =isolate({input$variable16}),
#                       target = isolate({input$target4}),
#                       color = "black",
#                       backgroundColor =
#                         styleInterval(cuts=isolate({include$cuts}) ,
#                                       values=isolate({include$oranges})
#                                             ))
#       )
#       
#   # Convert variable for custom formatPercentage (only adding sign)
#   observeEvent(input$add1, {
#     req(input$variable6)
#     include$subdata[,input$variable6] <- include$subdata[,input$variable6]/100
#   })
#   
#  
  
#   proxy2 <- dataTableProxy('table2')
   
 # On proxy:
 # when replacing the table with same or smaller subset; colums must be the same,
 # so ranges are the same or smaller. 
 # filtered cases /columns should be subsetted through subsetbutton
   


