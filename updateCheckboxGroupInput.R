
######################## widgets conditioned on variable datasets ##############

# Fill the empty checkbox with column names;
# condition = "Choose"
observe({
  updateCheckboxGroupInput(session, "variable1",
                           label = "Variables",
                           choices = colnames(include$data),
                           #true or false:
                           inline= TRUE,
                           selected = "")
})
# Fill the empty checkbox;
# condition = "All"
observe({
  updateCheckboxGroupInput(session, "variable1b",
                           label = "All_Variables",
                           choices = colnames(include$data),
                           #true or false:
                           inline= TRUE,
                           selected = colnames(include$data))
})



# clear input (clear other output reactively on is.null(include$data))
observeEvent(input$reset, {
  include$data <- NULL    
  updateCheckboxGroupInput(session, "variable1",
                           label = "Variables",
                           choices = "",
                           selected = "")
  updateCheckboxGroupInput(session, "variable1b",
                           label = "All_Variables",
                           choices = "",
                           selected = "")
})


