## This file is where the server functionality is implemented.
source("linearFitModel.R")
library(shiny)
shinyServer(
  function(input, output)
  {
    output$plotResults <- renderPlot({linearFitModel(input$summarySelection, input$predictorSelectionInput, input$groupDataByInput,
                                                     input$conditionalQueryInput, input$amInput, input$vsInput)})
  }
)