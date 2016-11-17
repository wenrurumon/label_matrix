library(shiny)
library(bit64)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(

  # Application title.
  headerPanel("DMP Demo"),

  sidebarPanel(
      sliderInput("i", "we have 10000 samples in the testdataset for demo:",
        min=1,max=10000,value=5000)
  ),

  mainPanel(
    h4("Input Data"),
    verbatimTextOutput("euc.input"),

    h4("Label Output"),
    tableOutput("label.output"),

    h4("Bidding Output"),
    tableOutput("bidding.output")
  )
))