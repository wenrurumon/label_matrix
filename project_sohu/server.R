library(shiny)
library(data.table)
library(dplyr)
library(dtplyr)
setwd('C:\\Users\\WenluluSens\\Documents\\Project\\SohuVedio\\dmp')
load('dmp_demo.rda')

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

  output$euc.input <- renderPrint({
    x.uid <- unique(euc2_demo$uid)[input$i]
    x.input <- as.data.table(select(filter(euc2_demo,uid==x.uid),uid,cid,score=score2))
    x.dmp <- dmp(euc.input=x.input,label.input=lab2)
    x.dmp$euc.input
  })

  output$label.output <- renderTable({
    x.uid <- unique(euc2_demo$uid)[input$i]
    x.input <- as.data.table(select(filter(euc2_demo,uid==x.uid),uid,cid,score=score2))
    x.dmp <- dmp(euc.input=x.input,label.input=lab2)
    x.dmp$label.output
  })

  output$bidding.output <- renderTable({
    x.uid <- unique(euc2_demo$uid)[input$i]
    x.input <- as.data.table(select(filter(euc2_demo,uid==x.uid),uid,cid,score=score2))
    x.dmp <- dmp(euc.input=x.input,label.input=lab2)
    x.dmp$bidding.output
  })
})