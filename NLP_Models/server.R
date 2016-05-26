#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(stringi)
library(tm)
library(qdap)
library(ggplot2)
library(RWeka)
library(data.table)
source("kneter.R")
source("Common.R")
source("Naive.R")

ngramFreq<-list()
for(j in 1:4)
{
  ngramFreq[[j]]<-fread(unzip(paste0(j, "Cn1gram.zip")))
  setkey(ngramFreq[[j]], token)
} 

gtList<-list()
for(j in 1:4)
{
  gt<-ngramFreq[[j]] [, .(Nr=.N), by=N][order(N)]
  gtList[[j]] <- data.table( c=gt[1:(.N-1), N], ca=(gt[2:.N, Nr] / gt[1:(.N-1), Nr] *  gt[1:(.N-1), N]) )
  gtList[[j]] <- rbind(gtList[[j]], data.table(c=gt[.N, N], ca=gt[.N, N]))
  setkey( gtList[[j]], "c")
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  v <- reactiveValues(data = NULL)
  
  output$tableOutput <- DT::renderDataTable(DT::datatable({
    #ngramFreq[[1]]
    data.table()
  }))
  
  
  output$tableKneserOutput <- DT::renderDataTable(DT::datatable({
    HighestPkn(ngramFreq, v$data)
  }))

  output$tableNaiveOutput <- DT::renderDataTable(DT::datatable({
    Naive(ngramFreq, gtList, v$data)
  }))
  
  observeEvent(input$tokens, {
    v$data<-input$tokens
  })
  
  observeEvent(input$goButton, {
    v$data<-paste0(input$tokens, " ")
  })

})
