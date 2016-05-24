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

ngramFreq<-list()
for(j in 1:4)
{
  ngramFreq[[j]]<-fread(unzip(paste0(j, "Cn1gram.zip")))
  setkey(ngramFreq[[j]], token)
} 


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  v <- reactiveValues(data = NULL)
  
  output$tableKneserOutput <- DT::renderDataTable(DT::datatable({
    HighestPkn(v$data)
  }))

  output$tableNaiveOutput <- DT::renderDataTable(DT::datatable({
    Naive(v$data)
  }))
  
  observeEvent(input$tokens, {
    v$data<-input$tokens
  })
  
  observeEvent(input$goButton, {
    v$data<-paste0(input$tokens, " ")
  })

})
