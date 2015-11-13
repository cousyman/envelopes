# server.R
library(gmailr)
library(ReporteRs)
library(stringr)


gmail_auth('data/silisales1.json')

source('data/envelopes.R')
source('data/envelopes2.R')



shinyServer(
  function(input, output) {
    
    datasetInput <- reactive({
      test <- as.character(input$dates)
      if(substr(test,9,9)=='0'){
        test2 <- paste(substr(test,6,7),substr(test,10,10),sep='/')
      }
      else{
      test2 <- paste(substr(test,6,7),substr(test,9,10),sep='/')
      }
      test3 <- paste('petkey',test2,sep=' ')
      envelopes(test3)
    })
    datasetInput2 <- reactive({
      test <- as.character(input$dates)
      if(substr(test,9,9)=='0'){
        test2 <- paste(substr(test,6,7),substr(test,10,10),sep='/')
      }
      else{
        test2 <- paste(substr(test,6,7),substr(test,9,10),sep='/')
      }
      test3 <- paste('petkey',test2,sep=' ')
      envelopes2(test3)
    })
    
    output$table <- renderTable({
      datasetInput()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {paste(input$dates,'envelopes.docx',sep=' ')},
      content = function(file){
        writeDoc(datasetInput2(),file)
      }
      
    )

  }
    )