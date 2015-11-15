# server.R
library(gmailr)
library(ReporteRs)
library(stringr)


gmail_auth('data/silisales1.json')

source('data/envelopes.R')

#Create Server
shinyServer(
  function(input, output) {
    
    datasetInput <- reactive({
      test <- as.character(input$dates)
      month <- gsub('-','',gsub('-0','',substr(test,5,7)))
      day <- gsub('-','',gsub('-0','',substr(test,8,10)))
      test <- paste(month, day,substr(test,1,4),sep='/')
      test3 <- paste('petkey',test,sep=' ')
      envelopes(test3)[[1]]
    })
    datasetInput2 <- reactive({
      test <- as.character(input$dates)
      month <- gsub('-','',gsub('-0','',substr(test,5,7)))
      day <- gsub('-','',gsub('-0','',substr(test,8,10)))
      test <- paste(month, day,substr(test,1,4),sep='/')
      test3 <- paste('petkey',test,sep=' ')
      envelopes(test3)[[2]]
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