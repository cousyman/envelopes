shinyUI(fluidPage(
  titlePanel("Envelope Creation"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create envelopes for Mikey!"),
      
      dateInput('dates',
                     label='Date range input',
                     start = Sys.Date() - 1),
      downloadButton('downloadData','Download')
  ),
    mainPanel(tableOutput('table'))
  )
))