
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)
library(ggplot2)
library(markdown)
library(rjson)
library(RCurl)


# -----------------
# Shiny app UI ----
# -----------------

# App Architecture ----
header <- dashboardHeader(
  title = "Forecast Tool"
  )

# Side Bar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    
    # Create Menu ----
    menuItem(text = 'Forecast Summary',
             tabName = 'summary'),
    
    sidebarLayout(
      sidebarPanel(
        fileInput('data', 'Choose CSV file to upload',
                  accept = '.csv'
                  )
      )
    )
  )
)


#TODO Enter button to upload CSV Data 
# This imported dataframe will be referenced directly in the line below


# Control Dropdowns ----
#fixedRow(
 # column(1, selectInput(inputId = "Part", label = "Part Number", choices = <REFERENCE DF HERE>, selected = "13002"))
#)

# ----------------------------------------------------------------------------------------------------------------------------------

# -------------
# Server.R ----
# -------------

function(input, output) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$data
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
}




