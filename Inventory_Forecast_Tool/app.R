
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)
library(ggplot2)
library(markdown)
library(rjson)
library(RCurl)

library('forecast', lib.loc="~/R/win-library/3.4")
library('xts')
library('broom')
library('stats')
library('lmtest')
library('DT')
library('ggpubr')
library('DescTools')
library('lubridate')

# Custom Functions ----
format_data <- source('C:\\Users\\1217543\\OneDrive\\Python Projects\\Inventory-Forecast\\Inventory-Forecast\\format_data.R')$value
regression_metrics <- source('C:\\Users\\1217543\\OneDrive\\Python Projects\\Inventory-Forecast\\Inventory-Forecast\\regression_metrics.R')$value

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
             tabName = 'summary')
  )
)


# App Body: Controls & Output ----
body <- dashboardBody(
  
fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      # Filte Upload: 'data' ----
      fileInput('file1', 
                'Choose CSV file to upload', 
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                  )
                ),
      
      # Format Button: 'action1' ----
      tags$h5('Data Processing:'),
      actionButton("action1", "Format"),
      
      # Dropdown of part numbers ----
      uiOutput("part_number"),
     
      # Training Exclusion: 'training'
      numericInput('training', 'Excluded observations (training)', 12),
      
      # Run model: 'action2' ----
      tags$h5('Forecast Units:'),
      actionButton('action2', 'Run Model', class = 'btn-primary')
      
    ),
    
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Model: Final",
                  
                 # Plotly Graph Output ----
                 plotlyOutput("forecast_graph"),
                 verbatimTextOutput("Unit Forecast"),
                 
                 # Table Output ----
                 dataTableOutput(outputId = 'test'),
                 
                  # Forecast Table ----
                # h4("Forecast Table"),
                # tableOutput("forecast_table"),
                  
                 # Download Forecast Table ----
                 downloadButton("download_data", "Download"),
                
                 # Formatted data test ----
                 dataTableOutput(outputId = 'test2')
                  
        )
        
      
      )
    )
  )
)
)


# Define UI for application (Dashboard) ----
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)

# ----------------------------------------------------------------------------------------------------------------------------------

# -------------
# Server.R ----
# -------------

server <- function(input, output) {
  
  # ------------------------------------------------------------------------
  # Data frame containers ----
  values <- reactiveValues(df_data = NULL)
  formatted_df <- reactiveValues(df_data = NULL)
  filtered_df <- reactiveValues(df_data = NULL)
  regression_output <- reactiveValues(df_data = NULL)
  # -------------------------------------------------------------------------
  
  # <PROGRESS BAR INPUT> 
  
  # Observe Data upload & Store variable ----
  observeEvent(input$file1, {
    values$df_data <- read.csv(input$file1$datapath, header = TRUE, sep = ",")
  })
  
  
  # Observe Click of "Format" button ----
    observeEvent(input$action1, { # Once the button is clicked, do the following 
        
        # Save formatted DF as variable ----
        temp_df <- format_data(values$df_data)
        formatted_df$df_data <- temp_df
        
    
      })
  
  
  # Part Number drop down ----
  output$part_number <- renderUI({
    df <- formatted_df$df_data
    items <- as.character(df[[1]])
    
    selectInput("part_dropdown", "Part Number (Format before selecting):", items)
    
  })
  
  
  # Create filtered data frame of single part number ----
  observeEvent(input$part_dropdown, {
    
    # Filter formatted DF ----
    
      if(is.null(formatted_df$df_data)){
        
        filtered_df$df_data <- 'None'
        
      } else {
        
        filtered_df$df_data <- 
          formatted_df$df_data %>%
          dplyr::filter(Part_Number == input$part_dropdown)
      }
  })
  
  
  # Regression Function process ----
  observeEvent(input$action2, {
    
    observeEvent(input$part_dropdown, {
      
      # Filter formatted DF ----
      
      if(is.null(formatted_df$df_data)){
        
        filtered_df$df_data <- 'None'
        
      } else {
        
        filtered_df$df_data <- 
          formatted_df$df_data %>%
          dplyr::filter(Part_Number == input$part_dropdown)
      }
      
      # Regression Part ----  
      regression_output$df_data <- 
        regression_metrics(filtered_df$df_data)
      
      model <- regression_output$df_data[['Model']]
      model_summary <- regression_output$df_data[['Model_Summary']]
      augmented_model <- regression_output$df_data[['Augmented_Model']]
      calibration_table <- regression_output$df_data[['Calibration_Table']]
      forecast_table <- regression_output$df_data[['Forecast']]
      
      print(forecast_table)
      
      #Render Plotly Forecast Graph ----
      output$forecast_graph <-
        renderPlotly({
        plot_ly(data.frame(forecast_table),
        x = ~Date, 
        y = ~Forecast,
        mode = 'lines')
           
      })
      
      
      })
})
  
  
  # Create a data table of the output ----
  output$test2 <-
    renderDataTable({
      filtered_df$df_data
    })
  
  
  # Data table output ----
  output$test <- 
    renderDataTable({
    
      inFile <- input$file1
      
     if(is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = TRUE, sep = ",")
      
    })
  
  
  # Download Forecast to CSV ----
  output$download_data <- 
    downloadHandler(
      filename = function(){
        
        paste(forecast_table$Part_Number[1],"_Forecast", ".csv", sep = "")
      },
      content = function(file) {
        
        write.csv(forecast_table, file, row.names = FALSE)
        
      }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
