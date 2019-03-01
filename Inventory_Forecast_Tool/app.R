
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)
library(ggplot2)
library(markdown)
library(rjson)
library(RCurl)

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
  
#fixedRow(
# column(1, selectInput(inputId = "Part", label = "Part Number", choices = 'Test', selected = "13002"))),

fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      # Filte Upload: 'data'
      fileInput('file1', 
                'Choose CSV file to upload', 
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                  )
                ),
      
      # Format Button: 'action1'
      tags$h5('Data Processing:'),
      actionButton("action1", "Format"),
      
      
     # selectInput('part_number', 'Part Number', choices = , selected = 'None'),
     
      # Training Exclusion: 'training'
      numericInput('training', 'Excluded observations (training)', 12),
      
      # Run model: 'action2'
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
  
  values <- reactiveValues(df_data = NULL)
  formatted_df <- reactiveValues(df_data = NULL)
  
  
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
  
  
  # Create a data table of the output ----
  output$test2 <-
    renderDataTable({
      formatted_df$df_data
    })
  
 
  output$test <- 
    renderDataTable({
    
      inFile <- input$file1
      
     if(is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = TRUE, sep = ",")
      
    })
  
  
  
  
  # Render Plotly Forecast Graph ----
  output$forecast_graph <-
    renderPlotly({
      plotly::plot_ly(Forecast,
                      x = ~Date, 
                      y = ~Forecast,
                      mode = 'lines')
      
      
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
