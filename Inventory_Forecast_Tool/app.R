
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
      actionButton('action2', 'Run Model', class = 'btn-primary'),
      
      tags$h5('Model Summary (Final)'),
      verbatimTextOutput('final_model_summary')
      
    ),
    
    
    
    mainPanel(
      tabsetPanel(
        
        # Initial Tab (Final Model Summary)
        tabPanel("Model: Final",
                  
                 # Plotly Graph Output ----
                 plotlyOutput("forecast_graph"),
                 verbatimTextOutput("Unit Forecast"),
                 
                # Forecast Table ----
                 h4("Forecast Table"),
                 tableOutput("forecast_table"),
                
                 # Formatted data test ----
                 dataTableOutput(outputId = 'forecast'),
                
                # Download Forecast Table ----
                downloadButton("download_data", "Download")
                  
        ),
        
        # Secondary Tab (Residual Diagnostics on Training set) ----
        tabPanel("Model Training & Test",
                 
                 # Plotly graph of actual vs model ----
                 plotlyOutput('training_graph'),
                 
                 # Model Output ----
                 verbatimTextOutput('final_model_summary')
                 
                 # Residual Distribution ----
                 
                 
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
  training_regression <- reactiveValues(df_data = NULL)
  
  forecast_df <- reactiveValues(df_data = NULL)
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
      combined_df <- regression_output$df_data[['Combined_DF']]
      
      forecast_df$df_data <- forecast_table[,c('Date', 'Part_Number', 'Trend', 'Forecast', 'Upper_Conf', 'Lower_Conf')]
      
      #Render Plotly Forecast Graph ----
      hovertxt <- paste0('<b>Date: </b>', combined_df$Date, '<br>',
                          '<b>Forecast: </b>', combined_df$Forecast, '<br>',
                          '<b>Upper Bound: </b>', combined_df$Upper_Conf, '<br>',
                          '<b>Lower Bound: </b>', combined_df$Lower_Conf)
      
      output$forecast_graph <-
        renderPlotly({
        plot_ly(combined_df, x = ~Date)  %>%
            add_lines(y = ~Units, name = 'Current') %>%
            add_lines(y = ~Forecast, name = 'Forecast', line = list(width = 4, color = "#00587b"),
                      hoverinfo = 'text', text = hovertxt) %>%
            add_lines(y = ~Lower_Conf, name = 'Lower', line = list(color = "#3d83a3")) %>%
            add_lines(y = ~Upper_Conf, name = 'Upper', line = list(color = "#3d83a3"), 
                      fill = 'tonexty')
          
        })
          
      # Tranining Set Regression ----
      training_df <- 
        subset(filtered_df$df_data, Trend <= max(Trend) - as.numeric(input$training))
          
      training_regression$df_data <- 
        regression_metrics(training_df)
      
      # Subset individual regression parts ----
      t_model <-  training_regression$df_data[['Model']]
      t_model_summary <- training_regression$df_data[['Model_Summary']]
      t_augmented_model <- training_regression$df_data[['Augmented_Model']]
      t_calibration_table <- training_regression$df_data[['Calibration_Table']]
      t_forecast_table <- training_regression$df_data[['Forecast']]
      t_combined_df <- training_regression$df_data[['Combined_DF']]
        actual <- augmented_model[, c('Date', 'Units')]
        contrast_df <- left_join(x=t_combined_df, y=actual, by = 'Date')
        names(contrast_df)[2] <- 'Units'
        names(contrast_df)[8] <- 'Actual_Units'
      
      
      # Render Plotly model training graph ----
      t_hovertxt <- paste0('<b>Date: </b>', t_combined_df$Date, '<br>',
                         '<b>Forecast: </b>', t_combined_df$Forecast, '<br>',
                         '<b>Upper Bound: </b>', t_combined_df$Upper_Conf, '<br>',
                         '<b>Lower Bound: </b>', t_combined_df$Lower_Conf)
      
      
      output$training_graph <-
        renderPlotly({
          plot_ly(contrast_df, x = ~Date)  %>%
            add_lines(y = ~Actual_Units, name = 'Actual') %>%
            add_lines(y = ~Forecast, name = 'Forecast', line = list(width = 4, color = "#00587b", dash = 'dash'),
                      hoverinfo = 'text', text = t_hovertxt) %>%
            add_lines(y = ~Lower_Conf, name = 'Lower', line = list(color = "#3d83a3")) %>%
            add_lines(y = ~Upper_Conf, name = 'Upper', line = list(color = "#3d83a3"), 
                      fill = 'tonexty')
        })
      
      
      
      # Model Summary Outputs ----
      # Final Model Summary ----
      output$final_model_summary <- renderPrint({
        summary(model)
      })
      
      # Training Model Summary ----
      output$training_model_summary <- renderPrint({
        summary(t_model)
      })
      
      })
})
  
  
  # Create a data table of the output ----
  output$forecast <-
    renderDataTable({
      forecast_df$df_data
    })
  
  
  # Download Forecast to CSV ----
  output$download_data <- 
    downloadHandler(
      filename = function(){
        
        paste(input$part_dropdown,'_',Sys.yearmon(),'_forecast','.csv', sep = "")
      },
      content = function(file) {
        
        write.csv(forecast_df$df_data, file, row.names = FALSE)
        
      }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
