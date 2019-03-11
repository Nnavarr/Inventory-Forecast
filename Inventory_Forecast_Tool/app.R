
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)
library(ggplot2)
library('broom')
library('stats')
library('lmtest')
library('DT')
library('DescTools')
library('lubridate')

# Custom Functions ----
#format_data <- source('C:\\Users\\1217543\\OneDrive\\Python Projects\\Inventory-Forecast\\Inventory-Forecast\\format_data.R')$value
#regression_metrics <- source('C:\\Users\\1217543\\OneDrive\\Python Projects\\Inventory-Forecast\\Inventory-Forecast\\regression_metrics.R')$value

# Insert functions referenced above (for deployment) ----

format_data <- 
function(x){
  
  require('dplyr')
  require('lazyeval')
  require('purrr')
  
  # Format Columns ----
  colnames(x)[1] <- 'Part_Number'
  colnames(x)[2] <- 'Date'
  
  # Convert "Date" column to Date values ----
  x$Date <- as.character(x$Date)
  x$Date <- as.Date(x$Date, format = '%m/%d/%Y')
  
  # Convert Part_Number column to character (not factor) ----
  x$Part_Number <- as.character(x$Part_Number)
  
  # Order by part number ASC ----
  x <-
    x %>%
    arrange(`Part_Number`)
  
  # Create trend and month columns ----
  x$Trend = ""
  x$Feb = ""
  x$Mar = ""
  x$Apr = ""
  x$May = ""
  x$Jun = ""
  x$Jul = ""
  x$Aug = ""
  x$Sep = ""
  x$Oct = ""
  x$Nov = ""
  x$Dec = ""
  
  # Create Trend column sequence ----
  x <- 
    x %>%
    group_by(Part_Number) %>%
    mutate(Trend = seq_along(Part_Number))
  
  
  for(i in seq_along(x$Date)){
    
    if(months(x$Date)[i] == 'February'){
      
      x$Feb[i] = 1
      
    } else if(months(x$Date)[i] == 'March'){
      
      x$Mar[i] = 1
      
    } else if(months(x$Date)[i] == 'April'){
      
      x$Apr[i] = 1
      
    } else if(months(x$Date)[i] == 'May'){
      
      x$May[i] = 1
      
    } else if(months(x$Date)[i] == 'June'){
      
      x$Jun[i] = 1
      
    } else if(months(x$Date)[i] == 'July'){
      
      x$Jul[i] = 1
      
    } else if(months(x$Date)[i] == 'August'){
      
      x$Aug[i] = 1
      
    } else if(months(x$Date)[i] == 'September'){
      
      x$Sep[i] = 1
      
    } else if(months(x$Date)[i] == 'October'){
      
      x$Oct[i] = 1
      
    } else if(months(x$Date)[i] == 'November'){
      
      x$Nov[i] = 1
      
    } else if(months(x$Date)[i] == 'December'){
      
      x$Dec[i] = 1
      
    } 
  }
  
  # Replace blank value with 0 ----
  x$Feb[x$Feb == ""] <- 0
  x$Mar[x$Mar == ""] <- 0
  x$Apr[x$Apr == ""] <- 0
  x$May[x$May == ""] <- 0
  x$Jun[x$Jun == ""] <- 0
  x$Jul[x$Jul == ""] <- 0
  x$Aug[x$Aug == ""] <- 0
  x$Sep[x$Sep == ""] <- 0
  x$Oct[x$Oct == ""] <- 0
  x$Nov[x$Nov == ""] <- 0
  x$Dec[x$Dec == ""] <- 0
  
  # Convert columns to numeric ----
  x$Feb <- as.numeric(x$Feb)
  x$Mar <- as.numeric(x$Mar)
  x$Apr <- as.numeric(x$Apr)
  x$May <- as.numeric(x$May)
  x$Jun <- as.numeric(x$Jun)
  x$Jul <- as.numeric(x$Jul)
  x$Aug <- as.numeric(x$Aug)
  x$Sep <- as.numeric(x$Sep)
  x$Oct <- as.numeric(x$Oct)
  x$Nov <- as.numeric(x$Nov)
  x$Dec <- as.numeric(x$Dec)
  
  return(x)
  
}


# -----------------------------------------------------------------------------------------------------------------

regression_metrics <- 
function(x){
  
  # The x variable represents the subset of part number DF. This will test our regression model prior to implementation. If out-of-sample accuracy is deemed acceptable, we can proceed with a full data set model.
  
  require('dplyr')
  require('lazyeval')
  require('purrr')
  
  # Create the LM (OLS regression) ----
  model = lm(Units ~
               Trend 
             + Feb
             + Mar
             + Apr 
             + May
             + Jun
             + Jul
             + Aug
             + Sep 
             + Oct
             + Nov 
             + Dec,
             data = x)
  
  model_summary <- summary(model)
  
  
  # Establish Crititcal T and F values based on model Degrees of Freedom ----
  # Critial T = Two Trailed, N-K-1 Degrees of Freedom & Critical F = one tailed test ----
  n_k_1 <- model$df.residual
  k = model_summary$fstatistic[2]
  
  critical_t <- round(abs(stats::qt(.05/2, n_k_1)),3)
  critical_f <- stats::qf(.95,  df1 = k, df2 = n_k_1)
  critical_chi <- stats::qchisq(.95, df = k)
  
  # -----------------------------------------------
  # Model Calibration Test 1: Multicolinearity ----
  # -----------------------------------------------
  
  # Independent X variable significance ----
  coefficient_pval <- model_summary$coefficients[,4]
  significance_x_var <- coefficient_pval < .05
  significance_x_var_boolean <- sum(significance_x_var) > 0
  
  # In the code above, if one X (independent) variable is significant, it will return TRUE.
  
  # F-test (all X variables combined; H0: All beta coefficients = 0 ; Ha: At least one beta coefficient != 0)
  significant_f_boolean <- model_summary$fstatistic[1] > critical_f
  
  # Multicolinearity conclusion ----
  multicolinearity_conflict <- significance_x_var_boolean != significant_f_boolean
  
  multicolinearity_results <-
    
    if(multicolinearity_conflict == FALSE){
      
      multicolinearity_results = 'Not Detected'
      
    } else {
      
      multicolinearity_results = 'Detected'
      
    }
  
  # -------------------------------------------------
  # Model Calibration Test 2: Heteroskedasticity ----
  # -------------------------------------------------
  
  augmented_model <- broom::augment(model)
  augmented_model$Date <- x$Date
  augmented_model <-
    augmented_model %>%
    mutate(Part_Number = x$Part_Number)
  
  augmented_model <- augmented_model[ ,c(21,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,22)]
  
  # Breusch-Pagan Test: Regress X variables on squared residuals from first regression ----
  resid_model = lm(.resid^2 ~
                     Trend 
                   + Feb
                   + Mar
                   + Apr 
                   + May
                   + Jun
                   + Jul
                   + Aug
                   + Sep 
                   + Oct
                   + Nov 
                   + Dec,
                   data = augmented_model)
  
  # Residual model summary ----
  resid_model_summary <- summary(resid_model)
  
  # Breusch-Pagan value: R-squared from residual regression * N (one tailed; only an issue if too big) ----
  bp_val <- resid_model_summary$r.squared * max(augmented_model$Trend) 
  
  heteroskedasticity_results <- 
    
    if(bp_val > critical_chi){
      
      heteroskedasticity_results = 'Detected'
      
    } else {
      
      heteroskedasticity_results = 'Not Detected'
      
    }
  
  # ----------------------------------------------
  # Model Calibration Test 3: Autocorrelation ----
  # ----------------------------------------------
  
  durbin_value <- lmtest::dwtest(model)
  durbin_watson_pvalue <- durbin_value$p.value
  autocorrelation_results <-
    
    if(durbin_watson_pvalue < .05){
      
      autocorrelation_results = 'Detected'
      
    } else {
      
      autocorrelation_results = 'Not Detected'
      
    }
  
  
  # Model Calibration Table ----
  Test <- c('Multicolinearity', 'Heteroskedasticity', 'Autocorrelation')
  Result <- c(multicolinearity_results, heteroskedasticity_results, autocorrelation_results)
  
  # Table compilation ----
  model_calibration_results <- data.frame(data.frame(Result))
  rownames(model_calibration_results) <- Test
  calibration_table <- DT::datatable(model_calibration_results) %>%
    formatStyle('Result',
                target = 'row',
                backgroundColor = styleEqual(c("Detected"), c('yellow')))
  
  
  # Model Forecast Table ----
  forecast_start_date <- DescTools::AddMonths(max(x$Date),1)
  forecast_end_date <- DescTools::AddMonths(forecast_start_date, 11)
  forecast_range <- seq.Date(from = forecast_start_date, to = forecast_end_date, by = 'month')
  forecast_trend <- seq.int(from = max(x$Trend) + 1, to = length(forecast_range) + max(x$Trend) , by = 1)
  forecast_df <- data.frame("Date" = forecast_range, "Trend" = forecast_trend)
  
  # Create Forecast ----
  model_coefficients <- stats::coef(model)
  numeric_coefficient_names <- c("Intercept", "Trend", 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  names(model_coefficients) <- numeric_coefficient_names
  forecast_df$Forecast <- as.numeric("")
  
  # Create linear model and point forecast ----
  for(i in seq_along(forecast_df$Date)){
    
    if(month(forecast_df$Date)[i] == 1){
      
      forecast_df$Forecast[i] = round(model_coefficients['Intercept'] 
                                      + (model_coefficients['Trend']* forecast_df$Trend[i])
                                      ,0)
      
    } else {
      
      forecast_df$Forecast[i] = round((model_coefficients['Intercept'] 
                                       + model_coefficients[paste(month(forecast_df$Date[i]))]) 
                                      + (model_coefficients['Trend'] * forecast_df$Trend[i])
                                      ,0)
    }
  }
  
  
  # Create confidence intervals (95%, upper and lower) ----
  forecast_df <- 
    forecast_df %>% 
    mutate(Part_Number = x$Part_Number[1])
  forecast_df$Upper_Conf <- as.numeric("")
  forecast_df$Lower_Conf <- as.numeric("")
  regression_standard_error <- stats::sigma(model)
  
  for(i in seq_along(forecast_df$Date)){
    
    forecast_df$Upper_Conf[i] = round(forecast_df$Forecast[i] + (critical_t * regression_standard_error),0)
    forecast_df$Lower_Conf[i] = round(forecast_df$Forecast[i] - (critical_t * regression_standard_error),0)
    
  }
  
  forecast_df <- forecast_df[ , c('Part_Number', 'Date', 'Trend', 'Forecast', 'Upper_Conf', 'Lower_Conf')] 
  
  # Create single dataframe that contains forecast & original data ----
  combined_df <- bind_rows(augmented_model[, c('Date', 'Units', 'Part_Number')], forecast_df)
  
  
  # Export Function Variables ----
  function_variables <- 
    list("Model" = model,
         "Model_Summary" = model_summary,
         "Augmented_Model" = augmented_model,
         "Calibration_Table" = calibration_table,
         "Forecast" = forecast_df,
         'Combined_DF' = combined_df)
  
  return(function_variables)
  
}



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
    
    menuItem(text = 'Disclosure',
             tabName = 'disclosure')
  )
)


# App Body: Controls & Output ----
body <- dashboardBody(

tabItems(
tabItem(tabName = 'summary',
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
      actionButton('action2', 'Run Model', class = 'btn-primary'),
      
      tags$h4('Model Summary (Final)'),
      verbatimTextOutput('final_model_summary')
      
    ),
    
    
    # ---------------------
    # Full Model Summary --
    # ---------------------
    
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
        
        # ---------------------------------------------------------
        # Secondary Tab (Residual Diagnostics on Training set) ----
        # ---------------------------------------------------------
        tabPanel("Model Training & Test",
                 
                 # Plotly graph of actual vs model ----
                 plotlyOutput('training_graph'),
                 
                 fluidRow(
                   valueBoxOutput('AdjR2'),
                   valueBoxOutput('MAE'),
                   valueBoxOutput('RMSE')
                 ),
  
                 fluidRow(
                   column(6, 
                          
                          # Model Output ----
                          tags$h3('Training Model Summary'),
                          verbatimTextOutput('training_model_summary')),
                   
                   column(6,
                          
                          # Calibration Table ----
                          tags$h3('Calibration Table'),
                          dataTableOutput(outputId = 't_calibration'))),
                 
                 fluidRow(
                   column(12,
                          tags$h3('Residual Distribution'),
                          plotlyOutput('t_residual_distribution')
                   )
                 ),
                 
                 fluidRow(
                   column(12, 
                          tags$h3('Residual vs Fitted'),
                          plotlyOutput('t_residual_vs_fitted')
                   
                 )
                   
                 )
                 
                  )
                 
                 )
      )
    )
  )
), # First Tab Item Closure ----


tabItem(tabName = 'disclosure',
        h3('Disclosure:'),
        h4('This tool was built for Luis Terrazas and team with the intent of forecasting inventory levels based on units sold. It uses a time series trend model with monthly categorical variables (seasonality) to forecast unit counts. This Ordinary Least Squares Regression technique represents one of many forecasting methodologies. It is important to remember the forecast is based on historical trends. That is, if underlying data characteristics change, the forecast may perform poorly. In order to mitigate this risk, it may be appropriate to recalibrate the model periodically. The use of the tool and associated forecasts are ultimately left to analyst discretion.')

    )
  )
)

# Second Tab Item (Disclosures) ----


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
          
        contrast_df$Units <- as.numeric(contrast_df$Actual_Units)

        # Out-of-sample errors ----
        contrast_df <-
          contrast_df %>%
          mutate(Residual = Actual_Units - Forecast,
                 Absolute_Error = abs(Residual),
                 Squared_Resid = Residual^2
                 )
    
        adj_r_squared <- t_model_summary$adj.r.squared
        
        MAE <-
          contrast_df %>%
          summarize(mean(Absolute_Error, na.rm = TRUE))
        
        # Root Mean Squared Error ----
        RMSE <- 
          contrast_df %>%
          summarize(sqrt(mean(Squared_Resid, na.rm = TRUE)))
        
        
        
        
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
      
      
      
      
      # --------------------------
      # Model Summary Outputs ----
      # --------------------------
      
      # Value Box Adjusted R2 ----
      output$AdjR2 <- renderValueBox({
        valueBox(
          paste0(round(adj_r_squared,4) * 100), subtitle = 'Adjusted R2 (Training)', icon = icon('percent'))
        
      })
      
      # Value Box Mean Absolute Error ----
      output$MAE <- renderValueBox({
        valueBox(
          paste0(round(MAE,0)), subtitle = 'Mean Absolute Error (Out-of-Sample)', icon = icon('times-circle'))
    
        })
      
      # Value Box Root Mean Squared Error ----
      output$RMSE <- renderValueBox({
        valueBox(
          paste0(round(RMSE,0)), subtitle = 'Root Mean Squared Error (Out-of-Sample)', icon = icon('times-circle'))
      })
      
      
      # Final Model Summary ----
      output$final_model_summary <- renderPrint({
        summary(model)
      })
      
      # Training Model Summary ----
      output$training_model_summary <- renderPrint({
        t_model_summary
      })
      
      # Training Model calibration table ----
      output$t_calibration <- renderDataTable({
        t_calibration_table
      })
      
      # Training Model Residual Distribution ----
      output$t_residual_distribution <- 
        renderPlotly({
          
          resid_dist <- plot_ly(data = t_augmented_model,
                          x = t_augmented_model$.resid,
                          type = 'histogram')
          
          layout(resid_dist, xaxis = list(title = 'Residual'), yaxis = list(title = 'Units'))
          
          })
      
      # Residual vs Fitted ----
      output$t_residual_vs_fitted <-
        renderPlotly({
          resid_vs_fitted <- plot_ly(data = t_augmented_model,
                                     x = t_augmented_model$.resid,
                                     y = t_augmented_model$.fitted,
                                     type = 'scatter')
          
          layout(resid_vs_fitted, xaxis = list(title = 'Fitted Values (Predicted)'), yaxis = list(title = 'Residual'))
          
        })
      
      

      })
    
}) # Close Observe Event Reactive section ----
  
  
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
