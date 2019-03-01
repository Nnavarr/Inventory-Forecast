
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)
library(ggplot2)
library(markdown)
library(rjson)
library(RCurl)


# ------------------
# App Functions ----
# ------------------

# Function 1: Format inidial data import ----
# Here, 1st col = Part Number, 2nd col = Date, 3rd col = Unit Count ----

format_data <- function(x){
  
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


# Function 2: Regression Model & Forecast ----
regression_metrics <- function(x){
  
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
  
  t_model_summary <- summary(training_model)
  
  
  # Establish Crititcal T and F values based on model Degrees of Freedom ----
  # Critial T = Two Trailed, N-K-1 Degrees of Freedom & Critical F = one tailed test ----
  n_k_1 <- training_model$df.residual
  k = t_model_summary$fstatistic[2]
  
  critical_t <- round(abs(stats::qt(.05/2, n_k_1)),3)
  critical_f <- stats::qf(.95,  df1 = k, df2 = n_k_1)
  critical_chi <- stats::qchisq(.95, df = k)
  
  # -----------------------------------------------
  # Model Calibration Test 1: Multicolinearity ----
  # -----------------------------------------------
  
  # Independent X variable significance ----
  coefficient_pval <- t_model_summary$coefficients[,4]
  significance_x_var <- coefficient_pval < .05
  significance_x_var_boolean <- sum(significance_x_var) > 0
  
  # In the code above, if one X (independent) variable is significant, it will return TRUE.
  
  # F-test (all X variables combined; H0: All beta coefficients = 0 ; Ha: At least one beta coefficient != 0)
  significant_f_boolean <- t_model_summary$fstatistic[1] > critical_f
  
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
  augmented_model <- augmented_model[ ,c(21,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
  
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
  bp_val <- resid_model_summary$r.squared * max(augment_model$Trend) 
  
  heteroskedasticity_results <- 
    
    if(bp_val > critical_chi){
      
      heteroskedasticity_results = 'Detected'
      
    } else {
      
      heteroskedasticity_results = 'Not Detected'
      
    }
  
  # ----------------------------------------------
  # Model Calibration Test 3: Autocorrelation ----
  # ----------------------------------------------
  
  durbin_value <- lmtest::dwtest(reg_model)
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
  calibration_table <- DT::datatable(model_calibration_results)
  
  
  # Model Forecast Table ----
  forecast_start_date <- DescTools::AddMonths(max(x$Date),1)
  forecast_end_date <- DescTools::AddMonths(forecast_start_date, 11)
  forecast_range <- seq.Date(from = forecast_start_date, to = forecast_end_date, by = 'month')
  forecast_trend <- seq.int(from = forecast_trend + 1, to = length(forecast_range) + forecast_trend , by = 1)
  forecast_df <- data.frame("Date" = forecast_range, "Trend" = trend_sequence)
  
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
    forecast_df$Lower_Conf[i] = round(forecast_df$Forecast[i] + (critical_t * regression_standard_error),0)
    
  }
  
  forecast_df <- forecast_df[ , c('Part_Number', 'Date', 'Trend', 'Forecast', 'Upper_Conf', 'Lower_Conf')] 
  
  # Export Function Variables ----
  function_variables <- 
    list("Model" = model,
         "Model_Summary" = t_model_summary,
         "Augmented_Model" = augmented_model,
         "Calibration_Table" = calibration_table,
         "Forecast" = forecast_df)
  
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
                 tableOutput(outputId = 'test'),
                 
                  # Forecast Table ----
                 # h4("Forecast Table"),
                #  tableOutput("forecast_table"),
                  
                  # Download Forecast Table ----
                  downloadButton("download_data", "Download")
                  
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
  
  output$test <- 
    renderTable({
      
      inFile <- input$file1
      
      if(is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = TRUE, sep = ",")
      
    })
  
  # Observe Click of "Format" button ----
  format_button_click <- 
    eventReactive(
      input$action1, 
      {
        formatted_data <<- format_data(input$data)
      }
      
  )
  
  
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
