
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
