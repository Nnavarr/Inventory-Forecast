# Inventory Forecast Tool 

## Overview
The inventory forecast tool was originally developed for the retail sales team at U-Hual with the goal of forecasting hitch unit demand. It was their resposibility to ensure an adequate amount of inventory was maintained on-hand and these forecasts would be used to purchase inventory accordingly. While there are various forecasting methods, Ordinary Least Squares (OLS) was used in order to capture and convey various month effects on unit sales. The explainability of OLS regression in terms of variable contribution/impact made it an ideal candidate for forecasting methodology. 

Because this is a time series, the model was created with two parts in mind; 1) a trend (monthly) and 2) Month specific categorical variables. The latter would help explain the difference between a Januray base case scenario against other months. That is, we could determine (on average) how demand was different in January vs August in terms of unit sales. 

In terms of scalability, because there were 100+ parts needing a forecast, a Shiny App framework (R) was used to provide flexibility. The app itself was set to take an Excel upload, generate monthly forecasts, provide back-testing on out-of-sample data, model calibration tests (multicolinearity, heteroskedasticity, and autocorrelation), and finally a forecast download via CSV. 

## App Update (2020)
During the start of the pandemic, our team was interested in estimating COVID-19 impact on major business lines. At this time, the inventory forecast tool was re-purposed and generalized for general ledger accounts to be forecasted using the same methodology. This is the most recent version of the tool.

## App Location 
The tool is currently hosted on shinyapps.io
Link: https://nnavarr.shinyapps.io/Forecast_Tool/?_ga=2.164557432.693284105.1618099831-351596434.1618099831

## How to use
1. Upload monthly csv data in the following format <Insert picture of format>
2. Click the "Format" button to process the data into a regression ready state
3. Select the Account Number. Here, the account number is just used as a group classifier, it doesn't have to be an account number; any tag will suffice. 
4. Select the number of observations to remove for the out-of-sample model test
5. Click "Run Model"

From here, the user can change the Account number selection and check model performance under the "Model Training & Test" tab. If the user is satisfied with the forecast error, the forecasted numbers along with upper/lower bounds at 95% confidence can be downloaded by clicking the "Download" button below. 
