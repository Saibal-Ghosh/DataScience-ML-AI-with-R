################################################
## This file is for daily demand of the chemical
## Obective is to predict daily demand of the chemical, with high accuracy

## Participants should :-

## Clean the data
## perform EDA and Vizualizations
## perform feature engineering
## build a model (logistic regression)
## evaluate the model
## provide business insights from the model

## Data Understanding
## Data Cleaning/ Quality checks
## Data Manipulations
## Data Exploration and Viz
## Modeling (Development and Evaluation)
## Submissions

## Created : Saibal 14/05/2022
################################################

## Clear all variables in workspace   --  starting from fresh
rm(list=ls())

## Installing the Required Packages & Calling the Packages
install.packages("dplyr")	
library(dplyr)
install.packages("knitr")
library(knitr)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("mice")
library(mice)
install.packages("lattice")
library(lattice)
install.packages("reshape2")
library(reshape2)
install.packages("DataExplorer") # if the following package is not available
library(DataExplorer)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
library(car)
library(forecast)
library(tseries)
install.packages("fpp2")
library(fpp2)
library(lmtest)
library(TTR)
install.packages("MLmetrics")
library(MLmetrics)

## Setting the Directory and Checking the Working Directory 
setwd("C:\\NC-Personal\\IPBA Jigsaw\\Training Materials for Class\\R Basics\\ChemicalDemand_TimeSeries_Practice")
getwd()

## Reading the Train, Test, Sample data into variables and taking a look
train <- read.csv("DemandTrain.csv")
test <- read.csv("DemandTest.csv")
sample <- read.csv("DemandSubmission.csv")
dim(train)
head(train)
plot(train)
dim(test)
dim(sample)
introduce(train)

# Converting to TimeSeries format, keeping train from 2014 onwards, and test from 2018 onwards and forecast for 1 yr
# If we want to have an End-Date , we can use end=c(2015,31,12) i.e. dataset till 31st Dec 2015
# Frequency is how many datasets we are taking into consideration for Prediction
# Here Freq == 365, since data is in Daily format and we are taking into 1 Year data for Forecasting
# If Data is in Monthly format, we will use Freq == 12
# If Data is in Weekly format, we will use Freq == 52
# If Data is in Yearly format, we will use Freq == 12
# and for Predicting we have to use the 113 days as per Q or Requirement using h=113 (at a later point of code)

## Also Plotting the initial to see how it looks
## ggtsdisplay() is used to generate ACF and PACF plot also

## Declare this as TimeSeries data
train_ts <- ts(train[-1],start=c(2014,1,1),frequency=450)

## Preliminary Analysis

head(train_ts)
plot(train_ts)
ggtsdisplay(train_ts)
autoplot(train_ts)
autoplot(train_ts) + ggtitle("Chemical Demand Plot") + ylab("Dollar in Million") + xlab("Time Period in Years")

## if there is a TREND find the differences 

        diff(train_ts) # default difference = 1            -OR-
        diff(train_ts, differences = 1)
        diff(train_ts, differences = 2)  # difference = 2


## plotting to see if there is a TREND based on the difference 
## if it changes to a Normal or the TREND is gone means okay to proceed to next step  
        
## the value of Difference we use is the value of "d" for ARIMA model        
        
        autoplot(diff(train_ts, differences = 1))  # difference = 1
        autoplot(diff(train_ts, differences = 2))  # difference = 2
        autoplot(diff(train_ts, differences = 3))  # difference = 3        
        
        
## Once TREND is removed using differences "diff()" we can investigate seasonality

        ## One Seasonal Plot         
ggseasonplot(train_ts)
ggseasonplot(train_ts) + ggtitle("Chemical Demand Plot") + ylab("Dollar in Million") + xlab("Time Period in Years")           

        ## Another Seasonal Plot 
ggsubseriesplot(train_ts) 
ggsubseriesplot(train_ts) + ggtitle("Chemical Demand Plot") + ylab("Dollar in Million") + xlab("Time Period in Years")           

 
boxplot(train_ts)



  # Doing a Regular differencing / derivative
  reg_diff <- diff(train_ts)
  ggtsdisplay(reg_diff)
  
  # Doing a Seasonal differencing / derivative
  seasonal_diff <- diff( train_ts , lag=100, differences = 1)
  ggtsdisplay(seasonal_diff)
  
  # Removing both seasonal and regular derivative / differencing
  reg_seasonal_diff <- diff( diff( train_ts, lag=100  )  )
  ggtsdisplay(reg_seasonal_diff)


  
## *** this is bit slow ***

  # Now, try various ARIMA model with both seasonality and non-seasonal based on the ACF and PACF plots (as below)  
  pred0.1 <- arima(train_ts , order=c(3,1,1), seasonal = c(0,1,0) , lambda=NULL, include.constant = TRUE)
  
  
  # Do an autoplot(pred0.1) of each model - to check if all points are lying withing the circle for perfect one
  # it should plot all the points within the circle , if any points on or outside the circle - means some issue
  autoplot(pred0.1)

  # Then a coefficient test of each models - the p-value should be significant ie. < 0.05
  coeftest(pred0.1)
  
  # Then check the residuals of each of the model - the p-value should be significant ie. < 0.05
  checkrsiduals(pred0.1)
  
  # Then plot residuals of each of the model - most lines of ACF and PACF should be within BLUE lines
  ggtsdisplay(pred0.1$residualsduals)
  
  
  # Then runs a summary of the model to check AIC, BIC values - AIC should decrease for a good model
  summary(pred0.1$residualsduals)
  

  
## Forecasting using snaive() Forecasting Method
  
pred6_snaive <- snaive(train_ts)  
summary(pred6_snaive)  
accuracy(pred6_snaive)

### Prediction Using Simple Exponential Model  

      pred6_ses <- predict(  snaive(train_ts)  ,h = 113, prediction.interval = TRUE)  
      pred6_ses
      
      # plotting the forecast
      plot(data.frame(pred6_ses)["Point.Forecast"][,1])
      
      # Submission dataframe with 2 columns
      submission <- data.frame(test,data.frame(pred6_ses)["Point.Forecast"][,1])
      
      # Rename the columns to match sample submission format
      names(submission) = c("date", "mean_demand_kiloGallon")
      
      # Check submission df
      head(submission)
      
      # writing submission file to local
      write.csv(submission, "saibal_submission.csv", row.names = FALSE)

      
## Checking the accuracy and summary of the Model
      summary(pred6_ses)
      checkresiduals(pred6_ses)
    
 


# Checking a BoxCox to see how linear the Model is 
# any value close to 1 means Model is Linear - we dont need to do Transformation or Logarithmic of the Data
# say if a value of 0.21 comes we have to think of Transformation

BoxCox.lambda(train_ts)

test_ts <- ts(train[-1],start=c(2018,1,1),frequency=365)
head(test_ts)
plot(test_ts)

## Decompose - using Multiplicative or Additive Model   by decompose()
## it will split into different sets of Trend, Seasonality, Observer, Random 
train_ts_dec_add <- decompose(train_ts, type = "additive")   # additive model 
train_ts_dec_mul <- decompose(train_ts, type = "multiplicative")  # multiplicative model



#plotting the graph 
plot(train_ts_dec_add$seasonal)         # plotting individually each component
plot(train_ts_dec_add$trend) 
plot(train_ts_dec_add$random)
plot(train_ts_dec_add)                      # plotting all components together 
# OR
autoplot(train_ts_dec_add)
autoplot(train_ts_dec_mul)


# Forecasting Using Simple Exponential Model  

ses_train_ts <- ses(train_ts, 113)   # calling a model and giving 113 time frames ahead
ses_train_ts
summary(ses_train_ts) 
plot(ses_train_ts)
accuracy(ses_train_ts)               # will show accuracy of the training set by default
accuracy(ses_train_ts, x=test_ts)   # checking the accuracy of the test model wrt training set 

### Prediction Using Simple Exponential Model  

    pred1_ses <- predict(ses_train_ts,h = 113, prediction.interval = TRUE)
    pred1_ses
    
    # plotting the forecast
    plot(data.frame(pred1_ses)["Point.Forecast"][,1])
    
    # Submission dataframe with 2 columns
    submission <- data.frame(test,data.frame(pred1_ses)["Point.Forecast"][,1])
    
    # Rename the columns to match sample submission format
    names(submission) = c("date", "mean_demand_kiloGallon")
    
    # Check submission df
    head(submission)
    
    # writing submission file to local
    write.csv(submission, "saibal_submission.csv", row.names = FALSE)


# Forecasting using Plain Holts Winter Model to predict for next 113 days
hw_train_ts <- holt(train_ts, 113)
hw_train_ts
plot(hw_train_ts)
accuracy(hw_train_ts)
accuracy(hw_train_ts, x=test_ts)  

### Prediction using Plain Holts Winter Model to predict for next 113 days

      pred2_plainhltwinter <- predict(hw_train_ts,h = 113, prediction.interval = TRUE)
      pred2_plainhltwinter
      
      # plotting the forecast
      plot(data.frame(pred2_plainhltwinter)["Point.Forecast"][,1])
      
      # Submission dataframe with 2 columns
      submission <- data.frame(test,data.frame(pred2_plainhltwinter)["Point.Forecast"][,1])
     
       # Rename the columns to match sample submission format
      names(submission) = c("date", "mean_demand_kiloGallon")
      
      # Check submission df
      head(submission)
      
      # writing submission file to local
      write.csv(submission, "saibal_submission.csv", row.names = FALSE)



# Damped Holts         
hw_d_train_ts <- holt(train_ts, 113, damped = T)  # it tries to damp or bring the curve down to realistic
hw_d_train_ts
plot(hw_d_train_ts)
accuracy(hw_d_train_ts)
accuracy(hw_d_train_ts, x=test_ts)  


### Prediction using Damped Holts Winter Model to predict for next 113 days

      pred3_damphltwinter <- predict(hw_d_train_ts,h = 113, prediction.interval = TRUE)
      pred3_damphltwinter
      
      # plotting the forecast
      plot(data.frame(pred3_damphltwinter)["Point.Forecast"][,1])
      
      # Submission dataframe with 2 columns
      submission <- data.frame(test,data.frame(pred3_damphltwinter)["Point.Forecast"][,1])
      
      # Rename the columns to match sample submission format
      names(submission) = c("date", "mean_demand_kiloGallon")
      
      # Check submission df
      head(submission)
      
      # writing submission file to local
      write.csv(submission, "saibal_submission.csv", row.names = FALSE)


# Forecasting using Damped Holts Winter Model (Additive or Multiplicative) to predict for next 113 days
      
hw_d_add_train_ts <- holt(train_ts, 113, damped = T, seasonal= 'additive')
#accuracy(hw_d_add_train_ts, x=test_ts) 

hw_d_mul_train_ts <- holt(train_ts, 113, damped = T, seasonal= 'multiplicative')
#accuracy(hw_d_mul_train_ts, x=test_ts) 

hw_add_train_ts <- holt(train_ts, 113,  seasonal= 'additive')
#accuracy(hw_add_train_ts, x=test_ts) 

hw_mul_train_ts <- holt(train_ts, 113,  seasonal= 'multiplicative')
#accuracy(hw_mul_train_ts, x=test_ts) 


### Prediction using Damped Holts Winter Model (Additive or Multiplicative) to predict for next 113 days

#  Re-Iterate this step 4 times (Additive & Multiplicative for Damped and Normal)

      pred4_damphltwinter_mul <- predict(hw_d_mul_train_ts,h = 113, prediction.interval = TRUE)
      pred4_damphltwinter_mul
      
      pred4_damphltwinter_add <- predict(hw_d_add_train_ts,h = 113, prediction.interval = TRUE)
      pred4_damphltwinter_add      
      
      
      pred4_damphltwinter_add <- predict(hw_add_train_ts,h = 113, prediction.interval = TRUE)
      pred4_damphltwinter_add  
      
      pred4_damphltwinter_add <- predict(hw_mul_train_ts,h = 113, prediction.interval = TRUE)
      pred4_damphltwinter_add  
      
      
      
      # plotting the forecast
      plot(data.frame(pred4_damphltwinter_add)["Point.Forecast"][,1])
      
      # Submission dataframe with 2 columns
      submission <- data.frame(test,data.frame(pred4_damphltwinter_add)["Point.Forecast"][,1])
      
      # Rename the columns to match sample submission format
      names(submission) = c("date", "mean_demand_kiloGallon")
      
      # Check submission df
      head(submission)
      
      # writing submission file to local
      write.csv(submission, "saibal_submission.csv", row.names = FALSE)





# Forecasting time series univariate Using Another Method ets() - ETS (Error, Trend, Seasonal)
# This ETS model focuses on trend and seasonal  components    

auto_ets_train_ts <- ets(train_ts)
summary(auto_ets_train_ts)


# Predicting using ets() method for next 113 days

      pred5_auto_forecast_train_ts <- predict(auto_ets_train_ts,h = 113, prediction.interval = TRUE)
      pred5_auto_forecast_train_ts
      ##  MAPE(pred5_auto_forecast_train_ts$mean, validation) * 100 , where validation is the time period needed
      MAPE(pred5_auto_forecast_train_ts$mean, 113) * 100
      
      # plotting the forecast
      plot(data.frame(pred5_auto_forecast_train_ts)["Point.Forecast"][,1])
      
      # Submission dataframe with 2 columns
      submission <- data.frame(test,data.frame(pred5_auto_forecast_train_ts)["Point.Forecast"][,1])
      
      # Rename the columns to match sample submission format
      names(submission) = c("date", "mean_demand_kiloGallon")
      
      # Check submission df
      head(submission)
      
      # writing submission file to local
      write.csv(submission, "saibal_submission.csv", row.names = FALSE)
  
  
# Checking Accuracy with sample Test Data Set
accuracy(pred5_auto_forecast_train_ts, x=test_ts)


# Finally do a Ljung box test to see p-value > 0.05 at various lags (i.e. p-values should not be significant)
# if p-value < 0.05 means that data is still having auto correlation

Box.test(pred5_auto_forecast_train_ts$residuals, lag =5 , type='Ljung-Box')
Box.test(pred5_auto_forecast_train_ts$residuals, lag =20 , type='Ljung-Box')




# ADF Test ( Stationary Test {using the differencing}  ) [Augmented Dickey-Fuller Test]
# p-value should be < 0.05 for Series to be Stationary

adf.test(train_ts)        #no difference 
adf.test(diff(train_ts)) # default difference = 1            -OR-
adf.test(diff(train_ts, differences = 1))
adf.test(diff(train_ts, differences = 2))  # difference = 2



# trying to find the AR and MA based on PACF and ACF plots
# Choosing p (AR or Lag) term with PACF Plot
# Choosing q (MA or Moving Average) term with ACF Plot
# here in our case pacf and acf will work for diff==1
# ACF and PACF plots should have most lines within BLUE boundaries for a model to be Stationary
pacf(diff(train_ts, differences = 2))
acf(diff(train_ts, differences = 2))

pacf(diff(train_ts))
acf(diff(train_ts))

pacf(diff(train_ts, differences = 4))


# Fitting ARIMA Model arima(p,d,q) based on PACF and ACF
pred0.1 <- arima(train_ts , order=c(3,1,1), seasonal = c(0,1,0) , lambda=NULL, include.constant = TRUE)

m_arima <- arima(x=train_ts, order=c(1,1,2))
m_arima

m_arima <- arima(x=train_ts, order=c(0.3,2,2))
m_arima

# Using Auto ARIMA features to find the best Model as per R  (** But ** remember it can be slow based on dataset)
# the best auto arima is the 1 with low AIC value
# tracing aith Auto Arima()

auto.arima(train_ts)
# -OR- 
auto.arima(train_ts, ic = "aic", trace = TRUE)


# SARIMA syntax - but need to check further - giving error 
# sarima_forecast = sarima.for(train_ts, n.ahead=length(validation),  p=0,d=1,q=1,P=1,D=1,Q=0,S=12)
#  sarima_forecast = sarima.for(train_ts, n.ahead=1,  p=3,d=1,q=1,P=0,D=1,Q=0,S=12)



# Now, we can use this auto suggested models to plot our ARIMA once with (3,1,1) and another with (0,1,0)
# auto arima with predicted model

m_arima_auto <- arima(x=train_ts, order=c(4,1,3))
checkresiduals(m_arima_auto)

# Once you get the ARIMA model based on PACF/ACF or AUTO ARIMA - we need to Forecast for desired future time period
# Now Forecast for the next 113 days and plot it to check it
forecast(m_arima_auto, h=113)
plot(forecast(m_arima_auto, h=113)) 


# Now, Predicting using the ARIMA model (derived from AUto Prediction)
# forecast() is used to predict the forecast

      #Prediction 
      hlt.pred <- forecast(m_arima_auto, h=113)
      hlt.pred
      
      # plotting the forecast
      plot(data.frame(hlt.pred)["Point.Forecast"][,1])
      
      # Submission dataframe with 2 columns
      submission <- data.frame(test,data.frame(hlt.pred)["Point.Forecast"][,1])
      # Rename the columns to match sample submission format
      names(submission) = c("date", "mean_demand_kiloGallon")
      # Check submission df
      head(submission)
      # submission file
      write.csv(submission, "saibal_submission.csv", row.names = FALSE)



# We can also store the auto generated ARIMA in a value  and use it 
best_model <- auto.arima(train_ts)
#   -OR-
best_model <- auto.arima(train_ts, ic = "aic", trace = TRUE)


# check the Model  (generated automatically - it will show the p.d.q selected)
best_model

# also now check the ACF and PACF of the auto generated model - most lines should be within both BLUE lines
acf(ts(best_model$residuals))
pacf(ts(best_model$residuals))
  

      #Then use this for Prediction at CI of 95% using level = c(95)  and "h" for the next future time periods
      ### here used 80% confidence interval (CI)
      hlt.pred <- forecast(best_model, level = c(95),    h=113)
      hlt.pred
      
      # plotting the forecast
      plot(data.frame(hlt.pred)["Point.Forecast"][,1])
      
      # Submission dataframe with 2 columns
      submission <- data.frame(test,data.frame(hlt.pred)["Point.Forecast"][,1])
      # Rename the columns to match sample submission format
      names(submission) = c("date", "mean_demand_kiloGallon")
      # Check submission df
      head(submission)
      # submission file
      write.csv(submission, "saibal_submission.csv", row.names = FALSE)

      
# after finding the prediction plot it to see if the  Forecast is also Stationary or not    
plot (hlt.pred)

# Finally do a Ljung box test to see p-value > 0.05 at various lags (i.e. p-values should not be significant)
# if p-value < 0.05 means that data is still having auto correlation
Box.test(hlt.pred$residuals, lag =5 , type='Ljung-Box')
Box.test(hlt.pred$residuals, lag =10 , type='Ljung-Box')
Box.test(hlt.pred$residuals, lag =15 , type='Ljung-Box')
Box.test(hlt.pred$residuals, lag =20 , type='Ljung-Box')



## Working on the BATS() model 

pred9_bats_train_ts <- bats(train_ts)
summary(pred9_bats_train_ts)
checkresiduals(pred9_bats_train_ts)


### Prediction using BATS() model  to predict for next 113 days

      pred9_bats <- predict(pred9_bats_train_ts,h = 113, level = c(95), prediction.interval = TRUE)
      #  OR
      pred9_bats <- predict(pred9_bats_train_ts,h = 113,  prediction.interval = TRUE)
      pred9_bats
      MAPE(pred9_bats_train_ts$mean, 113) * 100
      
      # plotting the forecast
      plot(data.frame(pred9_tbats)["Point.Forecast"][,1])
      
      # Submission dataframe with 2 columns
      submission <- data.frame(test,data.frame(pred9_tbats)["Point.Forecast"][,1])
      
      # Rename the columns to match sample submission format
      names(submission) = c("date", "mean_demand_kiloGallon")
      
      # Check submission df
      head(submission)
      
      ## Here changing the column names as an extra column was populated because of BATS() method
      ## dropping extra column and renaming
      submission <- submission[,-c(2)]
      head(submission)
      submission[2]
      colnames(submission)[2]='mean_demand_kiloGallon'
      head(submission)
      # writing submission file to local
      write.csv(submission, "saibal_submission.csv", row.names = FALSE)
      

      
## Working on the TBATS() model 
      
pred7_tbats_train_ts <- tbats(train_ts)
summary(pred7_tbats_train_ts)
checkresiduals(pred7_tbats_train_ts)

      


      
### Prediction using TBATS() model  to predict for next 113 days
      
      pred7_tbats <- predict(pred7_tbats_train_ts,h = 113, level = c(80), prediction.interval = TRUE)
      #  OR
      pred7_tbats <- predict(pred7_tbats_train_ts,h = 113,  prediction.interval = TRUE)
      pred7_tbats
      MAPE(pred7_tbats_train_ts$mean, 113) * 100
      
      # plotting the forecast
      plot(data.frame(pred7_tbats)["Point.Forecast"][,1])
      
      # Submission dataframe with 2 columns
      submission <- data.frame(test,data.frame(pred7_tbats)["Point.Forecast"][,1])
      
      # Rename the columns to match sample submission format
      names(submission) = c("date", "mean_demand_kiloGallon")
      
      # Check submission df
      head(submission)
      
      # writing submission file to local
      write.csv(submission, "saibal_submission.csv", row.names = FALSE)      
      
      
      
      
## Working with log() model 
## log() model can done on the same already applied models - by adding log() to the time series converted ones      
      
      pred11_tbats_train_ts_log <- tbats(log(train_ts))
      summary(pred11_tbats_train_ts_log)
      checkresiduals(pred11_tbats_train_ts_log)
      accuracy(pred11_tbats_train_ts_log)
      
      
      
      
      ### Prediction using TBATS() model  to predict for next 113 days
      
      pred11_tbats_log <- predict(pred11_tbats_train_ts_log,h = 113, level = c(95), prediction.interval = TRUE)
      #  OR
      pred11_tbats_log <- predict(pred11_tbats_train_ts_log,h = 113,  prediction.interval = TRUE)
      pred11_tbats_log
      MAPE(pred11_tbats_train_ts_log$mean, 113) * 100
      
      # plotting the forecast
      plot(data.frame(pred11_tbats_log)["Point.Forecast"][,1])
      
      # Submission dataframe with 2 columns
      submission <- data.frame(test,data.frame(pred11_tbats_log)["Point.Forecast"][,1])
      
      # Rename the columns to match sample submission format
      names(submission) = c("date", "mean_demand_kiloGallon")
      
      # Check submission df
      head(submission)
      
      # writing submission file to local
      write.csv(submission, "saibal_submission.csv", row.names = FALSE)      
      
      
      
      
      
## Working on the Facebook's prophet() model 
      
      install.packages("prophet")
      library("prophet")
      
      head(train)
      head(train_ts)
      
      # prophet() model uses the "ds" and "y" in prophet() 
      ds <- train$date
      y <- train$mean_demand_kiloGallon
      df <- data.frame(ds,y)
      head(df)
      
      #  finding the best fit using prophet()
      m <- prophet(df, daily.seasonality = TRUE)
      m

### Prediction using prophet() model  to predict for next 113 days
      
      pred8_prophet_train_ts <- make_future_dataframe(m, periods = 113)
      summary(pred8_prophet_train_ts)
      
      pred8_prophet_train_ts_forecast <- predict(m, pred8_prophet_train_ts)
      
      # yhat is the predicted value
      tail(pred8_prophet_train_ts_forecast[c('yhat')])
      tail(pred8_prophet_train_ts_forecast)
      pred8_prophet_train_ts_forecast[c('yhat')]
      
      temp1 <- head(pred8_prophet_train_ts_forecast,113)
      dim(temp1)
      head(temp1,3)
      temp1 <- subset(temp1, select = -c(ds,trend,additive_terms))	
      head(temp1,3)
      
      write.csv(temp1,"temp1.csv")
      
      
###   manually extract the 'yhat' column from this temp file and put into submission file and check
      
      
     
## Working with STL() Model 
## Pull out the seasonal, trend, and irregular components from the time series 
## But it will work only with univariate data - so here it wont be useful      
pred10_stl_train_ts <- stl(train_ts, s.window = "daily")      
      
     

## trying with nnetar (neural network)
fit <- nnetar(train_ts)
fcast <- forecast(fit)
plot(fcast)
  
## Arguments can be passed to nnet()
fit <- nnetar(train_ts, decay=0.5, maxit=150)
plot(forecast(fit))
lines(train_ts)



## Fit model to first 100 years of lynx data
fit <- nnetar(window(train_ts), decay=0.5, maxit=150)
plot(forecast(fit,h=14))
lines(train_ts)
head(fit)

## Apply fitted model to later data, including all optional arguments
fit2 <- nnetar(window(train_ts,start=2018), model=fit)




# Calling Auto Arima on log() of the Time Series converted Model
m_arima <- auto.arima((train_ts))


checkresiduals(m_arima_auto)



# Now, Predicting using the ARIMA model (derived from AUto Prediction)
# forecast() is used to predict the forecast

#Prediction 
hlt.pred <- forecast(m_arima, h=113)
accuracy(hlt.pred)

# plotting the forecast
plot(data.frame(hlt.pred)["Point.Forecast"][,1])
accuracy(hlt.pred)

# Submission dataframe with 2 columns
submission <- data.frame(test,data.frame(hlt.pred)["Point.Forecast"][,1])
# Rename the columns to match sample submission format
names(submission) = c("date", "mean_demand_kiloGallon")
# Check submission df
head(submission)
# submission file
write.csv(submission, "saibal_submission.csv", row.names = FALSE)






# Trying with stlf() method as ets() wont work for data with frequency greater than 24 
#  Seasonality will be ignored. Try stlf() if you need seasonal forecasts.

    stlf_train_ts <- stlf((train_ts))
    summary(stlf_train_ts)
    accuracy(stlf_train_ts)


    # Predicting using stlf() method for next 113 days
    
    pred12_stlf_forecast_train_ts <- predict(stlf_train_ts,h = 113, prediction.interval = TRUE)
    pred12_stlf_forecast_train_ts
    
    MAPE(pred12_stlf_forecast_train_ts$mean, 113) * 100
    accuracy(pred12_stlf_forecast_train_ts)
    
    # plotting the forecast
    plot(data.frame(pred12_stlf_forecast_train_ts)["Point.Forecast"][,1])
    
    # Submission dataframe with 2 columns
    submission <- data.frame(test,data.frame(pred12_stlf_forecast_train_ts)["Point.Forecast"][,1])
    
    # Rename the columns to match sample submission format
    names(submission) = c("date", "mean_demand_kiloGallon")
    
    # Check submission df
    head(submission)
    
    # writing submission file to local
    write.csv(submission, "saibal_submission.csv", row.names = FALSE)


    
    
    

    

##################################################

## Final Conclusion : stlf() method on time series converted data came out to be the best as of now

##################################################

    
    

## Forecasting using ses() Forecasting Method
    
    pred6_snaive <- snaive((train_ts))  
    summary(pred6_snaive)  
    accuracy(pred6_snaive)
    
    ### Prediction Using Simple Exponential Model  
    
    pred6_ses <- predict(  mstl((train_ts))  ,h = 113, prediction.interval = TRUE)  
    pred6_ses <- forecast.HoltWinters(train_ts, h=113)
    pred6_ses
    accuracy(pred6_ses)
    
    # plotting the forecast
    plot(data.frame(pred6_ses)["Point.Forecast"][,1])
    
    # Submission dataframe with 2 columns
    submission <- data.frame(test,data.frame(pred6_ses)["Point.Forecast"][,1])
    
    # Rename the columns to match sample submission format
    names(submission) = c("date", "mean_demand_kiloGallon")
    
    # Check submission df
    head(submission)
    
    # writing submission file to local
    write.csv(submission, "saibal_submission.csv", row.names = FALSE)    
    
    
    
    
    install.packages("prophet")
    library(prophet)
    stats <- train
    head(stats)
    colnames(stats) <- c('ds', 'y')
    head(stats)
    m <- prophet(stats)
    future <- make_future_dataframe(m, periods = 113)
    forecast <- predict(m, future)  
    plot(m, forecast)
    tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
    prophet_plot_components(m, forecast)
    final_prophet <- prophet_plot_components(m, forecast)
    forecast[c('ds','yhat')]
    dim(forecast)
    
    
    
    fizzBuzz <- function(n) 
    {
      for (i in 1:n)
      {
        if (i%%3 == 0 && i%%5 == 0  )
        {
          print "FizzBuzz"
        }
        else if (i%%3 ==0  )
        {
          print "Fizz"
        }  
        else if (i%%5 ==0  )
        {
          print "Buzz"
        }     
        else 
        {
          print i
        }                 
      }
      
    }
    
    
    