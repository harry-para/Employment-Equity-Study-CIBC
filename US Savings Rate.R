### Macroeconomic Forecasting: US Savings Rate

# Clear all
  rm(list=ls())

  setwd("~/Harry's Project Docs/US Savings Rate Project")

############################################################################################################

## Importing Data and Libraries
  
  library(repo)

  data = read.csv("usa_cy.csv")

## Convert data frame into time series vectors
  for(i in 1:dim(data)[2])
    assign(names(data)[i],window(ts(data[,i], 
                                    frequency = 4, 
                                    start = c(1945,1), 
                                    end=c(2015, 4)),
                                 start = c(1961,1),
                                 end = c(2007,4)))
  
  n = length(dateid01)

############################################################################################################

## Question 8.1 - Plotting logged variables of interest

  # Plots the log of Real Consumption (rc) divided by Real Disposable Income (rdy) next to the log of Real Net Worth (rnw) divided by Real Disposable Income (rdy)
  
  par(mar=c(5,4,4,5)+.1)
  
  # Time series plot: log of Real Consumption (rc) divided by Real Disposable Income (rdy)
    plot.ts(log(rc/rdy), 
            type="l", 
            col="red")
  
  # Time series plot: log of Real Net Worth (rnw) divided by Real Disposable Income (rdy)
    plot.ts(log(rnw/rdy), # logs variables within brackets
            type="l", 
            col="blue",
            xaxt="n", yaxt="n",xlab="",ylab="")
    # Puts y-axis scale
    axis(4)
    
    # labels y-axis
    mtext("log(rnw/rdy)",side=4,line=3)

############################################################################################################
  
## Question 8.2
  
  # Plot the residuals of a simple regression of the log of Real Consumption on the log of Real Net Worth to see how they change over time.
    
    fit = lm(log(rc/rdy) ~ log(rnw/rdy))
    residuals = ts(fit$residuals,frequency = 4, start = c(1961,1))
    plot.ts(residuals)
    abline(h=0)
    
    model <- lm(log(rc/rdy) ~ log(rnw/rdy))
    summary(model)
  
############################################################################################################
  
## Question 8.3 - Structural Break Representation 
  
  # Sets up a dummy variable to represent the structural break.
    
    n = length(dateid01)
    zeros = (1975-1961)*4 + 3
    sb_1975_4 = ts(c(rep(0,zeros), rep(1, n-zeros)),frequency = 4, start = c(1961,1))
    plot(sb_1975_4)
    
    model <- lm(log(rc/rdy) ~ log(rnw/rdy) + sb_1975_4) 
    summary(model)
  
############################################################################################################

## Question 8.4 & 8.5 - Unit Root Tests using Augmented Dickey-Fuller (ADF) Test
  
  # Tests for unit roots of Real Consumption, Real Disposable Income, and Real Net Worth. Tests for stationarity using the Augmented Dickey-Fuller Test. To automatically select the number of lags add the argument:
    # selectlags = c("Fixed", "AIC", "BIC")
    
  # Results vary slightly from EViews
    
    library("urca")
    
    # Tests for unit root in levels and includes an intercept.
    summary(ur.df(y = log(rc), type = "drift", lags = 2))
    
    summary(ur.df(y = log(rdy), type = "drift", lags = 0))
    
    summary(ur.df(y = log(rnw), type = "drift", lags = 4))
    
    library(tseries)
  
    # Augmented Dickey-Fuller test
    adf.test(log(rc), alternative="stationary")
  
############################################################################################################
  
## Question 8.6 - Vector Auto-Regressive (VAR) model & Cointegration Tests
  
  # Selects the correct number of lags for the VAR model given a variety of selection criteria.
  # (Results vary slightly from EViews)
    
    library(vars)
 
    # Each stat: AIC (Akaike Information Criterion), HQ(n) (Hannan-Quinn information criterion), SC(n) (Schwartz Criterion) & FPE(n) (Final Prediction Error) -> indicates best lag to use
    # EViews would show a table with all lags and summary stats, R shows individual lags
    # Here, best number of lags is 2.
    
    VARselect(cbind(log(rc),log(rdy),log(rnw)), # y = endogenous variables, combined here using cbind (column bind)
              # lag.max = Integer for the highest lag order (default is lag.max = 10)
              type="const", # type = Type of deterministic regressors to include. Can include one of "const", "trend", "both", "none" at a time.
              season = NULL,  # season = Inlusion of centered seasonal dummy variables (integer value of frequency). Default is NULL.
              exogen = data.frame(sb_1975_4 = sb_1975_4))$selection # exogen = Inlusion of exogenous variables. Dummy variable (sb_1975_4) is included here. Default is NULL.
    
    ## Cointegration Test 1: Maximum Eigenvalue test
      # y = the response
      # K = number of lags, minimum should be 2
      # type = type of test conducted
      # ecdet = constant model
      coint_test <- ca.jo(cbind(log(rc),log(rdy),log(rnw)), K = 2, type = "eigen", ecdet = "const", spec = "transitory")
      coint_test # Gives 3 values for 3 variables observed
      
      coint_test@teststat[2] # Test statistics H0 r=0, to be rejected
      coint_test@teststat[1] # Test statistics H0 r=1, should not be rejected
      
      coint_test@cval # Finds the critical values
      
      # If values in test stat is greater than critical values at i.e. 5% significance level, we reject null saying there is cointegration
      # If values in test stat is less than critical value, we fail to reject null and say there is cointegration BUT there is only 1 cointegration vector.
      # Cointegration implies a long-run relationship
      
    ## Cointegration Test 2: Johansen Trace Test -> should present same results as Maximum Eigenvalue test
      # y = the response
      # K = number of lags, minimum should be 2
      # type = type of test conducted
      # ecdet = constant model
      coint_test <- ca.jo(cbind(log(rc),log(rdy),log(rnw)), K = 2, type = "trace", ecdet = "const", spec = "transitory")
      coint_test # Gives 3 values for 3 variables observed
      
      coint_test@teststat[2] # Test statistics H0 r=0, to be rejected
      coint_test@teststat[1] # Test statistics H0 r=1, should not be rejected
      
      coint_test@cval # Finds the critical values
      
      # If values in test stat is greater than critical values at i.e. 5% significance level, we reject null saying there is cointegration
      # If values in test stat is less than critical value, we fail to reject null and say there is cointegration BUT there is only 1 cointegration vector.
      # Cointegration implies a long-run relationship
  
############################################################################################################
  
## Question 8.7 - 8.9
  
  # Set up the VAR model and run a series of tests for autocorrelation, heteroskedasticity, and normality.
  # (The tests in the VAR package are different from E-Views)
    
    # p indicates number of lags.
    SimpleVar = VAR(cbind(log(rc),log(rdy),log(rnw)),
                    p = 2,
                    type="trend",
                    exogen = data.frame(sb_1975_4 = sb_1975_4))
    summary(SimpleVar)
      
    serial.test(SimpleVar)
  
    arch.test(SimpleVar,4)  
    
    normality.test(SimpleVar)
  
  # Forecasting Real Consumption with VAR model and 95% confidence intervals.
    VARForecast = predict(SimpleVar,n.ahead = 27,dumvar=data.frame(sb_1975_4 = rep(1,27)), ci = .95)
    plot(VARForecast)
  
  # Impulse Response of Real Consumption to a shock in Real Disposable Income.
    VARForecast = irf(SimpleVar, impulse = "log.rdy.", n.ahead = 27, ci = .95)
    plot(VARForecast)
  
  
############################################################################################################
  
## Question 8.10
  
  # In order to determine the number of cointegrating vectors to use when estimating the Vector Error Correcting Model (VECM), we must run the Johansen Cointegration Test. 
  # (Result is different from EViews)
    
    VECMTest = ca.jo(cbind(log(rc),log(rdy),log(rnw)), type="eigen", ecdet = "const", K = 2, dumvar = data.frame(sb_1975_4 = sb_1975_4))
    summary(VECMTest)
  
############################################################################################################

## Question 8.11
  
  # Test the change in Unemployment (unemp) and the log of Consumer Sentiment (consumer_sentiment) for stationarity using the Augmented Dickey-Fuller Test.
    
    summary(ur.df(y = diff(unemp), type = "drift", selectlags = "AIC"))
  
    summary(ur.df(y = log(consumer_sentiment), type = "drift", selectlags = "AIC"))  

############################################################################################################

## Question 8.12
  
  # Using the Johansen Cointegration Test object, VECM model can be constructed.
    
    difUnem = window(diff(ts(data$unemp, frequency = 4, start = c(1945,1))),
                     start = c(1961,1),
                     end = c(2007,4))
    VECMTest = ca.jo(cbind(log(rc),log(rdy),log(rnw)), type="eigen", ecdet = "const", K = 2, dumvar = data.frame(sb_1975_4 = sb_1975_4, difUnem = difUnem, consConf = log(consumer_sentiment)))
    VECMSimple = cajorls(VECMTest, 1)
    VECMSimple

    
    
    
## Conclusion: The final crisis did not result in a permanent change in consumption in the US.
## This is seen when out of sample forecasting is done for the US savings rate. Forecast is the same as the actual values.