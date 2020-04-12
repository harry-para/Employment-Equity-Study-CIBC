### Rogers - Ordinal Logit Model Analysis - Form 2 Full-Time Employee Data

  # Clear all
    rm(list=ls())

############################################################################################################

## Importing Data and Libraries

  # Sets working directory (i.e. file path)
    setwd("~/ESDC ALIGN/Rogers Communications Inc")
  
  # Reads selected CSV file and stores under a dataframe name
    Rogers_Data <- read.csv("Rogers_Data.csv", header=TRUE)
  
  # Lists first 6 rows
    head(Rogers_Data)
  
  # Indicates the number of observations and number of variables
    dim(Rogers_Data)
  
  # Rserve library allows R to connect with other external software (i.e. Tableau)
    library(Rserve)
    Rserve()
    
    library(ggplot2)
    library(ggthemes)
    library(GGally)
    library("vcd")

############################################################################################################

## Data Cleaning

  # Removes missing values
    s = sum(is.na(Rogers_Data))
  
  # New dataset after removing missing values is Rogers. Done this way to avoid overwriting original file
    Rogers <- na.omit(Rogers_Data)
    
  # Renames specific column by stating the column number
    names(Rogers)[1] <- "Year"  
    
  # Filters data remove "Radio and Television Broadcasting" subsector
    library(dplyr)
    Rogers <- Rogers[Rogers$Subsector == "Wireless telecommunications carriers (except satellite)",]
    
    
############################################################################################################

## Descriptive Analyses

  head(Rogers)

  # QTR changed to numeric type for correlation calculations
    Rogers$QTR <- as.numeric(Rogers$QTR)
    is.numeric(Rogers$QTR)
    summary(Rogers$QTR)

    Rogers$All.Employees...Total <- as.numeric(Rogers$All.Employees...Total)
    is.numeric(Rogers$All.Employees...Total)
    summary(Rogers$All.Employees...Total)
    
    Rogers$Male <- as.numeric(Rogers$Male)
    is.numeric(Rogers$Male)
    summary(Rogers$Male)
    
    Rogers$Female <- as.numeric(Rogers$Female)
    is.numeric(Rogers$Female)
    summary(Rogers$Female)
    
    Rogers$Female_Dominated_Occupation <- as.numeric(Rogers$Female_Dominated_Occupation)
    is.numeric(Rogers$Female_Dominated_Occupation)
    summary(Rogers$Female_Dominated_Occupation)
    
    Rogers$Indigenous <- as.numeric(Rogers$Indigenous)
    is.numeric(Rogers$Indigenous)
    summary(Rogers$Indigenous)
    
    Rogers$PwD <- as.numeric(Rogers$PwD)
    is.numeric(Rogers$PwD)
    summary(Rogers$PwD)
    
    Rogers$RP <- as.numeric(Rogers$RP)
    is.numeric(Rogers$RP)
    summary(Rogers$RP)
    
    str(Rogers)

  # Correlation Matrix
    Corr_Matrix <- Rogers[, c(5,10,13,17)]
  # Prints the first 6 rows
    head(Corr_Matrix, 6)

    res <- cor(Corr_Matrix)
  # Rounds to 2 decimal places
    round(res, 2)

  # Correlogram Visual
    library(corrplot)
    corrplot(res, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45)
  
  # QTR changed back to factor type
    Rogers$QTR = factor(Rogers$QTR, levels = c("1", "2", "3", "4"), ordered = TRUE) 
    is.factor(Rogers$QTR)
    summary(Rogers$QTR)
  
  # Histograms
  # Format of code: data, x-axis var, fill colour by, chart type, manual colour hexadecimal specifications for filling, axis labels
    ggplot(Rogers, aes(All.Employees...Total, fill = QTR)) +
      geom_histogram() + scale_fill_manual(values=c("#00CCFF", "#00CC66", "#FFCC00", "#FF3300")) + 
      labs(x = 'All Employees', y = 'Frequency') + 
      theme_classic2()
  
    ggplot(Rogers, aes(Female_Dominated_Occupation, fill = QTR)) +
      geom_histogram() + scale_fill_manual(values=c("#00CCFF", "#00CC66", "#FFCC00", "#FF3300")) + 
      labs(x = 'Female Dominated Occupation', y = 'Frequency') + 
      theme_classic2()
    
    ggplot(Rogers, aes(Female, fill = QTR)) +
      geom_histogram() + scale_fill_manual(values=c("#00CCFF", "#00CC66", "#FFCC00", "#FF3300")) + 
      labs(x = 'Female Gender Status', y = 'Frequency') + 
      theme_classic2()
    
    ggplot(Rogers, aes(Indigenous, fill = QTR)) +
      geom_histogram() + scale_fill_manual(values=c("#00CCFF", "#00CC66", "#FFCC00", "#FF3300")) + 
      labs(x = 'Indigenous Status', y = 'Frequency') + 
      theme_classic2()
    
    ggplot(Rogers, aes(PwD, fill = QTR)) +
      geom_histogram() + scale_fill_manual(values=c("#00CCFF", "#00CC66", "#FFCC00", "#FF3300")) + 
      labs(x = 'PwD Status', y = 'Frequency') + 
      theme_classic2()

############################################################################################################

## Box plots

  # Plotting Females by QTR and color by QTR
    library("ggpubr")
    ggboxplot(Rogers, x = "QTR", y = "All.Employees...Total", 
              color = "QTR", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#37004D"),
              # Column Order
              order = c("1", "2", "3", "4"),
              ylab = "All.Employees...Total", xlab = "QTR")
  
  # Box plot matrix with Occupation Group by Province, analzing Number of Female employees per salary QTR
    ggplot(Rogers, aes(x = QTR, y = All.Employees...Total, fill = QTR)) +   geom_boxplot(size = .75) +   
      facet_grid(Occupational.Group ~ Female_Dominated_Occupation, margins = FALSE) +   
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

############################################################################################################

## Creating Ordinal Factor Variables
    
    # Ordering the dependent variable: QTR
      Rogers$QTR = factor(Rogers$QTR, levels = c("1", "2", "3", "4"), ordered = TRUE) 
      
    # Ordering the independent variables: Female_Dominated_Occupation, Female, Indigenous, PwD, RP  
      Rogers$Female_Dominated_Occupation = factor(Rogers$Female_Dominated_Occupation, levels = c("0", "1"), ordered = TRUE)
      Rogers$Female = factor(Rogers$Female, levels = c("0", "1"), ordered = TRUE)
      Rogers$Male = factor(Rogers$Male, levels = c("0", "1"), ordered = TRUE)
      Rogers$Indigenous = factor(Rogers$Indigenous, levels = c("0", "1"), ordered = TRUE)
      Rogers$PwD = factor(Rogers$PwD, levels = c("0", "1"), ordered = TRUE)
      Rogers$RP = factor(Rogers$RP, levels = c("0", "1"), ordered = TRUE)
      Rogers$Year = factor(Rogers$Year, levels = c("2015", "2016", "2017"), ordered = TRUE)
    
  # Summary statistics
    summary(Rogers)
  
  # Indicates the number of observations and number of variables
    dim(Rogers)
  
  # Class of object, which is dataframe
    class(Rogers)
  
  # Lists variable types
    str(Rogers)

############################################################################################################

## Exploratory Data Analysis
  
  # Frequency Tables
    table(Rogers$QTR, Rogers$Female_Dominated_Occupation)
    table(Rogers$QTR, Rogers$Female)
    table(Rogers$QTR, Rogers$Male)
    table(Rogers$QTR, Rogers$Indigenous)
    table(Rogers$QTR, Rogers$PwD)
    table(Rogers$QTR, Rogers$RP)

    
## Chi squared tests
  
  # Null Hypothesis: Factor variables are independent, Alternate Hypothesis: Factor variables are associated
  # Observing a 5% significance level
    tab <- table(Rogers$QTR, Rogers$Female_Dominated_Occupation)
    chisq.test(tab)
  
    tab
  # Scenario where test did not reject the null hypothesis, so values should be the same between the table and expected values table
    chisq.test(tab)$expected
  
############################################################################################################
  
## Partitioning Data
  
  # Dividing data into training and testing sets
  # Random sampling 
    samplesize = 0.60*nrow(Rogers)
    set.seed(100)
    index = sample(seq_len(nrow(Rogers)), size = samplesize)
    
  # Creating training and test set 
    Rogerstrain = Rogers[index,]
    Rogerstest = Rogers[-index,]
    
    
############################################################################################################

## Ordinal Logit Model 1
    
    # Hess equal to true which returns hessian matrix. 
    # Returning the hessian matrix is essential to use summary function or calculate variance-covariance matrix of the fitted model.
      
      library(MASS)
    
      logmodel <- polr(QTR ~ Female + Male + Female_Dominated_Occupation + Indigenous + PwD, data = Rogerstrain, Hess = TRUE)
      summary(logmodel)
    
    # Calculates p-values for estimated coefficients and intercepts to determine statistical significance
      summary_table <- coef(summary(logmodel))
      pval <- pnorm(abs(summary_table[, "t value"]), lower.tail = FALSE) * 2
    
    # Coefficient Interpretation Example: An increase in value of Female by one unit increases the expected value of QTR in log odds by 0.40.
      summary_table <- cbind(summary_table, "p value" = round(pval,3))
      summary_table
      
      library(car)
      Anova(logmodel)
    
  ## Odds Ratios
    
    # Odds ratios with Confidence Intervals to avoid interpreting predicted coefficients that are scaled in terms of logs
      ci <- confint(logmodel)
    
    # Coefficient Interpretation Example: If a person is female, the odds of being more likely to achieve a lower salary QTR is 0.67
    # times that of a person who is not a female.
    # OR = Odds Ratio = (p/(1-p))
      exp(cbind(OR = coef(logmodel), ci))
  
  ## Classification using test set
      
    # Computes confusion matrix
      predictQTR = predict(logmodel,Rogerstest)
      table(Rogerstest$QTR, predictQTR)
      
    # Misclassification error in decimal form
      mean(as.character(Rogerstest$QTR) != as.character(predictQTR))
      
      
  ## Visuals to alternatively interpret regression results rather than observing the log odds ratio.
      
    # Plotting the effects: Female & Indigenous 
      library("effects")
      Effect(focal.predictors = "Female",logmodel)
      
      # Individual effect of Female Dominated Occupations on achieving a specific salary QTR
        plot(Effect(focal.predictors = "Female_Dominated_Occupation",logmodel))
      
      # Individual effect of Indigenous background on achieving a specific salary QTR
        plot(Effect(focal.predictors = "Indigenous",logmodel))
      
      # Joint effect of Indigenous background and Female Gender on achieving a specific salary QTR
        plot(Effect(focal.predictors = c("Female", "Indigenous"),logmodel))
      
      # Joint effect of Indigenous background and Female Dominated Occupation on achieving a specific salary QTR
        plot(Effect(focal.predictors = c("Female_Dominated_Occupation", "Indigenous"),logmodel))

############################################################################################################

## Ordinal Logit Model 2 without Female Dominated Occupation

    # Hess equal to true which returns hessian matrix. 
    # Returning the hessian matrix is essential to use summary function or calculate variance-covariance matrix of the fitted model.
    
    library(MASS)
    
    logmodel2 <- polr(QTR ~ Male + Female + Indigenous + PwD, data = Rogerstrain, Hess = TRUE)
    summary(logmodel2)
    
    # Calculates p-values for estimated coefficients and intercepts to determine statistical significance
      summary_table <- coef(summary(logmodel2))
      pval <- pnorm(abs(summary_table[, "t value"]), lower.tail = FALSE) * 2
    
    # Coefficient Interpretation Example: An increase in value of Female by one unit increases the expected value of QTR in log odds by 0.40.
      summary_table <- cbind(summary_table, "p value" = round(pval,3))
      summary_table
    
      library(car)  
      Anova(logmodel2)
    
    ## Odds Ratios
    
    # Odds ratios with Confidence Intervals to avoid interpreting predicted coefficients that are scaled in terms of logs
      ci <- confint(logmodel2)
    
    # Coefficient Interpretation Example: If a person is female, the odds of being more likely to achieve a lower salary QTR is 0.67
    # times that of a person who is not a female.
    # OR = Odds Ratio = (p/(1-p)) 
      exp(cbind(OR = coef(logmodel2), ci))
    
    ## Classification using test set
    
    # Computes confusion matrix
      predictQTR = predict(logmodel2,Rogerstest)
      table(Rogerstest$QTR, predictQTR)
    
    # Misclassification error in decimal form
      mean(as.character(Rogerstest$QTR) != as.character(predictQTR))
    
    
    ## Visuals to alternatively interpret regression results rather than observing the log odds ratio.
    
    # Plotting the effects
      library("effects")
      Effect(focal.predictors = "Female",logmodel2)
      
    # Individual effect of Female status on achieving a specific salary QTR
      plot(Effect(focal.predictors = "Female",logmodel2))
    
    # Individual effect of Indigenous background on achieving a specific salary QTR
      plot(Effect(focal.predictors = "Indigenous",logmodel2))
    
    # Individual effect of PwD status on achieving a specific salary QTR
      plot(Effect(focal.predictors = "PwD",logmodel2))
    
    # Joint effect of Indigenous background and Female Gender on achieving a specific salary QTR
      plot(Effect(focal.predictors = c("Female", "Indigenous"),logmodel2))
      
    # Joint effect of PwD status and Female Gender on achieving a specific salary QTR
      plot(Effect(focal.predictors = c("Female", "PwD"),logmodel2))
        
############################################################################################################

## Ordinal Logit Model 3 with Female Dominated Occupation, but without Female

  # Hess equal to true which returns hessian matrix. 
  # Returning the hessian matrix is essential to use summary function or calculate variance-covariance matrix of the fitted model.
  
    library(MASS)
  
    logmodel3 <- polr(QTR ~ Female + Indigenous + PwD, data = Rogerstrain, Hess = TRUE)
    summary(logmodel3)
  
  # Calculates p-values for estimated coefficients and intercepts to determine statistical significance
    summary_table <- coef(summary(logmodel3))
    pval <- pnorm(abs(summary_table[, "t value"]), lower.tail = FALSE) * 2
  
  # Coefficient Interpretation Example: An increase in value of Female by one unit increases the expected value of QTR in log odds by 0.40.
    summary_table <- cbind(summary_table, "p value" = round(pval,3))
    summary_table
  
    library(car)
    Anova(logmodel3)
  
  ## Odds Ratios
  
  # Odds ratios with Confidence Intervals to avoid interpreting predicted coefficients that are scaled in terms of logs
    ci <- confint(logmodel3)
  
  # Coefficient Interpretation Example: If a person is female, the odds of being more likely to achieve a lower salary QTR is 0.67
  # times that of a person who is not a female.
  # OR = Odds Ratio = (p/(1-p))
    exp(cbind(OR = coef(logmodel3), ci))
  
  ## Classification using test set
  
  # Computes confusion matrix
    predictQTR = predict(logmodel3,Rogerstest)
    table(Rogerstest$QTR, predictQTR)
  
  # Misclassification error in decimal form
    mean(as.character(Rogerstest$QTR) != as.character(predictQTR))
  
  
  ## Visuals to alternatively interpret regression results rather than observing the log odds ratio.
  
    library("effects")
    Effect(focal.predictors = "Female_Dominated_Occupation",logmodel3)
  
  # Individual effect of Female Dominated Occupations on achieving a specific salary QTR
    plot(Effect(focal.predictors = "Female_Dominated_Occupation",logmodel3))
  
  # Individual effect of Indigenous background on achieving a specific salary QTR
    plot(Effect(focal.predictors = "Indigenous",logmodel3))
    
  # Individual effect of PwD status on achieving a specific salary QTR
    plot(Effect(focal.predictors = "PwD",logmodel3))
  
  # Joint effect of Indigenous background and Female Gender on achieving a specific salary QTR
    plot(Effect(focal.predictors = c("Female_Dominated_Occupation", "Indigenous"),logmodel3))
    
  # Joint effect of PwD status and Female Gender on achieving a specific salary QTR
    plot(Effect(focal.predictors = c("Female_Dominated_Occupation", "PwD"),logmodel3))
  

############################################################################################################

## Model Comparisons

  library(jtools)    
  
  # Tabular Comparison
    library(huxtable)
  # Format of code: models for comparison, scaling, column names, confidence intervals
    export_summs(logmodel, logmodel2, scale = TRUE, model.names = c("logmodel", "logmodel2"), error_format = "[{conf.low}, {conf.high}]")
    export_summs(logmodel, logmodel3, scale = TRUE, model.names = c("logmodel", "logmodel3"), error_format = "[{conf.low}, {conf.high}]")
    export_summs(logmodel2, logmodel3, scale = TRUE, model.names = c("logmodel2", "logmodel3"), error_format = "[{conf.low}, {conf.high}]")
    
  
  # Visuals to compare estimated coefficients within logit models
    library(broom)
    library(broom.mixed)
    library(ggstance)
  
  # Visual for indiviual model's predicted coefficients, shows normal distributions
    plot_summs(logmodel, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9, model.names = c("logmodel"))
    plot_summs(logmodel2, scale = TRUE, plot.distributions = FALSE, inner_ci_level = .9, model.names = c("logmodel2"))
    plot_summs(logmodel3, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9, model.names = c("logmodel3"))
  
  
  # Visual for multiple models' predicted coefficients, plot shows estimate beta coefficient point and corresponding CI
  # plot.distribution = TRUE would show a normal distribution for each predicted beta coefficient
    plot_summs(logmodel, logmodel2, scale = TRUE, plot.distributions = TRUE, model.names = c("logmodel", "logmodel2"))
    plot_summs(logmodel, logmodel3, scale = TRUE, plot.distributions = TRUE, model.names = c("logmodel", "logmodel3"))
    plot_summs(logmodel2, logmodel3, scale = TRUE, plot.distributions = TRUE, model.names = c("logmodel2", "logmodel3"))
  
  
  # Visual to show predictions, only applicable for GLM models, not polyr
    effect_plot(logmodel2, pred = Female, interval = TRUE, plot.points = TRUE)
  
  
  ### FINAL APPROVED MODEL: (logmodel) -> lowest misclassification rate

############################################################################################################

## Forecasting: ARIMA (Auto Regressive Integrative Moving Average) Model
    
    # Clear all
    rm(list=ls())
    
    ## Loading Data
    
      # Reads selected CSV file and stores under a dataframe name
        Rogers_Data <- read.csv("Rogers_Data.csv", header=TRUE)
      
      # Lists first 6 rows
        head(Rogers_Data)
      
      # Removes missing values
        s = sum(is.na(Rogers_Data))
      
      # New dataset after removing missing values is Rogers. Done this way to avoid overwriting original file
        Rogers <- na.omit(Rogers_Data)
      
      # Renames specific column by stating the column number
        names(Rogers)[1] <- "Year"
      
        summary(Rogers)

    
    ## Pivoting data
      
      library(reshape)
      
      # Displays pivoted data; order gives hierarchy
        cast(Rogers, Occupational.Group ~ Province + QTR + Female)
    
    
    ## Querying
        
      library(dplyr)
      library(tidyr)
        
      # Organizing data by Year -> Occupation -> Province -> Total Female Employees   
      
      # Selecting the columns
        pivot <- Rogers %>%
          select(Year, Occupational.Group, Province, QTR, All.Employees...F)
      
        # Lists first few records
          head(pivot)
        
      # Then Group By
        pivot <- Rogers %>%
          select(Year, Occupational.Group, Province, QTR, All.Employees...F)%>%
          group_by(Year, Occupational.Group)
        
      # Then tell R how to sum the female employees and create new variable
        pivot <- Rogers %>%
          select(Year, Occupational.Group, Province, QTR, All.Employees...F)%>%
          group_by(Year, Occupational.Group)%>%
          summarise(TotalFemales = sum(All.Employees...F))
        
        head(pivot)
    
      # Lists variables types
        str(pivot)
        
      # Lists summary statistics  
        summary(pivot)
    
      # Pivot to select province and occupation  
        pivot <- pivot[pivot$Province == "Ontario",]
        pivot <- pivot[pivot$Occupational.Group == "Professionals",]
        
        summary(pivot)
        head(pivot)
        
      # Plots scatter plot across years (y-var then x-var)  
        plot(pivot$Year, pivot$TotalFemales)
        
      
    ## Auto ARIMA Modeling (finds bests ARIMA model i.e best AR, I and MA and then forecasts with it)
        
        library(tseries)
        library(forecast)
        library(ggplot2)
        
      # Creates time series data
        xi = ts(pivot$TotalFemales, frequency = 1, start = c(2015), end = c(2017))
        xi
        plot.ts(xi)
        
      # Creating auto ARIMA model  
        pi = auto.arima(xi)
        
      # ARIMA(#,#,#) -> (AR,I,MA), ar# is the auto-regressive term number, intercept should appear 
        summary(pi)
        
      # Checklist to see if time series data is good
        
        # Plot residuals of the time series data created, should have consistent variance and mean to be stationary  
          plot.ts(pi$residuals)
        
        # Good plot should resemble a 45 degree line, meaning residuals are normalized
          qqnorm(pi$residuals)
        
        # ACF (Autocorrelation) plot; should not have a structure after it
          acf(pi$residuals)
          
        # Partial ACF  
          pacf(pi$residuals)
        
        # Dickey-Fuller test -> Tests for if time series data is stationary
          adf.test(pivot, alternative="stationary", k=0)
          
        # Dickey-Fuller test -> Tests for if time series data is stationary; if alternative hypothesis is true (i.e. explosive), data is non-stationary
          adf.test(pivot, alternative="explosive", k=0)
          
        # Augmented Dickey-Fuller test
          adf.test(pivot, alternative="stationary")
        
      # Forecasting; h = # determines how many units to forecast for (i.e. years)  
        q = forecast(pi, h=3)
        q
        
      # Shows forecasted line, 80% confidence interval and 95% confidence interval
        plot(q)
        
      # Plot residuals of the forecast  
        plot(q$residuals)
      
