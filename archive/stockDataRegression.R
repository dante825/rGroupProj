#### Steps below reproduces the cleaned dataset from previous activities ####
# some steps were be skipped
# some comments will be removed in the final version

setwd(".\\rGroupProj\\data\\nyse-financial-stocks")
files <- list.files(pattern = "*.csv")

stockCol <- c("Date", "Stock", "Location", "High", "Low", "Open", "Close", 
              "Aggregated data 2 days", "Aggregated data 3 days", "Aggregated data 5 days",
              "Volume", "Number of employees", "Net change 0-numeric", "Net change 0-nominal",
              "Net change 5-numeric", "Net change 5-nominal", "Net change 25-numeric", 
              "Net change 25-nominal")

stockDf <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE,
                                                             header = TRUE, col.names = stockCol)))

# The column names for the imported company dataset
companyCol <- c('Stock', 'Company', 'SEC filings', 'GICS Sector', 'GICS Sub Industry', 'HQ Address',
                'Date first added', 'CIK')
companyDetails <- read.csv('../securities.csv', stringsAsFactors = FALSE, col.names = companyCol)


# Fix the date format in the dataset
library(lubridate)
library(dplyr)
stockDf$Date <- dmy(stockDf$Date)

# Fix the number incorrectly represented as characters
library(stringr)
stockDf$Number.of.employees <- str_replace(stockDf$Number.of.employees, pattern = ',', replacement = '')
stockDf$Number.of.employees <- as.numeric(stockDf$Number.of.employees)

# Fix the string that should be formatted as factors
stockDf$Net.change.0.nominal <- as.factor(stockDf$Net.change.0.nominal)
stockDf$Net.change.5.nominal <- as.factor(stockDf$Net.change.5.nominal)
stockDf$Net.change.25.nominal <- as.factor(stockDf$Net.change.25.nominal)

companyDetails$SEC.filings <- as.factor(companyDetails$SEC.filings)
companyDetails$GICS.Sector <- as.factor(companyDetails$GICS.Sector)
companyDetails$GICS.Sub.Industry <- as.factor(companyDetails$GICS.Sub.Industry)

levels(stockDf$Net.change.0.nominal)
levels(stockDf$Net.change.5.nominal)
levels(stockDf$Net.change.25.nominal)

levels(stockDf$Net.change.0.nominal) <- c("Unknown", "Equal", "Negative", "Positive")
levels(stockDf$Net.change.5.nominal) <- c("Unknown", "Equal", "Negative", "Positive")
levels(stockDf$Net.change.25.nominal) <- c("Unknown", "Equal", "Negative", "Positive")

levels(companyDetails$SEC.filings)
levels(companyDetails$GICS.Sector)
levels(companyDetails$GICS.Sub.Industry)

companyDetails$Date.first.added[is.na(companyDetails$Date.first.added)] <- dmy('01-01-1970')

# Function  to get company details with the stock code
getCompanyDetails <- function(tickerSym) {
  stock <- companyDetails[which(companyDetails$Stock==tickerSym),]
  return(stock)
}

getCompanyName <- function(tickerSym) {
  companyName <- companyDetails[which(companyDetails$Stock==tickerSym),]$Company
  return(companyName)
}

# Merge the 2 dataset with left join with the similar column Stock
stockData <- left_join(stockDf, companyDetails, by = c('Stock'))

# Select the columns for further analysis
stockData <- dplyr::select(stockData, c("Date", "Stock", "High", "Low", "Open", "Close", 
                                 "Volume", "Company", "GICS.Sector", "GICS.Sub.Industry", 
                                 "Net.change.0.numeric", "Net.change.0.nominal"))

# Rename the columns to intuitive names
names(stockData) <- c("Date", "StockCode", "High", "Low", "Open", "Close", "Volume", "Company", 
                      "Sector", "SubIndustry", "NetChange", "NetChangeNominal")


#### Starting ARIMA Implementation ####
# testing ARIMA on BAC stock
library(MASS)
library(forecast)
library(tseries)
library(ggplot2)

full = stockData %>% filter(StockCode == "BAC") %>% dplyr::select(Date,High)

# predicting only for 2 days ahead 
train = full[1:1257,] # 1:1000
test = full[1258:1259,] # 1001:1259

# Autocorrelation And Cross-Correlation Function Estimation
acf(train$High,lag.max = 20)

# Partial Autocorrelation And Cross-Correlation Function Estimation
# test indicates data is stationary, time series data is predictable to some degree
pacf(train$High,lag.max = 20)

diffstock = diff(train$High,1)
adf.test(train$High)

# Augmented Dickey-Fuller Test to test if model is stationary
adf.test(diffstock) 

pricearima = ts(train$High, start = c(2012,11,30), frequency = 365)

# auto arima automatically tunes the model
fitStock = auto.arima(pricearima) 

fitStock
ggplot2::autoplot(pricearima,main="High Price Values for BAC")

forecastedValues = forecast(fitStock, h=2) #259
forecastedValues

# higher level view of the time series + forecast
ggplot2::autoplot(forecast(fitStock, h=2), include = 50)
# closer look at the time series + forecast
ggplot2::autoplot(forecast(fitStock, h=2), include = 10) 

# evaluating the mean percentage error of the forecasted prices
finalForecastedValues = as.numeric(forecastedValues$mean)

compare = data.frame(test$High,finalForecastedValues)
columns = c("ActualPrice","ForecastedPrice")
names(compare) = columns
percentage_error = ((compare$ActualPrice - compare$ForecastedPrice)/compare$ActualPrice)
percentage_error
mean(percentage_error) 

