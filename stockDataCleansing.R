# R group project

# Reading the 20 source files at once
setwd('./nyse-financial-stocks/')
files <- list.files(pattern = "*.csv")
# The column names for the imported stock datasets
stockCol <- c("Date", "Stock", "Location", "High", "Low", "Open", "Close", 
              "Aggregated data 2 days", "Aggregated data 3 days", "Aggregated data 5 days",
              "Volume", "Number of employees", "Net change 0-numeric", "Net change 0-nominal",
              "Net change 5-numeric", "Net change 5-nominal", "Net change 25-numeric", 
              "Net change 25-nominal")
# Read the 20 source files and merge them into a single dataframe
stockDf <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE,
                                                             header = TRUE, col.names = stockCol)))

# The column names for the imported company dataset
companyCol <- c('Stock', 'Company', 'SEC filings', 'GICS Sector', 'GICS Sub Industry', 'HQ Address',
                'Date first added', 'CIK')
companyDetails <- read.csv('../securities.csv', stringsAsFactors = FALSE, col.names = companyCol)

# Check the structure and dimension of the data
head(stockDf)
dim(stockDf)
str(stockDf)
summary(stockDf)

head(companyDetails)
dim(companyDetails)
str(companyDetails)

# View the data to get a big picture
View(stockDf)
View(companyDetails)

# Fix the date format in the dataset
library(lubridate)
library(dplyr)
stockDf$Date <- dmy(stockDf$Date)

companyDetails$Date.first.added <- dmy(companyDetails$Date.first.added)

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

# Check the structure of the dataset after conversion
str(stockDf)
glimpse(stockDf)

str(companyDetails)
glimpse(companyDetails)

# Check if there is NA in the dataset
sum(is.na(stockDf))
sum(is.na(companyDetails))

# Check unique stock
unique(stockDf$Stock)
length(unique(stockDf$Stock))

unique(companyDetails$Stock)
length(unique(companyDetails$Stock))


# Function  to get stock details
getCompanyDetails <- function(tickerSym) {
  stock <- companyDetails[which(companyDetails$Stock==tickerSym),]
  return(stock)
}

getCompanyName <- function(tickerSym) {
  companyName <- companyDetails[which(companyDetails$Stock==tickerSym),]$Company
  return(companyName)
}

getCompanyDetails(tickerSym = 'AXP')
getCompanyDetails(tickerSym = 'BAC')
getCompanyDetails('C')

getCompanyName(tickerSym = 'AXP')
getCompanyName(tickerSym = 'ABT')
getCompanyName(tickerSym = 'ALB')

# Merge the 2 dataset
stockData <- left_join(stockDf, companyDetails, by = c('Stock'))
colnames(stockData)
dim(stockData)
View(stockData)

# Remove unneeded columns
stockData <- select(stockData, c("Date", "Stock", "High", "Low", "Open", "Close", 
                                 "Volume", "Company", "GICS.Sector", "GICS.Sub.Industry"))
colnames(stockData)
# Rename the columns
names(stockData) <- c("Date", "StockCode", "High", "Low", "Open", "Close", "Volume", "Company", "Sector", "Sub-industry")
colnames(stockData)
View(stockData)

# Check the number of NAs
sum(is.na(stockData)) # No NA after removing unnecessary columns

# Create new columns from the existing columns
stockData <- stockData %>% mutate(OpenCloseDiff = Open - Close, HighLowDiff = High - Low)
colnames(stockData)
glimpse(stockData)

# Visualize some of the data
plotClosePrice <- function(code) {
  subStock <- stockData %>% filter(StockCode==code, Date >= dmy('01-01-2017'))
  plot(x = subStock$Date, y = subStock$Close, main=paste("Closing price for", getCompanyName(code)),
       xlab="Year 2017", ylab="Closing Price, $", type='l')
}

plotClosePrice(code="BAC")

# Converting dataframe to timeseries and get the monthly returns
library(xts)
library(quantmod)

getMonthlyReturn <- function(code) {
  df <- stockData %>% filter(StockCode==code, Date >= dmy('01-01-2017')) %>% 
    select(Date, High, Low, Open, Close, Volume)
  ts <- xts(df[,-1], order.by = df$Date) 
  monthly <- monthlyReturn(ts, type='log')
  return(monthly)
}

jpmMonthly <- getMonthlyReturn('JPM')
head(jpmMonthly)
bacMonthly <- getMonthlyReturn('BAC')

plotMonthlyReturns <- function(monthly, code) {
  plot(monthly, main=paste("Monthly Returns for", getCompanyName(code)), ylab = "Monthly returns, $", type='l')
}
plotMonthlyReturns(jpmMonthly, 'JPM')
plotMonthlyReturns(bacMonthly, 'BAC')
