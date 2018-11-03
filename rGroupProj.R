# R group project

# Reading the 20 source files
setwd('./nyse-financial-stocks/')
files <- list.files(pattern = "*.csv")
stockCol <- c("Date", "Financial Institution", "Location", "High", "Low", "Open", "Close", 
              "Aggregated data 2 days", "Aggregated data 3 days", "Aggregated data 5 days",
              "Volume", "Number of employees", "Net change 0 (numeric)", "Net change 0 (Nominal)",
              "Net change 5 (numeric)", "Net change 5 (nominal)", "Net change 25 (numeric)", 
              "Net change 25 (nominal)")
stockDf <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE,
                  header = TRUE, col.names = stockCol)))

companyDetails <- read.csv('../securities.csv', stringsAsFactors = FALSE)

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

# Check the structure of the dataset after conversion
str(stockDf)
glimpse(stockDf)

str(companyDetails)
glimpse(companyDetails)

# Check if there is NA in the dataset
sum(is.na(stockDf))
sum(is.na(companyDetails))

# Check unique stock
unique(stockDf$Financial.Institution)
length(unique(stockDf$Financial.Institution))

unique(companyDetails$Ticker.symbol)
length(unique(companyDetails$Ticker.symbol))


# Function  to get stock details
getCompanyDetails <- function(tickerSym) {
  stock <- companyDetails[which(companyDetails$Ticker.symbol==tickerSym),]
  return(stock)
}

getCompanyName <- function(tickerSym) {
  companyName <- companyDetails[which(companyDetails$Ticker.symbol==tickerSym),]$Security
  return(companyName)
}

getCompanyDetails(tickerSym = 'AXP')
getCompanyDetails(tickerSym = 'BAC')
getCompanyDetails('C')

getCompanyName(tickerSym = 'AXP')
getCompanyName(tickerSym = 'ABT')
getCompanyName(tickerSym = 'ALB')

# Merge the 2 dataset
stockData <- merge(stockDf, companyDetails, by.x = c('Financial.Institution'), by.y = c('Ticker.symbol'))
View(stockData)
colnames(stockData)

# Remove unneeded columns
stockData <- select(stockData, c("Date", "Financial.Institution", "High", "Low", "Open", "Close", 
                                  "Volume", "Security", "GICS.Sector", "GICS.Sub.Industry"))
colnames(stockData)
names(stockData) <- c("Date", "StockCode", "High", "Low", "Open", "Close", "Volume", "Company", "Sector", "Sub-industry")
colnames(stockData)
View(stockData)

# Check the number of NAs
sum(is.na(stockData)) # No NA after removing unnecessary columns

# Visualize some of the data
plotClosing <- function(code) {
  subStock <- stockData %>% filter(StockCode==code, Date >= dmy('01-01-2017'))
  plot(x = subStock$Date, y = subStock$Close, main=paste("Closing price for", getCompanyName(code)),
       xlab="Year 2017", ylab="Closing Price, $", type='l')
}

plotClosing(code="BAC")

<<<<<<< HEAD

##---calcualting Sharpe ratio 
=======
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

<<<<<<< HEAD
##---calcualting Sharpe ratio
install.packages("dplyr")
library(dplyr)

install.packages("lubridate")
library(lubridate)

test_date <-  stockDf$Date
head(test_date)

start_date <- "30/11/2012"
end_date <-  "30/12/2016"

stockDf$Date[start_date]


||||||| merged common ancestors
##---calcualting Sharpe ratio 
=======
jpmMonthly <- getMonthlyReturn('JPM')
head(jpmMonthly)
>>>>>>> ed85dd2279ebfc4892780969e04a522ebba208f1

plotMonthlyReturns <- function(monthly, code) {
  plot(monthly, main=paste("Monthly Returns for", getCompanyName(code)), ylab = "Monthly returns, $", type='l')
}
plotMonthlyReturns(jpmMonthly, 'JPM')
>>>>>>> parent of 7cc8afd... Merge branch 'master' of https://github.com/dante825/rGroupProj



##-- Creating portfolio of selected finnacial institutions







