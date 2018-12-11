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

levels(stockDf$Net.change.0.nominal)
levels(stockDf$Net.change.5.nominal)
levels(stockDf$Net.change.25.nominal)

levels(stockDf$Net.change.0.nominal) <- c("Unknown", "Equal", "Negative", "Positive")
levels(stockDf$Net.change.5.nominal) <- c("Unknown", "Equal", "Negative", "Positive")
levels(stockDf$Net.change.25.nominal) <- c("Unknown", "Equal", "Negative", "Positive")

levels(companyDetails$SEC.filings)
levels(companyDetails$GICS.Sector)
levels(companyDetails$GICS.Sub.Industry)

# Check the structure of the dataset after conversion
str(stockDf)
glimpse(stockDf)

str(companyDetails)
glimpse(companyDetails)

# Check if there is NA in the dataset
sum(is.na(stockDf))
sum(is.na(companyDetails))

# Find out where is the NA
stockDf[!complete.cases(stockDf),]
sapply(stockDf, function (x) sum(is.na(x)))
nrow(stockDf[!complete.cases(stockDf),])
View(stockDf[!complete.cases(stockDf),])

# Most of the NAs are from the column Net.change.25.numeric
sum(is.na(stockDf$Net.change.25.numeric))
# Fill in the NAs with the mean from the column
stockDf$Net.change.25.numeric[is.na(stockDf$Net.change.25.numeric)] <- mean(stockDf$Net.change.25.numeric, 
                                                                            na.rm=T)
# Another column with NAs
sum(is.na(stockDf$Net.change.5.numeric))
stockDf$Net.change.5.numeric[is.na(stockDf$Net.change.5.numeric)] <- mean(stockDf$Net.change.5.numeric, 
                                                                            na.rm=T)
# Removed all the NAs from the stock dataframe
sum(is.na(stockDf))

# Find the NAs in the company dataframe
companyDetails[!complete.cases(companyDetails),]
sapply(companyDetails, function (x) sum(is.na(x)))
nrow(companyDetails[!complete.cases(companyDetails),])
View(companyDetails[!complete.cases(companyDetails),])
sum(is.na(companyDetails$Date.first.added))

# Replace the NAs in the date column with a date
companyDetails$Date.first.added[is.na(companyDetails$Date.first.added)] <- dmy('01-01-1970')

# Removed all the NAs in the company data frame
sum(is.na(companyDetails))

# Check unique stock
unique(stockDf$Stock)
length(unique(stockDf$Stock))

unique(companyDetails$Stock)
length(unique(companyDetails$Stock))

# Function  to get company details with the stock code
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

# Merge the 2 dataset with left join with the similar column Stock
stockData <- left_join(stockDf, companyDetails, by = c('Stock'))
colnames(stockData)
dim(stockData)
View(stockData)

# Select the columns for further analysis
stockData <- select(stockData, c("Date", "Stock", "High", "Low", "Open", "Close", 
                                 "Volume", "Company", "GICS.Sector", "GICS.Sub.Industry", 
                                 "Net.change.0.numeric", "Net.change.0.nominal"))
colnames(stockData)
dim(stockData)

# Rename the columns to intuitive names
names(stockData) <- c("Date", "StockCode", "High", "Low", "Open", "Close", "Volume", "Company", 
                      "Sector", "SubIndustry", "NetChange", "NetChangeNominal")
colnames(stockData)
View(stockData)

# Create new columns from the existing columns
stockData <- stockData %>% mutate(HighLowDiff = High - Low)
colnames(stockData)
glimpse(stockData)

# Cleansing END. The data is cleaned and merged

############# Exploratory Data Analysis #################
library(ggplot2)
library(gridExtra)

# Function to get year range for the plot functions
getYear <- function(year) {
  vec <- c()
  if (year == '2013') {
    vec <- c('2013', '01-01-2013', '31-12-2013')
  } else if (year == '2014') {
    vec <- c('2014', '01-01-2014', '31-12-2014')
  } else if (year == '2015') {
    vec <- c('2015', '01-01-2015', '31-12-2015')
  } else if (year == '2016') {
    vec <- c('2016', '01-01-2016', '31-12-2016')
  } else if (year == '2017') {
    vec <- c('2017', '01-01-2017', '31-12-2017')
  } else {
    vec <- c()
  }
  
  return (vec)
}
# Closing price of a certain stock
plotClosePrice <- function(code, year='2017') {
  yearVec <- getYear(year)
  subStock <- stockData %>% filter(StockCode==code, Date >= dmy(yearVec[2]) & Date <= dmy(yearVec[3]))
  ggplot(data = subStock, aes(x=Date, y=Close)) +
    geom_line(color='Blue') +
    ggtitle(paste('Closing Price for', getCompanyName(code))) +
    xlab(yearVec[1]) +
    ylab('Closing Price, $')
}

plotClosePrice(code="BAC", year='2016')

# High Low difference of a stock
plotHighLowDiff <- function(code, year='2017') {
  yearVec <- getYear(year)
  subStock <- stockData %>% filter(StockCode==code, Date >= dmy(yearVec[2]) & Date <= dmy(yearVec[3]))
  ggplot(data=subStock, aes(x=Date, y=HighLowDiff)) +
    ggtitle(paste('High-Low Difference for', getCompanyName(code))) +
    geom_line(color='blue') +
    xlab(yearVec[1]) +
    ylab('High-Low Difference, $')
}

plotHighLowDiff(code='BAC', year='2016')

# Volume
plotVolume <- function(code, year='2017') {
  yearVec <- getYear(year)
  substock <- stockData %>% filter(StockCode==code, Date >= dmy(yearVec[2]) & Date <= dmy(yearVec[3]))
  ggplot(data=substock, aes(x=Date, y=Volume)) +
    geom_line(color='blue') +
    ggtitle(paste('Volume of', getCompanyName(code))) +
    xlab(yearVec[1]) +
    ylab('Volume')
    # scale_y_continuous(labels = scales::comma)
}

plotVolume(code='BAC', year='2017')

# Comparing the year on year closing price
BACClo2013 <- plotClosePrice(code = 'BAC', year = '2013')
BACClo2014 <- plotClosePrice(code = 'BAC', year = '2014')
BACClo2015 <- plotClosePrice(code = 'BAC', year = '2015')
BACClo2016 <- plotClosePrice(code = 'BAC', year = '2016')
BACClo2017 <- plotClosePrice(code = 'BAC', year = '2017')
grid.arrange(BACClo2013, BACClo2014, BACClo2015, BACClo2016, BACClo2017, ncol=2)
# Closing price rises at the end of the years

# Comparing the year on year sales volume
BACVol2013 <- plotVolume(code = 'BAC', year = '2013')
BACVol2014 <- plotVolume(code = 'BAC', year = '2014')
BACVol2015 <- plotVolume(code = 'BAC', year = '2015')
BACVol2016 <- plotVolume(code = 'BAC', year = '2016')
BACVol2017 <- plotVolume(code = 'BAC', year = '2017')
grid.arrange(BACVol2013, BACVol2014, BACVol2015, BACVol2016, BACVol2017, ncol=2)
# Volume drop at the end of the years

# Comparing High low differences
BACPri2013 <- plotHighLowDiff(code = 'BAC', year = '2013')
BACPri2014 <- plotHighLowDiff(code = 'BAC', year = '2014')
BACPri2015 <- plotHighLowDiff(code = 'BAC', year = '2015')
BACPri2016 <- plotHighLowDiff(code = 'BAC', year = '2016')
BACPri2017 <- plotHighLowDiff(code = 'BAC', year = '2017')
grid.arrange(BACPri2013, BACPri2014, BACPri2015, BACPri2016, BACPri2017, ncol=2)


# Exploring the relationship between High and Volume
BACStock <- stockData %>% subset(Date >= dmy('01-01-2017'))
cor(BACStock$Volume, BACStock$Close)
cor(BACStock$Volume, BACStock$High)
ggplot(BACStock, aes(x=High, y=Volume)) +
  geom_line(color='blue')

cor(BACStock$High, BACStock$Close)
cor(BACStock$Close, BACStock$Low)
cor(BACStock$High, BACStock$Low)
ggplot(BACStock, aes(x=Close, y=High)) +
  geom_point(color='blue')
ggplot(BACStock, aes(x=Close, y=Low)) +
  geom_point(color='blue')
ggplot(BACStock, aes(x=High, y=Low)) +
  geom_point(color='blue')
# There is a corelation between High, Close, Low