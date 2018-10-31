# R group project

# Reading the 20 source files
setwd('./nyse-financial-stocks/')
files <- list.files(pattern = "*.csv")
colNames <- c("Date", "Financial Institution", "Location", "High", "Low", "Open", "Close", 
              "Aggregated data 2 days", "Aggregated data 3 days", "Aggregated data 5 days",
              "Volume", "Number of employees", "Net change 0 (numeric)", "Net change 0 (Nominal)",
              "Net change 5 (numeric)", "Net change 5 (nominal)", "Net change 25 (numeric)", 
              "Net change 25 (nominal)")
stockDf <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE,
                  header = TRUE, col.names = colNames)))

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

getCompanyDetails(tickerSym = 'AXP')
getCompanyDetails(tickerSym = 'BAC')
getCompanyDetails('C')

# Create a new column for company name
# This method, merge the 2 datasets, need to remove alot of columns
mergeData <- merge(stockDf, companyDetails, by.x = c('Financial.Institution'), by.y = c('Ticker.symbol'))
View(mergeData)


stockDf$Company <- lapply(stockDf$Financial.Institution, getCompanyDetails(tickerSym = stockDf$Financial.Institution))

getCompanyDetails(tickerSym = 'AXP')$Security
mutate(stockDf, getCompanyDetails(stockDf$Financial.Institution)$Security)
