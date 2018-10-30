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


# Exploring the data
head(stockDf)
summary(stockDf)
dim(stockDf)
str(stockDf)

# View the data to get a big picture
View(stockDf)

# change the date format in the datasets
library(lubridate)
library(dplyr)
stockDf$Date <- dmy(stockDf$Date)
str(stockDf)
glimpse(stockDf)
View(stockDf)

# Subset by year
data2012 <- subset(stockDf, stockDf$Period.Ending==ymd('2012-12-31'))
dim(data2012)
View(data2012)
