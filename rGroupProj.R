# R group project

# Reading the file
path <- file.path("E:\\academic\\mds-sem2\\WQD7004-Rprogramming\\groupProjectData\\")
fundamentalPath <- file.path(path, "fundamentals.csv")

stockDf <- read.csv(fundamentalPath)

# Exploring the data
head(stockDf)
summary(stockDf)
dim(stockDf)
str(stockDf)

# View the data to get a big picture
View(stockDf)

# Remove the first column because it is just a column number
stockDf <- stockDf[, -1]
dim(stockDf)

# Check how many rows belong to AAL
sum(stockDf$Ticker.Symbol=='AAL')

# Subset by stock
aalStock <- subset(stockDf, stockDf$Ticker.Symbol=='AAL')
dim(aalStock)
View(aalStock)

# change the date format in the datasets
library(lubridate)
library(dplyr)
stockDf$Period.Ending <- dmy(stockDf$Period.Ending)
str(stockDf)
glimpse(stockDf)
View(stockDf)

# Subset by year
data2012 <- subset(stockDf, stockDf$Period.Ending==ymd('2012-12-31'))
dim(data2012)
View(data2012)
