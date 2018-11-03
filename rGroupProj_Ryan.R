setwd("F://wqd7004/nyse-financial-stocks/")
getwd()

install.packages("dplyr")
library(dplyr)


files <- list.files(pattern = "*.csv")

stockCol <- c("Date", "Financial Institution", "Location", "High", "Low", "Open", "Close", 
              "Aggregated data 2 days", "Aggregated data 3 days", "Aggregated data 5 days",
              "Volume", "Number of employees", "Net change 0 (numeric)", "Net change 0 (Nominal)",
              "Net change 5 (numeric)", "Net change 5 (nominal)", "Net change 25 (numeric)", 
              "Net change 25 (nominal)")

stockDf <- do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE,
                                                             header = TRUE, col.names = stockCol)))

companyDetails <- read.csv('../securities.csv', stringsAsFactors = FALSE)

#craete a subset of date feature 
date_df <- stockDf$Date
#define start and end date 
which(date_df == "30/11/2012")
startdate <-  which(date_df == "30/11/2012")

startdate
date_df[[191]]
