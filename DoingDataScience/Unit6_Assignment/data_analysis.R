# Author: Trace Smith

library(plyr)
library(gdata)
setwd("/Users/tracesmith/Desktop/SMU/Github/DoingDataScience/Unit6_Assignment")

#Save as CSV File
bk <- read.csv("Dataset/rollingsales_queens.csv",skip=4,header=TRUE)

## Check the data
head(bk)
summary(bk)
str(bk) # Very handy function!

