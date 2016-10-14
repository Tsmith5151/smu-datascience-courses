# Author: Trace Smith

library(plyr)
library(gdata)
setwd("/Users/tracesmith/Desktop/SMU/Intro to Data Science/Unit6")

#Save as CSV File
bk <- read.csv("rollingsales_queens.csv",skip=4,header=TRUE)

## Check the data
head(bk)
summary(bk)
str(bk) # Very handy function!

