#Load R Libraries:
library(plyr)
library(downloader)
library(ggplot2)

#########################################

#Download Data GDP Ranking:
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url,destfile="Data/FEDSTATS_Country.raw.csv")

#Download Educational Data
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url,destfile="Data/FGDP.raw.csv")

#########################################
#Read FEDSTATS Dataset into Data Frame
fedstats.raw <- read.csv("Data/FEDSTATS_Country.raw.csv",header=TRUE)

#Names of Columns
print("Column Names of fedstats.raw Dataset:")
names(fedstats.raw)

#Dimensions
dim(fedstats.raw)

#Create New DataFrame From Raw
fedstats<-fedstats.raw

head(fedstats,5)

#########################################
#Read GDP Dataset into Data Frame
gdp.raw <- read.csv("Data/FGDP.raw.csv",header=TRUE,skip=3)

#Names of Columns
print("Column Names of gdp.raw Dataset")
names(gdp.raw)

#Dimensions
print("Total Dimensions: Rows,Columns:")
dim(gdp.raw)

#Assign Raw Data to "gdp"
gdp <-gdp.raw

#Drop Columns
gdp[6:10] <-NULL
gdp[[3]] <- NULL

#Select Certain row and all columns
gdp<-gdp[(2:216),]

#Rename Column
colnames(gdp)[1] <- "CountryCode"
colnames(gdp)[4] <- "GDP"

#Change Data Type to Numeric
gdp$GDP <- as.numeric(gsub("[^[:digit:]]","", gdp$GDP))
gdp$Rank <- as.numeric(gsub("[^[:digit:]]","", gdp$Ranking))

#Check everything looks correct:
head(gdp,5)

#########################################
