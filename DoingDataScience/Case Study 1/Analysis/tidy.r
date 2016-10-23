#Load R Libraries:
library(plyr)
library(downloader)
library(ggplot2)

#########################################

#Download Data GDP Ranking:
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url,destfile="Data/FEDSTATS_Country.raw.csv") #download file and save it to Data directory

#Download Educ Data
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url,destfile="Data/FGDP.raw.csv") #download file and save it to Data directory

#########################################
#Read FEDSTATS Dataset into datframe
fedstats.raw <- read.csv("Data/FEDSTATS_Country.raw.csv",header=TRUE)

#Names of Columns
print("Column Names of fedstats.raw Dataset:")
names(fedstats.raw)

#Dimensions (row,columns)
dim(fedstats.raw)

#Create New DataFrame From Raw
fedstats<-fedstats.raw

#Drop column index 4 thru the number of columns in the df
fedstats[4:ncol(fedstats)] <-NULL

#Display the first 5 rows of dataframe
head(fedstats,5)

#The number of N/A per column
colSums(is.na(fedstats))

#########################################
#Read GDP Dataset into dataframe
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

#Select Certain row and all columns; 
#not including the section of text below the data and world GDP info
gdp<-gdp[(2:216),] #select rows 2:215 and all columns (,)

#Rename Column
colnames(gdp)[1] <- "CountryCode"
colnames(gdp)[4] <- "GDP"


# pattern "[^[:digit:]]" refers to members of the variable name that start with digits. 
# gsub command to replace them with a blank space
# convert variables to numeric 
gdp$GDP <- as.numeric(gsub("[^[:digit:]]","", gdp$GDP))
gdp$Ranking <- as.numeric(gsub("[^[:digit:]]","", gdp$Ranking))

#Check everything looks correct:
head(gdp,5)

#The number of N/A per column
colSums(is.na(gdp))

#########################################
