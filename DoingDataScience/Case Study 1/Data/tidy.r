source("gather.r") #Read r code from gather.r

#########################################
#Read FEDSTATS Dataset into datframe
fedstats.raw <- read.csv("FEDSTATS_Country.raw.csv",header=TRUE)

#Names of Columns
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
gdp.raw <- read.csv("FGDP.raw.csv",header=TRUE,skip=3)

#Names of Columns
names(gdp.raw)

#Return Dimensions (rows,columns)
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
