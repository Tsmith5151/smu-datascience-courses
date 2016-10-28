#Load R Libraries:
library(plyr)
library(downloader)
library(ggplot2)

#########################################

#Download Data GDP Ranking:
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url,destfile="FEDSTATS_Country.raw.csv") #download file and save it to Data directory

#Download Educ Data
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url,destfile="FGDP.raw.csv") #download file and save it to Data directory
