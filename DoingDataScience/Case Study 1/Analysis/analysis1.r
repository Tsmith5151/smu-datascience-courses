#Merge Dataset
gdp.raw <- read.csv("Data/FEDSTATS_Country.csv",header=TRUE)
educ.raw <- read.csv("Data/FGDP.csv",header=TRUE)

names(gdp.raw)
names(educ.raw)