source("Analysis/analysis1.r") #calling r code from analysis1.r file

#merge data on country shortcode
merge.gdp.fedstats <- merge(gdp,fedstats,by="CountryCode")

#Remove any Rows With NA's
merge.data<-merge.gdp.fedstats[!(is.na(merge.gdp.fedstats$Ranking) | merge.gdp.fedstats$Ranking==""), ]

head(merge.data,5)
