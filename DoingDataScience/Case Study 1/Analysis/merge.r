source("Analysis/tidy.r") #Read r code from analysis1.r

#merge data on country shortcode
merge.gdp.fedstats <- merge(gdp,fedstats,by="CountryCode")

#replace any blank observations with N/A
merge.gdp.fedstats[merge.gdp.fedstats == ""] <- NA

#The number of N/A per column
merge.na <- colSums(is.na(merge.gdp.fedstats))
print("Number of Obersvations per column with N/A:")
merge.na


#Remove any Rows Wth NA's
merge.data.final<-merge.gdp.fedstats[!(is.na(merge.gdp.fedstats$Ranking)), ]

head(merge.data.final,5)
