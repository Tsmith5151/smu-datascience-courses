source("Analysis/tidy.r") #Read r code from analysis1.r

#merge data on country shortcode
merge.gdp.fedstats <- merge(gdp,fedstats,by="CountryCode")

#replace any blank observations with N/A
merge.gdp.fedstats[merge.gdp.fedstats == ""] <- NA

#Remove any Rows Wth NA's
merge.data.final<-merge.gdp.fedstats[!(is.na(merge.gdp.fedstats$Ranking)), ]

#The number of N/A per column
merge.na <- colSums(is.na(merge.data.final))
print("Number of Obersvations per column with N/A:")
merge.na

# Display the first 5 rows for the final merged data set
head(merge.data.final,5)
