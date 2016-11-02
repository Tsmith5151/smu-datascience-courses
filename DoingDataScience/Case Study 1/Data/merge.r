source("Data/tidy.r") #Read R code from tidy.r

#merge data on country shortcode
merge.gdp.fedstats <- merge(gdp,fedstats,by="CountryCode")

#Change the order of the columns:
merge.gdp.fedstats <-merge.gdp.fedstats[c(1,3,5,4,6,2)]

#replace any blank observations with N/A
merge.gdp.fedstats[merge.gdp.fedstats == ""] <- NA

#Remove any Rows Wth NA's
merge.data.final<-merge.gdp.fedstats[!(is.na(merge.gdp.fedstats$Ranking)), ]

#Return the number of N/A per column
merge.na <- colSums(is.na(merge.data.final))
merge.na

# Display the first 5 rows for the final merged data set
head(merge.data.final,5)
