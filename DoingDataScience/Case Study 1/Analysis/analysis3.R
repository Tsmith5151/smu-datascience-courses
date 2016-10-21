#########################################
########### Questions  ##################
#########################################

source("analysis2.r")

#Question:1 Number of Matches
print(paste0("Total Number of ID Matches: ",
             length(intersect(fedstats$CountryCode,gdp$CountryCode))))

#Question:2 Sort GDP - Descending Order (13th Rank)
sort.gdp <-merge.data[order(merge.data$GDP,decreasing=FALSE,na.last = TRUE),]
head(sort.gdp,13)

#Question:3 Avg. GDP Ranking Group By Income.Group
merge.data.agg <- ddply(merge.data, .(Income.Group), summarize,  Rank=mean(Rank))
#merge.data.agg <-merge.data.agg[!(is.na(merge.data.agg$Rank) | merge.data.agg$Rank==""), ]
merge.data.agg

#Question:4 Plot GDP and group by Income.Group
p<-ggplot(merge.data)+ geom_point(aes(y=GDP,x=Income.Group,colour=Income.Group)) +scale_y_log10()
p+labs(title="GDP vs Income Group",
       x="Income Groups",y="Log: GDP-millions",colour="Income Group") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#Question:4 GDP Ranking into 5 Quantiles
merge.data$Quant<-quantile(merge.data$Rank,seq(0, 1, 0.2))
quant.table<-table(merge.data$Income.Group, merge.data$Quant)
quant.table
