#########################################
########### Questions ###################
#########################################

source("Analysis/merge.r")

#Question:1 Number of Matches
print(paste0("Total Number of ID Matches: ",
             length(intersect(fedstats$CountryCode,gdp$CountryCode))))

#Question:2 Sort GDP - Descending Order (13th Rank)
sort.gdp <-merge.data.final[order(merge.data.final$GDP,decreasing=FALSE,na.last = TRUE),][12:13,]
sort.gdp[,c("Long.Name","Ranking","GDP")]

#Question:3 Avg. GDP Ranking Group By Income.Group
merge.data.agg <- ddply(merge.data.final, .(Income.Group), summarize,  Ranking=mean(Ranking))
#merge.data.agg <-merge.data.agg[!(is.na(merge.data.agg$Rank) | merge.data.agg$Rank==""), ]
head(merge.data.agg,5)

#Question:4 Plot GDP and group by Income.Group
p<-ggplot(merge.data.final)+ geom_point(aes(y=GDP,x=Income.Group,colour=Income.Group)) +scale_y_log10()
p+labs(title="GDP vs Income Group",
       x="Income Groups",y="Log: GDP-millions",colour="Income Group") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#Question:4 GDP Ranking into 5 Quantiles
merge.data.final$Ranking <- as.numeric(as.character(merge.data.final$Ranking))
merge.data.final$Group <- cut(merge.data.final$Ranking,breaks=5)
quant.table<-table(merge.data.final$Income.Group, merge.data.final$Group)
quant.table
