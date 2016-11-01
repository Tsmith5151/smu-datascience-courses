#########################################
########### Questions ###################
#########################################

source("../Data/gather.r") #Read R code from merge.r
source("../Data/tidy.r") #Read R code from merge.r
source("../Data/merge.r") #Read R code from merge.r

#Question:1 Number of Matches
# Returns the total mataches between fedstats and gpd when merging on CountryCode
length(intersect(fedstats$CountryCode,gdp$CountryCode))

#Question:2 Sort GDP - Descending Order (13th Rank)
# select rows 12-13 and all columns (tie between rankings)
sort.gdp <-merge.data.final[order(merge.data.final$GDP,decreasing=FALSE,na.last = TRUE),][12:13,]
sort.gdp[,c("CountryCode","Long.Name","Ranking","GDP")] #only display the identified columns


#Question:3 Avg. GDP Ranking Group By Income.Group
#Aggregrate data frame by Income.Group and take the mean rankings
merge.data.agg <- ddply(merge.data.final, .(Income.Group), summarize,  Ranking=mean(Ranking))
#merge.data.agg <-merge.data.agg[!(is.na(merge.data.agg$Rank) | merge.data.agg$Rank==""), ]
head(merge.data.agg,5)

#Question:4 Plot GDP and group by Income.Group

#Create a scatter plot using ggplot2 to plot Income.Group vs GDP (merged data frame)
p<-ggplot(merge.data.final)+ geom_point(aes(y=GDP,x=Income.Group,colour=Income.Group)) +scale_y_log10() #change y axis to log scale
p+labs(title="GDP vs Income Group", # add title
       x="Income Groups",y="Log: GDP-millions",colour="Income Group") + #name labels
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) #adjust the x-axis labels (rotate)

#create a box blot (ggplot2)
p<-ggplot(merge.data.final,aes(x=Income.Group,y=GDP))+ geom_boxplot(aes(fill=Income.Group)) +scale_y_log10() #change y axis to log scale
p+labs(title="GDP vs Income Group",
       x="Income Groups",y="Log: GDP (millions)",colour="Income Group") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + #adjust the x-axis labels (rotate)
  theme(legend.position = "none") #turn off legend

#Create a scatter plot using ggplot2 to plot Ranking vs GDP and group by Income (merged data frame)
p<-ggplot(merge.data.final)+ geom_point(aes(y=GDP,x=Ranking,colour=Income.Group)) +scale_y_log10() #change y axis to log scale
p+labs(title="GDP vs Income Group", # add title
       x="Ranking",y="Log: GDP (millions)",colour="Income Group") + #name labels
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) #adjust the x-axis labels (rotate)

#Question:4 GDP Ranking into 5 Quantiles
#convert Ranking column into numeric -- initially a factor
merge.data.final$Ranking <- as.numeric(as.character(merge.data.final$Ranking))
#divide the numeric vector into 5 break points (i.e. quantiles)
merge.data.final$Group <- cut(merge.data.final$Ranking,breaks=5)
#take the quantiles and income.group from the merge data file and create a table
quant.table<-table(merge.data.final$Income.Group, merge.data.final$Group)
quant.table
