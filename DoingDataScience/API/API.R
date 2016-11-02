library(fitbitScraper)
library(ggplot2)

#Login and generate a cookie
cookie <- login(email="**********",password="******")

#Gather daily activity from account
df_data <- get_activity_data(cookie, end_date="2016-11-02")

#Tidy Data
df_data$formattedDistance <- gsub("miles","",df_data$formattedDistance) #remove "miles"
df_data$formattedDistance <- as.numeric(df_data$formattedDistance) #convert to numeric


###### Plot Distance vs Calories (Per Logged Run)
p <- ggplot(df_data, aes(x=df_data$formattedDistance,y=df_data$calories)) + 
  geom_point(color="cyan4",size=2) + ylab("Calories Burned") + xlab("Distance (Miles)") +
  geom_smooth(method='lm',se=FALSE,color="red") 
p + labs(title="Distance vs Calories Per Logged Run") +xlim(c(0,4)) +
  theme(plot.title = element_text(size=14,face="bold"))


#Gather Daily Data
#Distance
df_dist <- get_daily_data(cookie, what="distance",
            start_date="2016-07-22", end_date="2016-11-02")
#Calories
df_cal <- get_daily_data(cookie, what="caloriesBurnedVsIntake",
                          start_date="2016-07-22", end_date="2016-11-02")
#Steps
df_step <- get_daily_data(cookie, what="steps",
                          start_date="2016-07-22", end_date="2016-11-02")

# Merge Calories Burned, Steps, and Distance
merge.data <- merge(df_dist,df_cal, by="time")
merge.data <- merge(merge.data,df_step, by="time")
merge.data$caloriesIntake <- NULL # Drop calories intake column 
merge.data #merge calories burned vs distance

#Remove Rows with Zeros
##Go through each row and determine if a value is zero
merge.data <- merge.data[apply(merge.data[c(2:4)],1,function(z) !any(z==0)),] 


###### Plot Distance vs Days
p <- ggplot(merge.data) + geom_bar(aes(x=time,y=distance,fill=distance),
  stat="identity") + xlab("Date") + ylab("Distance")
p + labs(title="Daily Distance (Miles)") + 
  theme(axis.text.x=element_text(angle=60,hjust=1),legend.position = "none")

###### Plot Steps vs Days
p <- ggplot(merge.data,aes(x=time,y=steps)) + geom_line(aes(color="Fitbit"),size=.85) + 
  geom_point(color="darkcyan") +xlab("Date") + ylab("Total Steps") +
  scale_colour_manual(name='', values=c('Fitbit'='coral')) 

p + ylim(c(0,10000)) + labs(title="Daily Total Steps") + 
  theme(plot.title = element_text(size=12,face="bold"),
    axis.text.x=element_text(angle=60,hjust=1),
    legend.position = "none",
    panel.background = element_rect(fill = 'grey85'))

###### Plot Distance vs Calories (Daily)
p <- ggplot(merge.data, aes(x=distance,y=caloriesBurned)) + 
  geom_point(color="limegreen",size=2) + ylab("Calories Burned") + xlab("Distance (Miles)") +
  geom_smooth(method='lm',se=FALSE,color="blue") 
p + labs(title="Daily Distance vs Calories") +xlim(c(0,4)) + ylim(c(1700,2600)) +
  theme(plot.title = element_text(size=14,face="bold"))
