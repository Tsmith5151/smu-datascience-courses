# Live Unit 11 Assignment
library("fpp")

##a.) Plot the time series. Can you identify seasonal fluctuations and/or a trend?

#plot the monthly sales of homes:
plot(hsales, col="red", lwd=1.5, ylab = "Monthly Sales", xlab="Date",
     main="Monthly Sales of \n 1-Family Houses in USA since 1973")

##b.) Use a classical decomposition to calculate the trend-cycle and seasonal indices.

#Decompose a time series into seasonal, trend and irregular 
#components using moving averages.
data <- decompose(hsales)

# Plot the estimated trend, seasonal, and irregular components
plot(data, col="blue")

##d) Compute and plot the seasonally adjusted data.
  
#Decompose a time series into seasonal, trend and irregular components using:
#Returns seasonally adjusted data constructed by removing the seasonal component.
fit <- stl(hsales, s.window=5)
plot(fit, main="Seasonal Adjusted Data",col="blue")


#Plot of hsales with the seasonal adjusted data (red line):
plot(hsales, col="gray",lwd=2, main="Sales of US Single Family Housing\n
     with Seasonal Adjusted Data (red line)", ylab="New Single Family House Sales", xlab="Years")

lines(fit$time.series[,2],col="red",ylab="Trend",lwd=2.5)

##e) Change one observation to be an outlier (e.g., add 500 to one observation), 
#and recompute the seasonally adjusted data. What is the effect of the outlier?

#add outlier to data
#ts is used to create time-series objects
hsales3 <- ts(c(hsales[1:275],hsales[5]+100), end=c(1995,11),
              start=c(1973,1),frequency=15)

plot(hsales3,col="purple",lwd=1.5,
     main="Modify 'hsales' Data with Outlier",
     xlab = "Time", ylab="Monthly Sales")

##f) Does it make any difference if the outlier is near the end rather than in the middle of 
#the time series?
  
#Step 1: Create an outlier near the end: 
hsales4 <- ts(c(hsales[1:260],hsales[261]+500, hsales[262:275]),start=c(1973,1),frequency=12)

#Plot the time series: 
plot(hsales4, col="orange",main="Sales of US One Family Housing\n(Outlier Close to End)",
     ylab="New Single Family House Sales", xlab="Years")


##g) Now use STL to decompose the series.
  
#Step G-1: Plot Seasonal Adjusted Data with Outlier at End:
fit4 <- stl(hsales4, s.window=5)
plot(fit4, col="blue",main="Seasonal Adjusted Data\n(Outlier Close to End)")


#Plot of hsales3 with the seasonal adjusted data(purple line), fit3
plot(hsales4, col="gray",lwd=2,main="Sales of US Single Family Housing(Outlier Clost to End)
\nwith Seasonal Adjusted Data (purple line)",
     ylab="New Single Family House Sales", xlab="Years")
lines(fit4$time.series[,2],col="purple",ylab="Trend",lwd=2.5)
