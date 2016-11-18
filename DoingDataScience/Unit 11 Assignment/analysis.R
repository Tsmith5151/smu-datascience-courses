# Live Unit 11 Assignment
library("fpp")

#plot the monthly sales of homes:
plot(hsales, col="red", lwd=1.5, ylab = "Monthly Sales", xlab="Date",
    main="Monthly Sales of \n 1-Family Houses in USA since 1973")

#Decompose a time series into seasonal, trend and irregular 
#components using moving averages.
data <- decompose(hsales)

#Plot data
plot(data, col="orange")

#Plot the seasonlly adjusted data:
adj_sales <- seasadj(data)
plot(adj_sales, col="blue", lwd=1.5, ylab = "Monthly Sales", xlab="Date",
     main="Monthly Sales of \n 1-Family Houses in USA since 1973")

#add outlier to data
#ts is used to create time-series objects
hsales3 <- ts(c(hsales[1:275],hsales[5]+100), end=c(1995,11),
              start=c(1973,1),frequency=15)
plot(hsales3,col="purple",lwd=1.5,
     main="Modify 'hsales' Data with Outlier",
     xlab = "Time", ylab="Monthly Sales")

data3 <- decompose(hsales3)
adj_sales3 <- seasadj(data3)
plot(adj_sales3, col="blue", lwd=1.5, ylab = "Monthly Sales", xlab="Date",
     main="Monthly Sales of \n 1-Family Houses in USA since 1973")


#STL Decomposition
fit <- stl(hsales,s.window = 5)
plot(fit,col="blue")
plot(hsales,col="green3",main="Seasonal and Trend \n Decomposition using Loess",
     ylab="Monthly Sales",xlab="Time")
lines(fit$time.series[,2],col="red",lwd=2,ylab="Trend")

