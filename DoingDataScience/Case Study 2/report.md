# Case Study II
Trace Smith & Damon Resnick  
November 21, 2016  



<br>

### Question 1

**Create the X matrix and print it from SAS, R, and Python.**

- SAS code

```{}
proc iml;
/*create 3x4 matrix*/
x={4 5 1 2,
   1 0 3 5,
   2 1 8 2};
run;
create mymatrix from x[colname={"a","b","c","d"}];
append from x;
close mymatrix;
proc print data=mymatrix;
run;
```

- R code


```r
mymatrix <- matrix(c(4,5,1,2,1,0,3,8,2,1,8,2),nrow=3,ncol=4)
print(mymatrix)
```

```
##      [,1] [,2] [,3] [,4]
## [1,]    4    2    3    1
## [2,]    5    1    8    8
## [3,]    1    0    2    2
```

- Python Code

```{}
import numpy as np
x = np.matrix([[4,5,1,2],[1,0,3,5],[2,1,8,2]])
print x
```

<br>

### Question 2

- **Answer the following questions for Air Products & Chemicals, Inc. stock (symbol = `ADP`):**


- **1.) Download the data.**


- **2.) Calculate log returns.**


- **3.) Calculate volatility measure.**


- **4.) Calculate volatility over entire length of series for various three different decay factors.**


- **5.) Plot the results, overlaying the volatility curves on the data, just as was done in the S&P example.**

<br>

### Question 3

- The built-in data set called `Orange` in R is about the growth of orange trees. The `Orange` data frame has 3 columns of records of the growth of orange trees.

**Variable description**

- *Tree*: an ordered factor indicating the tree on which the measurement is made. The ordering  is according to increasing maximum diameter.

- *age*: a numeric vector giving the age of the tree (days since 1968/12/31) circumference: a numeric vector of trunk circumferences (mm). This is probably “circumference at breast height”, a standard measurement in forestry.


- **a) Calculate the mean and the median of the trunk circumferences for different size of the trees. (Tree)**


- **b) Make a scatter plot of the trunk circumferences against the age of the tree. Use different plotting symbols for different size of trees.**


- **c) Display the trunk circumferences on a comparative boxplot against tree. Be sure you order the boxplots in the increasing order of maximum diameter.**


<br>

### Question 4

- **1.)	First, d ownload “Temp” data set. Find the difference between the maximum and the minimum monthly average temperatures for each country and report/visualize top 20 countries with the maximum differences for the period since 1900.**


- **2.) Select a subset of data called “UStemp” where US land temperatures from 01/01/1990 in Temp data. Use UStemp dataset to answer the followings.**

  - **a) Create a new column to display the monthly average land temperatures in Fahrenheit (°F).**
  
  - **b) Calculate average land temperature by year and plot it. The original file has the average land temperature by month.** 
  
  - **c) Calculate the one year difference of average land temperature by year and provide the maximum difference (value) with corresponding two years.**


- **3.) Download “CityTemp” data set. Find the difference between the maximum and the minimum temperatures for each major city and report/visualize top 20 cities with maximum differences for the period since 1900.**

- **4.) Compare the two graphs in (i) and (iii)  and comment it.**







