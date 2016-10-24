# Case Study 1
**MSDS 6306 DOING DATA SCIENCE**

<p align="center">
<img src= "http://sportspagedfw.com/wp-content/uploads/2013/03/SMU-Logo1.jpg">
</p>

#### Data Source
- Load the Gross Domestic Product data for the 190 ranked countries in this data set:
https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv

- Load the educational data from this data set:
https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv

#### Task: Tidy, Merge, and Perform Statistical Analysis
- Merge the data based on the country shortcode. How many of the IDs match?
- Sort the data frame in ascending order by GDP (so United States is last). What is the 13th country in the resulting data frame?
- What are the average GDP rankings for the "High income: OECD" and "High income: nonOECD" groups?
- Plot the GDP for all of the countries. Use ggplot2 to color your plot by Income Group.
- Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group.
- How many countries are Lower middle income but among the 38 nations with highest GDP?

#### Deliverables: 
- An introduction to the case study and the objective of this work.
- Code for downloading, tidying, and merging data in a R Markdown file. The code is in a make file style, meaning that the source RMD document pulls in separate files for importing data, cleaning the data, and data analysis.
- A brief explanations of the purpose of the code. The explanation is given in a sentence or two before or after the code chunk. Note, even though the code chunks are not hidden, pretend that the client can’t see them.
- Code to answer the five questions above (plus the answers) in the same R Markdown file.
- Clear answers to the questions identified above
- Conclusion to the project. Summary of the findings from this exercise.

#### File Structure
 The main source code can be executed from the Analysis directory by running [analysis.r](https://github.com/Tsmith5151/DataScience-SMU/blob/master/DoingDataScience/Case%20Study%201/Analysis/analysis.r). This file performs several analysis on the clean data set and is linked to other R files as follow:
 
 - [gather.r](https://github.com/Tsmith5151/DataScience-SMU/blob/master/DoingDataScience/Case%20Study%201/Data/gather.r): downloads the data from the internet
 
 - [tidy.r](https://github.com/Tsmith5151/DataScience-SMU/blob/master/DoingDataScience/Case%20Study%201/Data/tidy.r): takes the two data frames and cleans the rows/columns to prepare for statistical analysis
 
 - [merge.r](https://github.com/Tsmith5151/DataScience-SMU/blob/master/DoingDataScience/Case%20Study%201/Data/merge.r): merges two data frames utilizing the unique key identifier "CountryCode"

### Reference:
Adapted from the Case Study Report Help website of the University of New South Wales School of Engineering: https://student.unsw.edu.au/writing-case-study
