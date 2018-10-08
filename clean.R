
##################################################################
# Use this block comment at the top of each R code file you submit
# IST 687 Homework 6 – Submitted by Ryan French on October 11, 2018
# Portions of this code came from Introduction to Data Science
# but the comments are all original.

#import necessary libraries
library(tibble)
library(dplyr)
library(ggplot2)


#STEP 1
#read in census dataset
#URL for population data
urlToRead <- ("https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/scprc-est2017-18+pop-res.csv")
#read in data from URL
dfStates <- read.csv(url(urlToRead), stringsAsFactors = FALSE)

#Remove unnecesary rows
dfStates <- dfStates[-c(1, 53),]
#Remove unnecesary columns
dfStates <- dfStates[,-c(1:4)]

#utilize colnames to change the names of columns in dfStates
cnames <- colnames(dfStates)
cnames[1] <- "stateName"
cnames[2] <- "population"
cnames[3] <- "popOver18"
cnames[4] <- "percentOver18"

colnames(dfStates) <- cnames
colnames(dfStates)

#copy USArrests dataset to local variable
arrests <- USArrests
#convert row names of states to new column for merging
arrests <- arrests %>% rownames_to_column("stateName")

#merge the two data frames
common_col_names <- intersect(names(arrests), names(dfStates))
allStateData <- merge.data.frame(arrests, dfStates, by=common_col_names, all.x=TRUE)



#STEP 2
#create histogram for the population
popHistogram <- ggplot(allStateData, aes(x=population))
popHistogram <- popHistogram +geom_histogram(binwidth=5000000, color="black", fill="white")
popHistogram <- popHistogram + ggtitle("states population histogram")
popHistogram

#create histogram for murder count
murderHistogram <- ggplot(allStateData, aes(x=Murder))
murderHistogram <- murderHistogram +geom_histogram(binwidth=1, color="black", fill="white")
murderHistogram <- murderHistogram + ggtitle("states murders histogram")
murderHistogram

#create boxplot for population
popBoxplot <- ggplot(allStateData, aes(x=factor(0),population)) + geom_boxplot()
popBoxplot

#create histogram for murder count
murderBoxplot <- ggplot(allStateData, aes(x=factor(0),Murder)) + geom_boxplot()
murderBoxplot

#I would consider the histogram to be more helpful in these cases
#as the bars of the histogram appear to be more intuitively digestable
#than the quartile system of the box plot



#STEP 3
#calculate the number of murders per state
murdersPerState <- allStateData$Murder * allStateData$population / 100000
allStateData <- cbind(allStateData, murdersPerState)

#build a barplot for the murders per state
murdersPerStateBar <- ggplot(allStateData,aes(x=stateName, y=murdersPerState, group=1))
murdersPerStateBar <- murdersPerStateBar + geom_col()
murdersPerStateBar <- murdersPerStateBar
murdersPerStateBar

#adjust the barplot to make state names readable
murdersPerStateBar2 <- ggplot(allStateData,aes(x=stateName, y=murdersPerState, group=1))
murdersPerStateBar2 <- murdersPerStateBar2 + geom_col()
murdersPerStateBar2 <- murdersPerStateBar2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
murdersPerStateBar2

#adjust the barplot to sort the x axis by the murder rate
murdersPerStateBar3 <- ggplot(allStateData,aes(x=reorder(stateName,murdersPerState), y=murdersPerState, group=1, fill=percentOver18))
murdersPerStateBar3 <- murdersPerStateBar3 + geom_col()
murdersPerStateBar3 <- murdersPerStateBar3 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
murdersPerStateBar3



#STEP 4
#generate a scatter plot where x=population, y=percent over 18, and size & color=murder rate
popStatsScatter <- ggplot(allStateData, aes(x=population, y=percentOver18))
popStatsScatter <- popStatsScatter + geom_point(aes(size=murdersPerState,color=murdersPerState))
popStatsScatter








