#install.packages("stringr")
install.packages("reshape")
library(stringr)
library(ggplot2)
library(reshape)

# Function to take a dataframe with $X and $StartDate 
# fields and find the rides per day from that DF
ridesPerDay <- function(df) {
  rides<-length(df$X)
  perday<- rides/ length(unique(df$StartDate))
  return(perday)
}


## Import Data ##
setwd("G:\\GradSchool\\DataScience\\Project\\data") ## Change this to proper dir
alldata <- read.csv("bikeData2018.csv") ## Change this to your data file name

## Separate day and time fields and delete original fields ##
startd<-str_split_fixed(alldata$Start.date, " ",2)
alldata$StartDate <- startd[,1]
alldata$StartTime <- startd[,2]
endd <- str_split_fixed(alldata$End.date, " ", 2)
alldata$EndDate <- endd[,1]
alldata$EndTime <- endd[,2]
alldata <- within(alldata,rm(Start.date, End.date))
alldata <- alldata[c(1,7,9,10,11,12,2,4,3,6,5,8)] #rearrange columns

## Specify your time frame and send to ridesPerDay function ##
df <- subset(alldata, grepl("2018-01", StartDate)) # this will do January 2018
rpd<-ridesPerDay(df)


## Run this for multiple time periods and then graph them ##

#get data for each desired month
jan <- subset(alldata, grepl("2018-01", StartDate))
mar <- subset(alldata, grepl("2018-03", StartDate))
may <- subset(alldata, grepl("2018-05", StartDate))
jul <- subset(alldata, grepl("2018-07", StartDate))
sep <- subset(alldata, grepl("2018-09", StartDate))
nov <- subset(alldata, grepl("2018-11", StartDate))
janrides<-ridesPerDay(jan)
marrides<-ridesPerDay(mar)
mayrides<-ridesPerDay(may)
julrides<-ridesPerDay(jul)
seprides<-ridesPerDay(sep)
novrides<-ridesPerDay(nov)


#make list to store these in
rpdlist <- list()
rpdlist[["Jan"]] <- janrides
rpdlist[["Far"]] <- marrides
rpdlist[["May"]] <- mayrides
rpdlist[["Jul"]] <- julrides
rpdlist[["Sep"]] <- seprides
rpdlist[["Nov"]] <- novrides
barplot(unlist(rpdlist))

## example line graph ##
months <- c('jan', 'mar', 'may', 'jul', 'sep', 'nov')
rides <- c(janrides, marrides, mayrides, julrides, seprides, novrides)
df2 <- data.frame(months, rides)
df2$months <- as.character(df2$months)
df2$months <- factor(df2$months, levels=unique(df2$months))
df2melt <- melt(df2, id.vars="months")
ggplot(df2melt, aes(x=months, y=value, color=variable, group=1)) + geom_line()

