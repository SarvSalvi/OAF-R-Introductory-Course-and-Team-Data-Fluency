# R Lesson 2 Exercise:
# Written by: Sarv Salvi
# March 9, 2018

## Initializing and loading packages:
# Cleaning out everything that was in the global environment
rm(list=ls())

# Before loading dplyr package you need to make sure that you have already downloaded it and installed it on your computer.
# If you have done this before skip this step the package only needs to be installed once.  If not go over to your console and type in "install.packages('dplyr')"
# exactly as it is without the quotation marks.

#Loading the dplyr package after it has already been installed:
library(dplyr)


# Setting the directory in which I will find the dataset.
setwd ("C:/Users/bsalv/Documents/Work/One Acre Fund/R Lessons/Lesson 2")

# Reading the season clients that I have saved to the directory above.:
SC <- read.csv("Season Clients Light_20180308-100714.csv")


## Pipes:  %>%  This symbol which is used below is called "pipes", and it is possible to use only after loading the dplyr package.
## It basically allows you to perform a series of commands on a dataset without repeatedly referencing the dataset on which the 
## commands are performed.  Please note that it is not neccesary to use %>% to run commands from dplyr like select, filter etc.
## It is just something that helps with readability of code and doing many manipulations one after another.  An example of how
## you could write the code below without using %>% is SubsetSC <- select(SC, RegionName, DistrictName, SiteName, GroupName, OAFID, TotalCredit, TotalRepaid)
## Note that the first term in select is now the original dataset which you are trying to manipulate (SC).


# Practice with Select and Pipes:
SubsetSC <- SC %>%
  select(RegionName, DistrictName, SiteName, GroupName, OAFID, TotalCredit, TotalRepaid)


#Dropping 1 column:
SubsetSC$OAFID <- NULL

## Practice with Filter: Filter allows you to drop all observations (rows) that do not meet the condition you have specified.
## In this case the filter condition is removing all observations NOT from Western.
SubsetSC <- SubsetSC %>%
  filter(RegionName == "Western Province")


# Mutate:  creating new variables using mutate (TotalRemaining and uniquesite, in this example)
SubsetSC <- SubsetSC %>%
  mutate(TotalRemaining = TotalCredit - TotalRepaid) %>%
  mutate(uniquesite = paste(DistrictName, SiteName, sep = "" ))


# Summarize: + Group by: Using these two functions toghether can be very helpful.  SubsetSC right now is a dataframe that has 
# only clients that are from Western province.  It has individual client data but we want to know some summary statistics at the 
# site level.  Therefore the code first groups people by people having the same answer to uniquesite. Then it calclates some summary 
# statistics at the grouping level, in this case site.  This means that at the end our dataset called SiteSummary will only have 
# 1 row per site instead of 1 row per individual.
SiteSummary <- SubsetSC %>% 
  group_by(uniquesite) %>% 
  summarise(sitecredit = sum(TotalCredit), 
            siterepaid = sum(TotalRepaid),
            siteremaining = sum(TotalRemaining),
            siterepaidper = siterepaid/sitecredit*100,
            meancredit = mean(TotalCredit),
            mincredit = min(TotalCredit),
            maxcredit = max(TotalCredit),
            middle = median(TotalCredit),
            numclients = n(),
            DistrictName = first(DistrictName))


