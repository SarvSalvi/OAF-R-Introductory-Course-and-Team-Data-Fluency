# R Lesson 5 Exercise: Data Cleaning
# Written by: Sarv Salvi
# May 3, 2018

## Initializing and loading packages:
# Cleaning out everything that was in the global environment
rm(list=ls())

# Before loading dplyr package you need to make sure that you have already downloaded it and installed it on your computer.
# If you have done this before skip this step the package only needs to be installed once.  If not go over to your console and type in "install.packages('dplyr')"
# exactly as it is without the quotation marks.

#Loading the dplyr package after it has already been installed:
library(dplyr)
library(tidyr)


# Setting the directory in which I will find the dataset.
setwd ("C:/Users/bsalv/Documents/Work/One Acre Fund/R Lessons/Lesson 5")

# Reading the season clients that I have saved to the directory above.:
SC <- read.csv("Season Clients Light_20180308-100714.csv")
Vert <- read.csv("Light_20180503-065207.csv")

# Common String Cleaning Functions
  
  # Standardizing Capitalization: This is very important becuase we will be merging two datasets later on.  In order to do this
  # correctly we will have to make sure that everything is spelled in EXACTLY the same way since merge functions are case sensitive.
  Vert$District <- tolower(Vert$District)
  Vert$Site <- toupper(Vert$Site)
  # toupper does the opposite it turns a function from lower case to upper case.  
  # There are many operations on character vectors that are possible in R.  Most will be only useful occaisonally so we have 
  # avoided going through too many in this lesson.  Googling will help you find ones that will make your life easier in your
  # project work.  Remember that sometimes you will have to install and load a new package to access some string manipulation functions.


  # Parsing Dates: Sometime it is neccesary to do operations on dates of the year.  For example later we will be finding all of the 
  # payments made by clients in each OAF site in each month of the year and then summing them.  In order to get from vertical 
  # repayment which lists all payments from each individual client to the monthly repayment for each site we will need to 
  # identify which payments belong in which months.  This means we will need to find a way to manipulate the date column in vertical repayment.
  # This example will give you a little taste of how you can manipulate dates.
  
  # There is actually a data type called "Date", that can be used to do calculations with dates that treat them as dates not as 
  # regular numbers.  However to do this we need to convert the "character" type variable with the dates from vertical repayment 
  # into a date type variable.  This involves telling R to the format of the date in the character string: 
  
  Vert$RepaymentDate <- as.Date(Vert$RepaymentDate, "%Y-%m-%d")
  Vert$MonthYear <- format(Vert$RepaymentDate, "%Y-%m")
  
  # In the statement above we have told R that in the character class variable called "RepaymentDate" the part before the first 
  # "-" in the string is the year, the one between the first and the second is the month and the one after the second is the day.
  # And that using that we would like to resave that character string as a date class variable given that it is in the specified format.


# Creating a Site level dataset using vertical repayment which lists the total repayment in each month 
SCSitesVertbyMonth <- Vert %>%
  group_by(District, Site, MonthYear) %>%
  summarise(
    MonthsRepayment = sum(Amount)
  )

view(SCSitesVertbyMonth)  ## Take a look at the dataset to verify that it looks like its supposed to.

# Long to Wide Formats: Now we before we merge this repayment data by site with roster data by site we need to change the format of the dataset we have created.
# Right now we have multiple rows for each site since there is a seperate row for each month of repayment.  This is called "long" format.
# We need to convert to a data set that has only 1 row for each site.  This means moving the repayment amount from each month to new column.
# This process is called going from long to wide.  The function "spread" can help you do this.
SCSitesVertWide <- SCSitesVertbyMonth %>%
  spread(key = MonthYear, value = MonthsRepayment)
# Note the arguement "key" indicates the variable whose values will become the new variable names

view(SCSitesVertWide)  ## You can look at the dataset to see what happened. Also if you look over to your environment you should see that
## there are 1576 observations in this dataset which makes sense becasue that is the number of sites we have this year.

# Creating a Site level dataset by grouping and summarizing by Districtname and SiteName:
SCSiteRoster <- SC %>%
  select(RegionName, DistrictName, SiteName, GroupName, OAFID, TotalCredit, TotalRepaid) %>% 
  mutate(DistrictName = tolower(DistrictName)) %>% ## So that the formatting matches the vertical repayment dataset for the merge
  mutate(SiteName = toupper(SiteName)) %>% ## So that the formatting matches the vertical repayment dataset for the merge
  group_by(DistrictName, SiteName) %>%
  summarise(
    NumClientsSite = n(), 
    TotalTransSite = sum(TotalCredit)
  )

# Merging Wide Site Level Repayment Data and Site Level Roster Data:  Here we will use left_join which is the most commonly used type of merge command
# However there are many other types of joins that can be used depending on your particular situation.  
help("left_join") #Shows the differences between the types of joins and the arguements that it takes.

SiteData <- left_join(SCSiteRoster, SCSitesVertWide, by = c("DistrictName" = "District", "SiteName" = "Site"))


# Exercise 1: Try using the gather command from tidyr to go back from wide to long.  View the dataset after you are done to see what happened.


# Checking for duplicates and Sites with no repayment data:  Two common types of problems that can be encountered in a dataset are if 
# There are duplicates or rows with no missing information in certain key rows.  Below are ways to identify whether you have thsese problems
# in your dataset.

SiteDataDuplicates <- SiteData[duplicated(SiteData$SiteName) | duplicated(SiteData$SiteName, fromLast = TRUE),]


# Exercise 2: Try figuring out what the "|" operator means as well as why I repeated the duplicated command with the fromlast command


# Exercise 3: check the dataset of duplicates, there appears to be a problem in how I checked for duplicates. What might you do to fix this problem?


# Now I will check for the sites from the roster data set that were not matched to any repayment data:
MissingRepayment <- SiteData[SiteData$`2017-10` == "NA", ]


# Exercise 5:  It is possible that there were some sites from the vertical repayment dataset that weren't in the roster dataset and
# so were not captured by the left_join.  What could we have done to check this?


 

  

  