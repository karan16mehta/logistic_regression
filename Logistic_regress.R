library(dplyr)
library(tidyr)
library(ggplot2)

# Importing the csv file

adult <- read.csv("adult_sal.csv")

# checking the first 6 rows of the dataset
head(adult,6)

# removing the index

adult <- select(adult,-1)

# checking the first 6 rows of the dataset

head(adult,6)

# checking the structure and summary of the data set

str(adult)
summary(adult)



# data cleaning
# to check the frequency of the type_employer column

table(adult$type_employer)

# combining without-pay and never-worked in single group "Unemployed"

unemp <- function(job){
  
  job <- as.character(job)
  if(job=="Never-worked" | job=="Without-pay"){
    
    return("Unemployed")
  } else{
    return(job)
  }
}

# using sapply to merge the without-pay and never worked

adult$type_employer <- sapply(adult$type_employer,unemp)

table(adult$type_employer)
