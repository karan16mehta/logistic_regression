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

# code for merging state-gov and local-gov into SL-gov
groupunemp <- function(job){
  job <- as.character(job)
  if (job =="State-gov"| job=="Local-gov") {
    return("SL-gov")
    
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,groupunemp)

#code for merging self employed jobs and self employed inc into self-emp

groupselfemp <- function(job){
  job <- as.character(job)
  if (job=="Self-emp-inc" | job =="Self-emp-not-inc") {
    return("self-emp")
    
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,groupselfemp)


#look at marital column
table(adult$marital)

# reducing marital column into 3 groups married, not married, never married

gmaritals <- function(mar){
  mar <- as.character(mar)
  if (mar=="Separated"|mar=="Divorced"|mar=="Widowed") {
return("Not-married")    
  }
  else if(mar=="Never-married"){
    return(mar)
  } else{return("Married")}
} 


adult$marital <- sapply(adult$marital,gmaritals)

table(adult$marital)

# checking country column 
table(adult$country)

str(adult$country)
levels(adult$country)
unique(adult$country)
levels(as.factor(adult$country))
# grouping the country in according to the continent


Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')




continent <- function(con){
  
  if(con %in% Asia){
    return("Asia")
  }
  else if (con %in% North.America){
    return("North America")
  }else if (con %in% Europe){return("Europe")}
  else if (con %in% Latin.and.South.America){
    return("Latin and South America")
  }else{
    return("Other")
  }
}


adult$country <- sapply(adult$country,continent)
table(adult$country)

# converting the columns in we changes into factors

str(adult)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$marital <- sapply(adult$marital,factor)
adult$country <- sapply(adult$country,factor)

# Missing Data
install.packages("Amelia")
library(Amelia)

# converting ? to NA
adult[adult=="?"] <- NA
table(adult$type_employer)


str(adult)

adult$occupation <- sapply(adult$occupation,factor)

# finding the missing values

missmap(adult)
# getting rid of Y lable
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#droping na values
adult <- na.omit(adult)

missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# EDA

library(ggplot2)
library(dplyr)

# plotting histogram of ages,colored by income

ggplot(adult, aes(age))+geom_histogram(aes(fill=income), color='black',binwidth = 1) + theme_bw()


# histogram of hours worked per week

ggplot(adult,aes(hr_per_week))+geom_histogram()+theme_bw()

# changing the name of country column to Region
adult <- adult %>% rename(Region=country)

ggplot(adult, aes(Region))+geom_bar(aes(fill = income))+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# buildng logistic model

head(adult,6)

# loading catool library for spliting the data into test and train

library(caTools)

#setting seed
set.seed(101)

#splitting the sample
sample <- sample.split(adult$income,SplitRatio = .70)

train <- subset(adult,sample==TRUE)
test <- subset(adult,sample==FALSE)


model = glm(income~.,family = binomial(logit),data=train)
summary(model)


# we will use step method to remove non significant predictor variable

model2 <- step(model)


# prediciting the test with model 1 
test$predincome <- predict(model,newdata = test, type='response')

# confusion matrix
table(test$income,test$predincome >0.5)

# accuracy
(6372+1423)/(6372+548+872+1423)
