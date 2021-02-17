#Import Libraries
library(ggplot2)
library(tidyverse)
rm(list=ls()) # Remove all variables

#------------------
# Data Preparation
#------------------

#Read datasets
#Set working directory if needed setfwd()
train <- read.csv("Credit_train.csv")
test <- read.csv("Credit_test.csv")

#Rows and Cols
dim(train)
dim(test)

#Columns name
colnames(train)
colnames(test)

#Show  
head(train)
head(test)

#------------------------------------------------------
# Data Exploration - Univariate analysis - Categorical
#------------------------------------------------------

#Statistics
summary(train)
summary(test)

#Credit Default
ggplot(train, aes(x = DEFAULT)) + 
  geom_bar(fill = "darkgreen", color= "azure4") + 
  theme_minimal() + 
  labs(x = "DEFAULT", title = "Credit Default", subtitle = "train") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggplot(test, aes(x = DEFAULT)) +
  geom_bar(fill = "brown", color= "azure4") +
  theme_minimal() +
  labs(x = "DEFAULT", title = "Credit Default", subtitle = "test") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#BUSTYPE
ggplot(train, aes(x = BUSTYPE)) + 
  geom_bar(fill = "darkgreen", color= "azure4") + 
  theme_minimal() + 
  labs(x = "BUSTYPE", title = "Type of small business", subtitle = "train") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggplot(test, aes(x = BUSTYPE)) + 
  geom_bar(fill = "brown", color= "azure4") + 
  theme_minimal() + 
  labs(x = "BUSTYPE", title = "Type of small business", subtitle = "test") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#----------------------------------------------------
# Data Exploration - Univariate analysis - Numerical
#----------------------------------------------------

# MAXLINEUTIL
boxplot(train$MAXLINEUTIL, main="Maximum number of lines being utilized", sub="train"
        , xlab="MAXLINEUTIL", col="darkgreen")
boxplot(test$MAXLINEUTIL, main="Maximum number of lines being utilized", sub="test"
        , xlab="MAXLINEUTIL", col="brown")

hist(train$MAXLINEUTIL, main="Maximum number of lines being utilized", sub="train"
     , xlab="MAXLINEUTIL", breaks=50, col="darkgreen")
hist(test$MAXLINEUTIL, main="Maximum number of lines being utilized", sub="test"
     , xlab="MAXLINEUTIL", breaks=50, col="brown")


# DAYSDELQ
boxplot(train$DAYSDELQ, main="Number of delinquent days", sub="train"
        , xlab="DAYSDELQ", col="darkgreen")
boxplot(test$MAXLINEUTIL, main="Number of delinquent days", sub="test"
        , xlab="DAYSDELQ", col="brown")

hist(train$DAYSDELQ, main="Number of delinquent days", sub="train"
     , xlab="DAYSDELQ", breaks=50, col="darkgreen")
hist(test$DAYSDELQ, main="Number of delinquent days", sub="test"
     , xlab="DAYSDELQ", breaks=50, col="brown")


# TOTACBAL
boxplot(train$TOTACBAL, main="Total balance of business account", sub="train"
        , xlab="TOTACBAL", col="darkgreen")
boxplot(test$TOTACBAL, main="Total balance of business account", sub="test"
        , xlab="TOTACBAL", col="brown")

hist(train$TOTACBAL, main="Total balance of business account", sub="train"
     , xlab="TOTACBAL", breaks=50, col="darkgreen")
hist(test$TOTACBAL, main="Total balance of business account", sub="test"
     , xlab="TOTACBAL", breaks=50, col="brown")

