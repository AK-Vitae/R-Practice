#------------------
# Data Preparation
#------------------
#Import Libraries
#rm(list=ls()) # Remove all variables

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

#---------------------------------------------------------------------
# Data Exploration - Bivariate analysis - Categorical and Categorical
#---------------------------------------------------------------------

#DEFAULT and BUSTYPE
x <- xtabs(~BUSTYPE+DEFAULT, data=train)
plot(x, main="Type of small business", sub="train", col="darkgreen")

y <- xtabs(~BUSTYPE+DEFAULT, data=test)
plot(y, main="Type of small business", sub="test", col="brown")


#Chi Squared test
tbl <- table(train$DEFAULT, train$BUSTYPE)
chisq.test(tbl)

#-----------------------------------------------------------------
# Data Exploration - Bivariate analysis - Numerical and Numerical
#-----------------------------------------------------------------

#BUSAGE, MAXLINEUTIL, DAYSDELQ, TOTACBA
pairs(~BUSAGE+MAXLINEUTIL+DAYSDELQ+TOTACBAL, data=train, main="Scatterplot - train", col="darkgreen")

pairs(~BUSAGE+MAXLINEUTIL+DAYSDELQ+TOTACBAL, data=test, main="Scatterplot - test", col="brown")


#Correlation
train <- na.omit(train)
cor(train$DAYSDELQ,train$MAXLINEUTIL)

#Covariance and Variance
train <- na.omit(train)
cov(train$DAYSDELQ,train$MAXLINEUTIL)
var(train$DAYSDELQ,train$DAYSDELQ)

#Correlation and Covariance matrix
d <- train
d$DEFAULT <- NULL
d$BUSTYPE <- NULL
cor(d)
cov(d)

#-------------------------------------------------------------------
# Data Exploration - Bivariate analysis - Categorical and Numerical
#-------------------------------------------------------------------

#DEFAULT and MAXLINEUTIL
boxplot(MAXLINEUTIL~DEFAULT, data=train, main="Maximum number of lines being utilized" 
        , sub="train", col="darkgreen", xlab="DEFAULT", ylab="MAXLINEUTIL")
boxplot(MAXLINEUTIL~DEFAULT, data=test, main="Maximum number of lines being utilized" 
        , sub="test", col="brown", xlab="DEFAULT", ylab="MAXLINEUTIL")

#DEFAULT and DAYSDELQ
boxplot(DAYSDELQ~DEFAULT, data=train, main="Number of delinquent days" 
        , sub="train", col="darkgreen", xlab="DEFAULT", ylab="DAYSDELQ")
boxplot(DAYSDELQ~DEFAULT, data=test, main="Number of delinquent days" 
        , sub="test", col="brown", xlab="DEFAULT", ylab="DAYSDELQ")

#DEFAULT and TOTACBAL
boxplot(TOTACBAL~DEFAULT, data=train, main="Total balance of business account" 
        , sub="train", col="darkgreen", xlab="DEFAULT", ylab="TOTACBAL")
boxplot(TOTACBAL~DEFAULT, data=test, main="Total balance of business account" 
        , sub="test", col="brown", xlab="DEFAULT", ylab="TOTACBAL")


#-------------------------------------------------------------------
# Z-test for two variable
#-------------------------------------------------------------------
#Z-test for two variables - DEFAULT and DAYSDELQ
train <- na.omit(train)
a <- subset(train$DAYSDELQ,train$DEFAULT=='Y')
b <- subset(train$DAYSDELQ,train$DEFAULT=='N')
n1 <- length(a)
n2 <- length(b)
z <- (mean(a) - mean(b)) / (sqrt(var(a)/n1 + var(b)/n2))
pz <- 1-(pnorm(abs(z))-0.5)*2
sprintf("DEFAULT and DAYSDELQ: z = %f, pz = %e\n",z,pz)

#Z-test for two variables - TOTACBAL and DEFAULT
train <- na.omit(train)
a <- subset(train$TOTACBAL,train$DEFAULT=='Y')
b <- subset(train$TOTACBAL,train$DEFAULT=='N')
n1 <- length(a)
n2 <- length(b)
z <- (mean(a) - mean(b)) / (sqrt(var(a)/n1 + var(b)/n2))
pz <- 1-(pnorm(abs(z))-0.5)*2
sprintf("TOTACBAL and DAYSDELQ: z = %f, pz = %e\n",z,pz)

#Z-test for two variables - BUSAGE and DEFAULT
train <- na.omit(train)
a <- subset(train$BUSAGE ,train$DEFAULT=='Y')
b <- subset(train$BUSAGE ,train$DEFAULT=='N')
n1 <- length(a)
n2 <- length(b)
z <- (mean(a) - mean(b)) / (sqrt(var(a)/n1 + var(b)/n2))
pz <- 1-(pnorm(abs(z))-0.5)*2
sprintf("BUSAGE and DAYSDELQ: z = %f, pz = %e\n",z,pz)

#Z-test for two variables - MAXLINEUTIL and DEFAULT
train <- na.omit(train)
a <- subset(train$MAXLINEUTIL ,train$DEFAULT=='Y')
b <- subset(train$MAXLINEUTIL ,train$DEFAULT=='N')
n1 <- length(a)
n2 <- length(b)
z <- (mean(a) - mean(b)) / (sqrt(var(a)/n1 + var(b)/n2))
pz <- 1-(pnorm(abs(z))-0.5)*2
sprintf("MAXLINEUTIL and DAYSDELQ: z = %f, pz = %e\n",z,pz)


#-------------------------------------------------------------------
# ANOVA
#-------------------------------------------------------------------
#ANOVA - BUSTYPE and TOTACBAL 
fit <- aov(train$TOTACBAL ~ train$BUSTYPE)
summary(fit)

#ANOVA - BUSTYPE and MAXLINEUTIL 
fit <- aov(train$MAXLINEUTIL ~ train$BUSTYPE)
summary(fit)

#ANOVA - BUSTYPE and DAYSDELQ 
fit <- aov(train$DAYSDELQ ~ train$BUSTYPE)
summary(fit)

#ANOVA - BUSTYPE and BUSAGE 
fit <- aov(train$BUSAGE ~ train$BUSTYPE)
summary(fit)
