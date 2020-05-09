# Import libraries
library(tidyverse)
library(chron)
library(geosphere)
library(lubridate)
library(DMwR)
library(ISLR)
library(dplyr)
library(Boruta)
library(caret)
library(randomForest)
library(nnet)
library(rpart)
library(rpart.plot)
library(gbm)
library(vtreat)
library(xgboost)

# Import data
citybike.train <- read.csv("C:/Users/.../Downloads/citybike.train.csv")
citybike.train$birth.year <- as.integer(citybike.train$birth.year)
# citybike.train <- subset(citybike.train, tripduration >=120 & tripduration <=3600 & birth.year>=(mean(citybike.train$birth.year)-(2*sd(citybike.train$birth.year))))#These data cleaning were ultimately removed as the actually increased the RMSE
citybike.train <- subset(citybike.train, tripduration >=120)

#The 10000A test data
citybike.test10000A <- read.csv("C:/Users/.../Downloads/citybike.test10000A.csv")
citybike.test10000Awithduration <- read.csv("C:/Users/.../Downloads/citybike.test10000Awithduration.csv")

#The 10000B test data
citybike.test10000B <- read.csv("C:/Users/.../Downloads/citybike.test10000B.csv")
#Creation of new columns
citybike.train$tripdistance <- distHaversine(cbind(citybike.train$start.station.longitude,citybike.train$start.station.latitude),cbind(citybike.train$end.station.longitude,citybike.train$end.station.latitude))
citybike.test10000A$tripdistance <- distHaversine(cbind(citybike.test10000A$start.station.longitude,citybike.test10000A$start.station.latitude),cbind(citybike.test10000A$end.station.longitude,citybike.test10000A$end.station.latitude))
citybike.test10000B$tripdistance <- distHaversine(cbind(citybike.test10000B$start.station.longitude,citybike.test10000B$start.station.latitude),cbind(citybike.test10000B$end.station.longitude,citybike.test10000B$end.station.latitude))

citybike.train$usertype <- ifelse(citybike.train$usertype == "Subscriber",1,2)
citybike.test10000A$usertype <- ifelse(citybike.test10000A$usertype == "Subscriber",1,2)
citybike.test10000B$usertype <- ifelse(citybike.test10000A$usertype == "Subscriber",1,2)

citybike.train$weekend <- as.numeric(is.weekend(substr(citybike.train$starttime,1,10)))
citybike.test10000A$weekend <- as.numeric(is.weekend(substr(citybike.test10000A$starttime,1,10)))
citybike.test10000B$weekend <- as.numeric(is.weekend(substr(citybike.test10000B$starttime,1,10)))
#citybike.train$tripdistance <- replace(citybike.train$tripdistance, citybike.train$tripdistance == 0, mean(citybike.train$tripdistance))

citybike.train$gender <- as.numeric(replace(citybike.train$gender, citybike.train$gender== 0, 3))
citybike.test10000A$gender <- as.numeric(replace(citybike.test10000A$gender, citybike.test10000A$gender== 0, 3))
citybike.test10000B$gender <- as.numeric(replace(citybike.test10000B$gender, citybike.test10000B$gender== 0, 3))

names(citybike.train)[names(citybike.train) == "starttime"] <- "startdate"
citybike.train$starttime = substr(citybike.train$startdate,1,nchar(citybike.train$startdate)-5)
citybike.train$hour    <- as.integer(hour(citybike.train$starttime))
citybike.train$rushhour <- ifelse(citybike.train$weekend == 0 & (citybike.train$hour>=7 & citybike.train$hour<=9 | citybike.train$hour>=17 & citybike.train$hour<=19),1,0)
citybike.train$startdate <- as.Date(substr(citybike.train$startdate,1,nchar(citybike.train$startdate)-14))

names(citybike.test10000A)[names(citybike.test10000A) == "starttime"] <- "startdate"
citybike.test10000A$starttime = substr(citybike.test10000A$startdate,1,nchar(citybike.test10000A$startdate)-5)
citybike.test10000A$hour    <- as.integer(hour(citybike.test10000A$starttime))
citybike.test10000A$rushhour <- ifelse(citybike.test10000A$weekend == 0 & (citybike.test10000A$hour>=7 & citybike.test10000A$hour<=9 | citybike.test10000A$hour>=17 & citybike.test10000A$hour<=19),1,0)
citybike.test10000A$startdate <- as.Date(substr(citybike.test10000A$startdate,1,nchar(citybike.test10000A$startdate)-14))

names(citybike.test10000B)[names(citybike.test10000B) == "starttime"] <- "startdate"
citybike.test10000B$starttime = substr(citybike.test10000B$startdate,1,nchar(citybike.test10000B$startdate)-5)
citybike.test10000B$hour    <- as.integer(hour(citybike.test10000B$starttime))
citybike.test10000B$rushhour <- ifelse(citybike.test10000B$weekend == 0 & (citybike.test10000B$hour>=7 & citybike.test10000B$hour<=9 | citybike.test10000B$hour>=17 & citybike.test10000B$hour<=19),1,0)
citybike.test10000B$startdate <- as.Date(substr(citybike.test10000B$startdate,1,nchar(citybike.test10000B$startdate)-14))

#Validation
# caret.control <- trainControl(method="repeatedcv", number=10, repeats=3) #10 fold validation
# city.cv <- caret::train(tripduration~tripduration~tripdistance+usertype.binary+gender, data = citybike.train, method="lm", trControl=caret.control, control=rpart.control(method="class"))
# pred <- predict(city.cv, citybike.test10000Awithduration)

#Weather Data: Ultimately removed
# names(citybike.train)[names(citybike.train) == "starttime"] <- "startdate"
# names(citybike.test10000A)[names(citybike.test10000A) == "starttime"] <- "startdate"
# names(citybike.test10000B)[names(citybike.test10000B) == "starttime"] <- "startdate"
# 
# citybike.train$startdate <- as.Date(substr(citybike.train$startdate,1,nchar(citybike.train$startdate)-14))
# citybike.test10000B$startdate <- as.Date(substr(citybike.test10000B$startdate,1,nchar(citybike.test10000B$startdate)-14))
# citybike.test10000A$startdate <- as.Date(substr(citybike.test10000A$startdate,1,nchar(citybike.test10000A$startdate)-14))

# weather <- read.csv("C:/Users/.../Downloads/weather.csv")
# weather <- subset(weather, select=c("DATE", "PRCP","TMAX", "TMIN"))
# weather$DATE <- as.Date(weather$DATE)
# weather$PRCP <- as.double(weather$PRCP)
# weather$TMAX <- as.integer(weather$TMAX)
# weather$TMIN <- as.integer(weather$TMIN)

#Join Data
# citybike.train <- weather %>% inner_join(citybike.train, by = c("DATE" = "startdate"))
# citybike.test10000A<- weather %>% inner_join(citybike.test10000A, by = c("DATE" = "startdate"))
# citybike.test10000B<- weather %>% inner_join(citybike.test10000B, by = c("DATE" = "startdate"))



#Determining which predictors to use
## The Boruta algorithm is time consuming so the code will be commented out and the results will be pasted below
### borutaOutput <- Boruta(tripduration ~ ., citybike.train)
### print(borutaOutput)
### plot(borutaOutput,  cex.axis=0.75, las=2, main="Boruta algorithm for Feature Selection", xlab="")
## Boruta performed 19 iterations in 16.48091 mins.
## 20 attributes confirmed important: age, birth.year, DATE, end.station.id, end.station.latitude and 15 more;
## 2 attributes confirmed unimportant: bikeid, holiday;

#Final predictors after excluding redundant or unimportant attributes and refining
#age
#gender
#usertype
#tripdistance
citybike.train <- select(citybike.train, tripduration, tripdistance, gender, usertype)

#10000A test data


# perf.rpart <- rpart(tripduration~tripdistance+usertype.binary+weekend+gender, data=citybike.train)
# pred <- predict(perf.rpart,citybike.test10000A)
# cat("RPART: ", "\n")
# cat("MSE: ", mean((pred-citybike.test10000Awithduration$tripduration)^2), "\n")
# cat("RMSE: ", sqrt(mean((pred-citybike.test10000Awithduration$tripduration)^2)), "\n")

#XGBoost

# variable names
# features <- setdiff(names(citybike.train), "tripduration")
# treatplan <- vtreat::designTreatmentsZ(citybike.train, features, verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
# new_vars <- treatplan %>%
#   magrittr::use_series(scoreFrame) %>%        
#   dplyr::filter(code %in% c("clean", "lev")) %>% 
#   magrittr::use_series(varName)   
# 
# features_train <- vtreat::prepare(treatplan, citybike.train, varRestriction = new_vars) %>% as.matrix()
# response_train <- citybike.train$tripduration
# 
# features_test <- vtreat::prepare(treatplan, citybike.test10000A, varRestriction = new_vars) %>% as.matrix()
# response_test <- citybike.test10000Awithduration$tripduration
# 
# dim(features_train)
# dim(features_test)

# create parameter list
# params <- list(
#   eta = .01,
#   max_depth = 10,
#   min_child_weight = 1,
#   subsample = 1,
#   colsample_bytree = 1
# )

# reproducibility
# set.seed(123)

# # train model
# xgb.fit <- xgb.cv(
#   params = params,
#   data = features_train,
#   label = response_train,
#   nrounds = 1000,
#   nfold = 5,
#   objective = "reg:linear",  # for regression models
#   verbose = 0,               # silent,
#   early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
# )
# 
# xgb.fit$evaluation_log %>%
#   dplyr::summarise(
#     ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
#     rmse.train   = min(train_rmse_mean),
#     ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
#     rmse.test   = min(test_rmse_mean),
#   )
# 
# xgb.fit.final <- xgboost(
#   params = params,
#   data = features_train,
#   label = response_train,
#   nrounds = 1,
#   objective = "reg:linear",
#   verbose = 0
# )
# pred.xgb <- predict(xgb.fit.final, features_test)
# caret::RMSE(pred.xgb, response_test)

#GBM
# hyper_grid <- expand.grid(
#   shrinkage = c(.01, .1, .3),
#   interaction.depth = c(1, 3, 5),
#   n.minobsinnode = c(5, 10, 15),
#   bag.fraction = c(.65, .8, 1), 
#   optimal_trees = 0,               # a place to dump results
#   min_RMSE = 0                     # a place to dump results
# )
# for(i in 1:nrow(hyper_grid)) {
#   
#   # reproducibility
#   set.seed(123)
#   
#   # train model
#   gbm.tune <- gbm(
#     formula = tripduration~ .,
#     distribution = "gaussian",
#     data = citybike.train,
#     n.trees = 5000,
#     interaction.depth = hyper_grid$interaction.depth[i],
#     shrinkage = hyper_grid$shrinkage[i],
#     n.minobsinnode = hyper_grid$n.minobsinnode[i],
#     bag.fraction = hyper_grid$bag.fraction[i],
#     train.fraction = .75,
#     n.cores = NULL, # will use all cores by default
#     verbose = FALSE
#   )
#   
#   # add min training error and trees to grid
#   hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
#   hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
# }
# 
# hyper_grid %>% 
#   dplyr::arrange(min_RMSE) %>%
#   head(10)
# 1       0.30                 5              5         0.80            48 977.1549
# 2       0.30                 3             10         0.80            74 977.4678
# 3       0.30                 3              5         0.65            52 978.2146
# 4       0.10                 3             10         1.00           350 978.3478
# 5       0.30                 5             10         0.80            82 978.3492
# 6       0.01                 5             10         1.00          3837 978.4120
# 7       0.01                 3             10         1.00          4468 978.5349
# 8       0.30                 3             15         1.00            68 978.5892
# 9       0.30                 3             15         0.80           111 978.7884
# 10      0.01                 3             10         0.80          4477 978.8058

set.seed(123)
gbm.fit <- gbm(
  formula = tripduration~ .,
  distribution = "gaussian",
  data = citybike.train,
  n.trees = 48,
  interaction.depth = 5,
  shrinkage = 0.30,
  n.minobsinnode = 5,
  bag.fraction = .80,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)
print(gbm.fit)

boost.predict <- predict(gbm.fit,n.trees = gbm.fit$n.trees, citybike.test10000A)
cat("GBM: ", "\n")
cat("MSE: ", mean((boost.predict-citybike.test10000Awithduration$tripduration)^2), "\n")
cat("RMSE: ", sqrt(mean((boost.predict-citybike.test10000Awithduration$tripduration)^2)), "\n")
# 13274.39 

#Random Forest
# rf.model <- randomForest(tripduration~ tripdistance+usertype.binary+gender, data = citybike.train)
# rf.predict <- predict(rf.model, citybike.test10000A)
# cat("Random Forest: ", "\n")
# cat("MSE: ", mean((rf.predict-citybike.test10000Awithduration$tripduration)^2), "\n")
# cat("RMSE: ", sqrt(mean((rf.predict-citybike.test10000Awithduration$tripduration)^2)), "\n")
#13275.04

#GLM
perf.glm <- glm(tripduration~ tripdistance+usertype+gender, data=citybike.train, family = gaussian)
pred <- predict(perf.glm,citybike.test10000A)
DMwR::regr.eval(pred,citybike.test10000Awithduration$tripduration)
cat("GLM: ", "\n")
cat("MSE: ", mean((pred-citybike.test10000Awithduration$tripduration)^2), "\n")
cat("RMSE: ", sqrt(mean((pred-citybike.test10000Awithduration$tripduration)^2)), "\n")
#RMSE:  13270.04 


#SVM
# perf.svm <- svm(tripduration~tripdistance+usertype.binary+weekend+gender, data=citybike.train)
# pred <- predict(perf.svm,citybike.test10000A)
# cat("SVM: ", "\n")
# cat("MSE: ", mean((pred-citybike.test10000Awithduration$tripduration)^2), "\n")
# cat("RMSE: ", sqrt(mean((pred-citybike.test10000Awithduration$tripduration)^2)), "\n")

#NNET
# perf.nnet <- nnet((tripduration/range(citybike.train$tripduration))~tripdistance+usertype.binary+weekend+gender, data=citybike.train, size = 6)
# pred <- predict(perf.nnet,citybike.test10000A)*range(citybike.train$tripduration)
# cat("NNET: ", "\n")
# cat("MSE: ", mean((pred-citybike.test10000Awithduration$tripduration)^2), "\n")
# cat("RMSE: ", sqrt(mean((pred-citybike.test10000Awithduration$tripduration)^2)), "\n")

################################################################################
# Smallest RMSE for the 10000A test data was from the GLM Prediction           #                                              
# RMSE: 13270.04                                                               #
# mae          mse         rmse         mape                                   #
# 7.340039e+02 1.760939e+08 1.327004e+04 6.999830e-01                          #                                                                           #
################################################################################

#10000B test data prediction using glm and multiple predictors
perf.glm <- glm(tripduration~ tripdistance+usertype+gender, data=citybike.train, family = gaussian)
pred10000B <- predict(perf.glm,citybike.test10000B)
write.csv(pred10000B, row.names = FALSE, file="C:/Users/.../Downloads/citybike.test10000BPrediction7.csv")

#ADDITIONAL COMMENTS:
#External weather and holiday data was added to the dataset but they increased the RMSE so they along with other "noisy" variables were removed