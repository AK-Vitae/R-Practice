library(tidyverse)
library(reshape2)
library(hrbrthemes)
library(timeSeries)
library(caret)
library(forecast)
library(lubridate)
library(randomForest)
library(e1071)
library(gbm)
library(DMwR)
theme_set(theme_economist())
set.seed(123)

#Data obtained from https://www.kaggle.com/c/covid19-global-forecasting-week-4/data and externally refined with information from https://github.com/CSSEGISandData/COVID-19 JHU/CCSE
k.train <- read.csv("C:/Users/.../Downloads/train.csv")#Training Data
head(k.train)
tail(k.train)

k.test <- read.csv("C:/Users/.../Downloads/test.csv")#Testing Data

#Making Data Usable
k.train$Province_State <- as.character(k.train$Province_State)
k.train$Country_Region <- as.character(k.train$Country_Region)

k.test$Province_State <- as.character(k.test$Province_State)
k.test$Country_Region <- as.character(k.test$Country_Region)

k.train$Date <- as.Date(k.train$Date, format = "%Y-%m-%d")
k.test$Date <- as.Date(k.test$Date, format = "%Y-%m-%d")

k.train$Province_State <- ifelse(k.train$Province_State == "", k.train$Country_Region, k.train$Province_State)
k.test$Province_State <- ifelse(k.test$Province_State == "", k.test$Country_Region, k.test$Province_State)

#Show any missing values
colSums(is.na(k.train)) 
colSums(is.na(k.test)) 
sum(is.na(k.train))

x <- head(k.train)
y <- tail(k.train)

x <- head(k.test)
y <- tail(k.test)


#Visualization
##Global
covid.total <- k.train %>% 
  group_by(Date) %>%
  summarise(ConfirmedCases = sum(ConfirmedCases), Fatalities = sum(Fatalities))


ggplot(covid.total, aes(Date)) + 
  geom_line(aes(y = ConfirmedCases/1000000, color = "Confirmed Cases"), size = 2) + 
  geom_line(aes(y = Fatalities/1000000, color = "Fatalities"), size = 2) + 
  labs(title="Global COVID-19 Confirmed & Fatalities Cases by Date",
       subtitle="Since 01/22/2020",
       x = "Date", y = "Count Per Million") +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.position = "right")

##US
covid.us <- k.train %>%
  dplyr::filter(Country_Region == "US") %>%
  dplyr::filter(Date >'2020-02-01') %>%
  group_by(Date, Province_State)

###Top 10 American States by Fatalities
k.train.temp <- k.train %>%
  arrange(Date) %>%
  mutate(daily_deaths = Fatalities - lag(Fatalities, default = first(Fatalities), order_by = Date)) %>%
  mutate(daily_cases = ConfirmedCases - lag(ConfirmedCases, default = first(ConfirmedCases), order_by = Date))

total.state <- k.train.temp %>% dplyr::filter(Country_Region=="US") %>%
  group_by(Province_State,Date) %>%
  dplyr::filter(Date >'2020-02-01') %>%
  summarize(totalConfirmed = sum(ConfirmedCases), totalDeaths=sum(Fatalities))

latestDate <- max(k.train$Date)
top.state <- total.state %>%
  dplyr::filter(Date==latestDate) %>%
  arrange(-totalDeaths)

top.10.state <- top.state[1:10,]

ggplot(top.10.state, aes(Province_State)) + 
  geom_bar(stat="identity",aes(x = reorder(Province_State,-totalDeaths), y = totalDeaths,fill=Province_State)) +
  labs(title = "Top 10 US States by COVID-19 Fatalities",
       subtitle = "Since 02/01/2020",
       y="Fatality count", x= "State") +
  theme(axis.text.x = element_text(angle = 90),legend.position = "none") +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  theme(plot.title = element_text(hjust=0.5))

###US Mid-Atlantic
covid.midatlantic <- covid.us%>% dplyr::filter(Province_State== "Delaware"|Province_State=="New York" |Province_State=="District of Columbia" |Province_State=="New Jersey"| 
                                                 Province_State=="Maryland"|Province_State=="Virginia"|Province_State=="West Virginia") %>% 
  dplyr::filter(Date >="2020-02-01") %>% 
  group_by(Date,Province_State) %>%
  summarize(Confirmed = sum(ConfirmedCases), Fatalities=sum(Fatalities))

ggplot(covid.midatlantic, aes(x=Date, y=Confirmed,color=Province_State)) + 
  geom_smooth(method = "loess", se=FALSE) + 
  labs(y = "Confirmed Positive",title="Confirmed Cases in US Mid-Atlantic",subtitle = "Since 02/01/2020")+
  theme(plot.subtitle = element_text(hjust=0.5)) +
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.position = "right")

ggplot(covid.midatlantic, aes(x=Date, y=Fatalities,color=Province_State)) + 
  geom_smooth(method = "loess", se=FALSE) + 
  labs(y = "Total Deaths",title="Total Deaths in US Mid-Atlantic",subtitle = "Since 02/01/2020")+
  theme(plot.subtitle = element_text(hjust=0.5)) +
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.position = "right")

###NJ
nj <-  k.train %>% dplyr::filter(Country_Region == "US" & Province_State=="New Jersey") %>% 
  dplyr::filter(Date >="2020-02-01") %>%
  dplyr::mutate(nDays = as.double(as.Date(Date) - min(as.Date(Date)))) 

nj_new <- nj %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(new_deaths = Fatalities - dplyr::lag(Fatalities, default = first(Fatalities), order_by = Date)) %>%
  dplyr::mutate(new_cases = ConfirmedCases - dplyr::lag(ConfirmedCases, default = first(ConfirmedCases), order_by = Date))

ggplot(nj_new,aes(x=Date, y=new_cases)) + 
  geom_smooth(method = "loess", se=FALSE) + 
  geom_point(color="tomato") + 
  labs(y = "New Cases",
       subtitle="Since 02/01/2020",
       title = "New Cases in New Jersey") +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  theme(plot.title = element_text(hjust=0.5))

ggplot(nj_new, aes(x=Date, y=new_deaths)) + 
  geom_smooth(method = "loess", se=FALSE) + 
  geom_point(color="tomato") + 
  labs(y = "New Deaths",
       subtitle="Since 02/01/2020", 
       title = "New Deaths in New Jersey") +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  theme(plot.title = element_text(hjust=0.5))




#US Box Office
##Data from https://www.boxofficemojo.com/month/by-year/?grossesOption=calendarGrosses
df <- data.frame(Month = c("01 - January","02 - February", "03 - March"), "2019" = c(812849718,624462229,962715490), "2020" = c(897742569,638097350,253333772))
df
data.m <- melt(df, id.vars='Month')
data.m
ggplot(data = data.m) +
  geom_bar(mapping = aes(x = Month, y = value/1000000, fill = variable), stat="identity", position = "dodge", width = 0.7) +
  scale_fill_manual("Year\n", values = c("tomato","skyblue"),
                    labels = c(" 2019", " 2020")) +
  labs(x="\nMonth",y="Cumulative Domestic Revenue in Millions($)\n") +
  theme_bw(base_size = 14) +
  ggtitle("Cumulative Domestic Box Office Revenue of the first 3 months of 2019 vs 2020")+
  theme(plot.title = element_text(hjust=0.5))

#Of note the Cumulative Domestic Revenue for April 2019 was $1,034,981,639 while in 2020 it was only $52,015
#Can be further refined as the year progresses

#Dining out in the US
##Data from https://www.opentable.com/state-of-industry
opentable <- read.csv("C:/Users/.../Downloads/opentable.csv")
opentable$Date <- as.Date(opentable$Date)
opentable$Year_Over_Year <- as.numeric(opentable$Year_Over_Year/100)

ggplot(opentable, aes(x=Date, y=Year_Over_Year)) +
  geom_line( color="steelblue") +
  xlab("") +
  theme_ipsum() +
  labs(x="\nDate",y="Year Over Year Change\n") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2020-02-19"),as.Date("2020-05-02"))) +
  ylim(-100,100)+
  ggtitle("Year Over Year Seated Diners at US Restaurants (2019 vs 2020)") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.y = element_text(angle=90, hjust = 0.5))+
  theme(axis.title.x = element_text(angle=0, hjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))

#Predicting using models
##NY Data
ny <-  k.train %>% dplyr::filter(Country_Region == "US" & Province_State=="New York") %>% 
  dplyr::filter(Date >="2020-03-05")

ny.test <- k.test %>% dplyr::filter(Country_Region == "US" & Province_State=="New York")
##GLM
glm <- glm(ConfirmedCases~ Date, data=ny)
summary(glm)
pred.glm <- predict(glm,ny.test)
pred.glm <- as.data.frame(pred.glm)

##SVM
svm.model <- svm(ConfirmedCases ~ Date, data = ny)
summary(svm.model)
pred.svm <- predict(svm.model, newdata = ny.test)
pred.svm <- as.data.frame((pred.svm))

##Random Forest
rf.model <- randomForest(ConfirmedCases~ Date, data = ny)
summary(rf.model)
rf.predict <- predict(rf.model, ny.test)
rf.predict <- as.data.frame((rf.predict))

#External data obtained from https://covidtracking.com/data/state/new-york to test value of prediction
nycConfirmedCases <- read.csv("C:/Users/.../Downloads/nycConfirmedCases.csv")
pred.glm.error <- head(pred.glm,-10)
pred.svm.error <- head(pred.svm,-10)
rf.predict.error <- head(rf.predict,-10)

print(paste("GLM: "))
DMwR::regr.eval(pred.glm.error,nycConfirmedCases$ConfirmedCases)
print(paste("SVM: "))
DMwR::regr.eval(pred.svm.error,nycConfirmedCases$ConfirmedCases)
print(paste("RF: "))
DMwR::regr.eval(rf.predict.error,nycConfirmedCases$ConfirmedCases)

#Remove uncessary columns
ny <- dplyr::select(ny, ConfirmedCases,Date)
ny$Date <- as.numeric(ny$Date)

#GBM
# hyper_grid <- expand.grid(
#   shrinkage = c(.01, .1, .3),
#   interaction.depth = c(1, 3, 5),
#   n.minobsinnode = c(1, 5, 10),
#   bag.fraction = c(.65, .8, 1),
#   optimal_trees = 0,               # a place to dump results
#   min_RMSE = 0                     # a place to dump results
# )
# for(i in 1:nrow(hyper_grid)) {
# 
#   # train model
#   gbm.tune <- gbm(
#     formula = ConfirmedCases~ .,
#     distribution = "gaussian",
#     data = ny,
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
# hyper_grid %>%
#           dplyr::arrange(min_RMSE) %>%
#           head(10)

# shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
# 1        0.3                 1              1         0.65           285 46997.20
# 2        0.3                 5              1         0.65            77 47445.67
# 3        0.3                 3              1         0.65           171 47471.25
# 4        0.3                 3              1         0.80           933 47482.13
# 5        0.1                 5              1         0.65          1911 47482.13
# 6        0.3                 5              1         0.80           933 47482.13
# 7        0.3                 3              1         1.00          1468 47482.13
# 8        0.3                 5              1         1.00           728 47482.13
# 9        0.1                 5              1         0.80          2884 47482.13
# 10       0.1                 3              1         1.00          3820 47482.13

gbm.fit <- gbm(
  formula = ConfirmedCases~ .,
  distribution = "gaussian",
  data = ny,
  n.trees = 285,
  interaction.depth = 1,
  shrinkage = 1,
  n.minobsinnode = 1,
  bag.fraction = .65,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)
print(gbm.fit)
boost.predict <- predict(gbm.fit,n.trees = gbm.fit$n.trees, ny)
boost.predict <- as.data.frame(boost.predict)
boost.predict.error <- head(boost.predict,-10)
print(paste("GBM: "))
DMwR::regr.eval(boost.predict.error,nycConfirmedCases$ConfirmedCases)

#Using the RandomForest Model to Predict Global Covid Cases for the next 2 months
# covid.total <- k.train %>% 
#         group_by(Date) %>%
#         summarise(ConfirmedCases = sum(ConfirmedCases), Fatalities = sum(Fatalities))
# 
# future.date <- read.csv("C:/Users/.../Downloads/future.date.csv", sep="")#Dates from 05/01/2020-07/01/2020
# names(future.date)[names(future.date) == "ï..Date"] <- "Date"
# future.date$Date <- as.Date(as.character(future.date$Date),format="%Y-%m-%d")
# future.date <- future.date %>%
#         dplyr::filter(Date <='2020-04-31')
#         
# 
# rf.model <- randomForest(ConfirmedCases~ Date, data = covid.total)
# predict.global <- predict(rf.model, future.date)
# predict.global <- as.data.frame((predict.global))
# head(predict.global)
# tail(predict.global)
