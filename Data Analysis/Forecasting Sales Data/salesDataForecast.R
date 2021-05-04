# Install Libraries
install.packages("gbm")
install.packages("tictoc")
install.packages("randomForest")
install.packages("scales")
install.packages("skimr")

# Import Libraries
library(skimr)
library(tidyr)
library(scales)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(e1071)
library(gbm)
library(data.table)
library(tictoc)

# list.files(path = "../input") # Helpful for finding relevant data files
# Load in the data
tic("Data Loading ")
sales_data = fread("../input/sales_train.csv")
item_data = fread("../input/items.csv")
test_data = fread("../input/test.csv")
toc()

glimpse(sales_data)
glimpse(item_data)
glimpse(test_data)
skim(sales_data)

#### Data Preparation  #####

sales_data = merge(sales_data, item_data[,c("item_id", "item_category_id")], by = "item_id", all.x = T)
sales_data$date = as.Date(sales_data$date, "%d.%m.%Y")

sales_data$year = year(sales_data$date)
sales_data$year =  as.factor(sales_data$year)

sales_data$month = month(sales_data$date)
sales_data$month = as.factor(sales_data$month)

sales_data$day = day(sales_data$date)
sales_data$day = as.factor(sales_data$day)

sales_data$weekdays =  weekdays(sales_data$date)
sales_data$weekdays = as.factor(sales_data$weekdays)

sales_data$item_category_id =  as.factor(sales_data$item_category_id)

# Remove Outliers and Invalid Data (negative or NA)
sales_data = sales_data[sales_data$item_price<= 100000,]
sales_data = sales_data[sales_data$item_cnt_day<= 1000,]

sales_data <- sales_data %>%
  mutate( item_cnt_day = ifelse(item_cnt_day < 0, 0, item_cnt_day),
          item_price = ifelse(is.na(item_price), 0, item_price))


### Data Exploration ####
yearly_sales = sales_data %>%
  group_by(year) %>%
  summarise(yearly_sale = sum(item_price * item_cnt_day))

ggplot(yearly_sales, aes(x =  year, y = yearly_sale, fill =  factor(year))) +
  geom_histogram(stat = "identity") +
  labs(title = "Yearly Revenue",
       x = "Year",
       y = "Total Revenue",
       fill = "Year") +
  geom_label(
    stat = "identity",
    position = position_dodge(width = 1),
    hjust = "center",
    aes(label = yearly_sale)
  ) +
  theme(
    # Remove panel grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
    # Change plot and panel background
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend
    # legend.position = "none",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # Align title to top center
    plot.title = element_text(
      color = "white",
      hjust = 0.5,
      face = "bold"
    ),
    # Change axis ticks to bold black
    axis.text = element_text(colour = "yellow", face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) + scale_y_continuous(labels = comma)

weekdays_sales = sales_data %>%
  group_by(weekdays) %>%
  summarise(total_sale = sum(item_cnt_day * item_price)) %>%
  arrange(desc(total_sale))
weekdays_sales$total_sale = round(weekdays_sales$total_sale, 2)

ggplot(weekdays_sales, aes(
  x = reorder(weekdays, total_sale),
  y =  total_sale,
  fill = factor(weekdays)
)) +
  geom_bar(stat = "identity") +
  labs(title = "Revenue Per Day of the Week",
       x = "Day of the Week",
       y =  "Revenue",
       fill = "Day of the Week") +
  coord_flip() +
  geom_label(
    stat = "identity",
    position = position_dodge(width = 1),
    hjust = "center",
    aes(label = total_sale)
  ) +
  theme(
    # Remove panel grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
    # Change plot and panel background
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend
    # legend.position = "none",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # Align title to top center
    plot.title = element_text(
      color = "white",
      hjust = 0.5,
      face = "bold"
    ),
    # Change axis ticks to bold black
    axis.text = element_text(colour = "yellow", face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) + scale_y_continuous(labels = comma)

sales_shopwise = sales_data %>%
  select(shop_id, item_cnt_day) %>%
  group_by(shop_id) %>%
  summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))

ggplot(data =  sales_shopwise,
       mapping = aes(
         x = shop_id,
         y = item_cnt_day,
         fill = factor(shop_id)
       )) +
  geom_bar(stat = "identity") +
  xlab("Shop ID") + ylab("Sales Count") +
  ggtitle(label = "Number of Sales Per Shop") +
  theme(
    # Remove panel grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
    # Change plot and panel background
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend
    # legend.position = "none",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # Align title to top center
    plot.title = element_text(
      color = "white",
      hjust = 0.5,
      face = "bold"
    ),
    # Change axis ticks to bold black
    axis.text = element_text(colour = "yellow", face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
  ) + scale_y_continuous(labels = comma) + scale_fill_discrete(name = "Shop ID")

popularity  =  sales_data %>%
  group_by(shop_id, item_id) %>%
  summarise(sold_item_count = sum(item_cnt_day)) %>%
  arrange(desc(sold_item_count))

popular_items_in_shop  =  sales_data %>%
  group_by(shop_id, item_id) %>%
  summarise(sold_item_count = sum(item_cnt_day)) %>%
  filter(sold_item_count == max(sold_item_count)) %>%
  arrange(desc(sold_item_count))

ggplot(data = popular_items_in_shop,
       mapping = aes(
         x = shop_id,
         y = sold_item_count,
         fill = factor(item_id)
       )) +
  geom_bar(stat = "identity") +
  xlab("Shop ID") + ylab("Sales Count") +
  ggtitle("Most Popular Item Per Shop") +
  coord_flip() +
  theme(
    # Remove panel grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
    # Change plot and panel background
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend
    # legend.position = "none",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # Align title to top center
    plot.title = element_text(
      color = "white",
      hjust = 0.5,
      face = "bold"
    ),
    # Change axis ticks to bold black
    axis.text = element_text(colour = "yellow", face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) + scale_y_continuous(labels = comma) + scale_fill_discrete(name = "Item ID")

daily_sale = sales_data %>%
  group_by(date) %>%
  summarise(items_sold =  sum(item_cnt_day))

ggplot(daily_sale, aes(x =  date, y = items_sold, color =  items_sold)) +
  # geom_line() +
  geom_point() +
  labs(title = "Daily Item Sales from January 2013 to October 2015", x =  "Date", y = "Number of Items Sold") +
  theme(
    # Remove panel grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
    # Change plot and panel background
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend
    # legend.position = "none",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # Align title to top center
    plot.title = element_text(
      color = "white",
      hjust = 0.5,
      face = "bold"
    ),
    # Change axis ticks to bold black
    axis.text = element_text(colour = "yellow", face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) + scale_fill_discrete(name = "Number of Items Sold")

ym_sales = sales_data %>%
  group_by(year, month) %>%
  summarise(ym_sale = sum(item_price * item_cnt_day)) %>%
  arrange(year)

ym_sales$ym_sale = round(ym_sales$ym_sale, 2)
ggplot(ym_sales, aes(x =  month, y = ym_sale, fill =  factor(year))) +
  geom_histogram(stat = "identity", color = "yellow") +
  labs(title = "Yearly-Monthly Sales",
       x = "Month",
       y =  "Total Revenue",
       fill = "Year") +
  geom_label(
    stat = "identity",
    position = position_stack(),
    hjust = "center",
    vjust = 0.5,
    aes(label = ym_sale)
  ) +
  theme(
    # Remove panel grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
    # Change plot and panel background
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = 'black'),
    # Change legend
    # legend.position = "none",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    # Align title to top center
    plot.title = element_text(
      color = "white",
      hjust = 0.5,
      face = "bold"
    ),
    # Change axis ticks to bold black
    axis.text = element_text(colour = "yellow", face = "bold"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white")
  ) + scale_y_continuous(labels = comma)


### Linear Regression Model ###
tic("Time Taken to Run Linear Regression Model ")
linear_model = lm(formula = item_cnt_day ~ shop_id + item_id, data = sales_data)
toc()

summary(linear_model)


# GBM Model
tic("Time Taken to Run GBM Model ")
gbm_model  =  gbm(
  formula = item_cnt_day ~ shop_id + item_id,
  data = sales_data,
  shrinkage = 0.01,
  distribution = "gaussian",
  n.trees = 1000,
  interaction.depth = 5,
  bag.fraction = 0.5,
  train.fraction = 0.8,
  cv.folds = 2,
  n.cores = 1,
  verbose = T
)
toc()

#RMSE - LM
RSS <- c(crossprod(linear_model$residuals)) # Residual Sum of Squares
MSE <- RSS / length(linear_model$residuals) # Mean Squared Error
RMSE_lm <- sqrt(MSE)

#RMSE - GBM
best <- which.min(gbm_model$cv.error)
RMSE_gbm <- sqrt(gbm_model$cv.error[best])

RMSE_lm
RMSE_gbm



result_gbm = predict(gbm_model, test_data[, c("shop_id", "item_id")], n.trees = 1000)

sub = data.frame(ID = test_data$ID, item_cnt_month =  result_gbm)
write.csv(sub, "submission.csv", row.names = F)